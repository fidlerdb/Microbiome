###############################################################################
# TURN WRAPPER -- interactive CLI play-testing harness
###############################################################################
#
# Depends on the engine files (00-04, 90, 99) and turn_plots.R already being
# sourced, plus the patchwork package for the combined display below.
#
###############################################################################

library(patchwork)

# ---------------------------------------------------------------------------
# Move validation helpers
# ---------------------------------------------------------------------------

# Run a game action; return list(ok, game, msg). Errors AND warnings count as
# illegal moves -- a warning from inside the engine (e.g. duplication_phase()
# warning that a request couldn't be fully afforded) is escalated to an error
# so it's treated the same as an explicit stop().
try_move <- function(expr_fn) {
  tryCatch(
    list(ok = TRUE, game = suppressWarnings(withCallingHandlers(
      expr_fn(),
      warning = function(w) stop(conditionMessage(w), call. = FALSE)
    )), msg = NULL),
    error = function(e) list(ok = FALSE, game = NULL, msg = conditionMessage(e))
  )
}

# Prompt until the answer passes `check`. `check` returns NULL if OK, else a message.
ask_valid <- function(prompt, check, allow_blank = FALSE) {
  if (!interactive()) stop("play_game() needs an interactive R session.")
  repeat {
    ans <- trimws(readline(prompt))
    if (allow_blank && ans == "") return("")
    msg <- check(ans)
    if (is.null(msg)) return(ans)
    cat("  x", msg, "\n")
  }
}

check_tile <- function(valid_ids) function(x) {
  if (!grepl("^[0-9]+$", x) || !(as.integer(x) %in% valid_ids))
    paste0("Choose a tile ID between ", min(valid_ids), " and ", max(valid_ids), ".")
}

check_guild <- function(guilds) function(x) {
  if (!(x %in% guilds)) paste("Choose one of:", paste(guilds, collapse = ", "))
}

check_times <- function(x) {
  if (!grepl("^[0-9]+$", x) || as.integer(x) < 1) "Enter a whole number of 1 or more."
}

# ---------------------------------------------------------------------------
# Combined display: resources on top, board below (patchwork)
#
# Every plot shown by the wrapper goes through this one function, so the
# layout only needs to be decided in one place.
# ---------------------------------------------------------------------------

show_turn_plots <- function(game) {
  print(
    (plot_resources(game) / plot_microbiome(game)
     ) + #| guide_area() +
      plot_layout(
        guides = 'collect'
        , heights = c(0.3, 1)
                  #, design = 
                  #    '
                  #    11111111113
                  #    22222222223
                  #    22222222223
                  #    22222222223
                  #    ')
        )
  )
}

# ---------------------------------------------------------------------------
# Phase prompts
# ---------------------------------------------------------------------------

prompt_add_microbes <- function(game, player) {
  guilds <- game$microbes$Guild
  
  cat("\n--- Player", player, ": add a microbe ---\n")
  
  cat("\nGuild traits:\n")
  print(game$microbes, row.names = FALSE)
  
  cat("\nTiles (SOM refills to SOM_generation each turn):\n")
  print(game$tiles[, intersect(c("ID", "SOM_generation", "stress"), names(game$tiles))],
        row.names = FALSE)
  
  repeat {
    tile  <- as.integer(ask_valid("\nWhich tile will you put a microbe on? ",
                                  check_tile(game$tiles$ID)))
    guild <- ask_valid(paste0("Choose a guild (", paste(guilds, collapse = " / "), "): "),
                       check_guild(guilds))
    res <- try_move(function() add_microbes(game, tile = tile, player = player, guild = guild))
    if (res$ok) return(res$game)
    cat("  x Illegal move:", res$msg, "\n  Try again.\n")
  }
}

# ---------------------------------------------------------------------------
# Duplication (one player at a time; each accepted event is applied immediately)
# ---------------------------------------------------------------------------

show_duplication_state <- function(game, p) {
  pools <- game$resource_pools[game$resource_pools$player == p, , drop = FALSE]
  m <- match(pools$guild, game$microbes$Guild)
  pools$cost  <- game$microbes$Duplication_cost[m]
  pools$yield <- game$microbes$Yield[m]
  cat("\nYour SOM pools (cost and yield are per duplication event):\n")
  print(pools[, c("guild", "SOM", "cost", "yield")], row.names = FALSE)
  
  mine <- game$populations[game$populations$player == p & game$populations$n > 0, ,
                           drop = FALSE]
  cat("\nYour populations:\n")
  print(mine[, c("tile", "guild", "n")], row.names = FALSE)
  invisible(pools)
}

prompt_duplication <- function(game, players) {
  guilds <- game$microbes$Guild
  
  for (p in players) {
    cat("\n--- Player", p, ": duplication ---\n")
    
    repeat {
      pools <- show_duplication_state(game, p)
      
      if (!any(pools$SOM >= pools$cost, na.rm = TRUE)) {
        cat("\nYou cannot afford any duplication events.\n")
        break
      }
      
      tile <- ask_valid("\nTile to duplicate on (blank to finish): ",
                        check_tile(game$tiles$ID), allow_blank = TRUE)
      if (tile == "") break
      guild <- ask_valid("Guild to duplicate: ", check_guild(guilds))
      times <- as.integer(ask_valid("How many times? ", check_times))
      
      move <- data.frame(player = p, tile = as.integer(tile),
                         guild = guild, times = times)
      res <- try_move(function() duplication_phase(game, move))
      
      if (res$ok) {
        game <- res$game
        cat("  OK: duplication applied.\n")
      } else {
        cat("  x", res$msg, "\n")   # e.g. can't afford it, or no live population there
      }
    }
  }
  game
}

# ---------------------------------------------------------------------------
# Scoring
# ---------------------------------------------------------------------------

count_objectives <- function(result, players) {
  objectives <- result[setdiff(names(result), "turn")]
  vapply(players, function(p) {
    sum(vapply(objectives, function(d) isTRUE(d$met[d$player == p]), logical(1)))
  }, numeric(1))
}

report_objectives <- function(result, players) {
  objectives <- result[setdiff(names(result), "turn")]
  tab <- sapply(objectives, function(d) d$met[match(players, d$player)])
  rownames(tab) <- paste("Player", players)
  cat("\nObjectives met:\n")
  print(ifelse(tab, "YES", "-"), quote = FALSE)
}

# ---------------------------------------------------------------------------
# The game loop
# ---------------------------------------------------------------------------

play_game <- function(game,
                      players = 1:2,
                      max_turns = 20,
                      win_objectives = 3) {
  
  cat("Guild traits:\n")
  print(game$microbes, row.names = FALSE)
  
  repeat {
    game <- start_turn(game)
    cat("\n==================== TURN", game$turn, "====================\n")
    show_turn_plots(game)
    
    # 1. Add microbes
    for (p in players) game <- prompt_add_microbes(game, p)
    
    # 2. Resource competition
    game <- resource_phase(game)
    cat("\nResources have been competed over.\n")
    show_turn_plots(game)
    
    # 3. Duplication
    game <- prompt_duplication(game, players)
    
    # 4. Bookkeeping and display
    game <- record_abundance_census(game)
    show_turn_plots(game)
    
    # 5. Game end check
    result <- game_end(game)
    score  <- count_objectives(result, players)
    report_objectives(result, players)
    
    if (any(score >= win_objectives) || game$turn >= max_turns) break
    invisible(readline("\nPress Enter for the next turn..."))
  }
  
  winners <- players[score == max(score)]
  cat("\n==================== GAME OVER ====================\n")
  if (length(winners) == 1) {
    cat("Player", winners, "wins with", max(score), "objectives met.\n")
  } else {
    cat("Draw between players", paste(winners, collapse = " & "),
        "with", max(score), "objectives each.\n")
  }
  invisible(list(game = game, result = result, score = score))
}