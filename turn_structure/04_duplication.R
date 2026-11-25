###############################################################################
# (4) DUPLICATION #############################################################
###############################################################################

# ------------------------------------------------------------------------- #
# pop_increase: one row per player decision
#
#   player = player making the decision
#   tile   = where duplication occurs
#   guild  = guild being duplicated
#   times  = number of duplication events requested
#
# Each duplication event costs `Duplication_cost` SOM (from that player's
# guild-specific resource pool) and produces `Yield` new microbes.
#
# ILLEGAL MOVES: if a requested `times` would cost more SOM than the
# player's pool currently holds, the whole call is rejected via stop().
# Nothing is partially applied -- see header note on copy-on-modify safety.
# ------------------------------------------------------------------------- #
duplication_phase <- function(game, pop_increase) {
  
  required_cols <- c("player", "tile", "guild", "times")
  missing_cols  <- setdiff(required_cols, names(pop_increase))
  
  if (length(missing_cols) > 0) {
    stop("pop_increase is missing: ", paste(missing_cols, collapse = ", "))
  }
  
  if (nrow(pop_increase) == 0) {
    return(game)
  }
  
  demographic_rows <- vector("list", nrow(pop_increase))
  
  for (i in seq_len(nrow(pop_increase))) {
    
    player <- pop_increase$player[i]
    tile   <- pop_increase$tile[i]
    guild  <- pop_increase$guild[i]
    times  <- pop_increase$times[i]
    
    if (times != round(times)) {
      stop("`times` must be a whole number (got ", times, ").")
    }
    
    if (times <= 0) {
      next
    }
    
    # -------------------------------------------------------------------------
    # Duplication is only possible where this player already has a live
    # population of this guild on this tile.
    # -------------------------------------------------------------------------
    
    population_index <- which(
      game$populations$tile   == tile &
        game$populations$player == player &
        game$populations$guild  == guild
    )
    
    if (length(population_index) != 1) {
      stop(
        "No unique ", guild, " population for Player ", player,
        " on tile ", tile, "."
      )
    }
    
    if (game$populations$n[population_index] <= 0) {
      stop(
        "Illegal move: Player ", player, " has no ", guild,
        " cells on tile ", tile, " to duplicate."
      )
    }
    
    # -------------------------------------------------------------------------
    # Guild traits
    # -------------------------------------------------------------------------
    
    microbe_index <- match(guild, game$microbes$Guild)
    
    if (is.na(microbe_index)) {
      stop("Guild '", guild, "' is not present in game$microbes.")
    }
    
    yield             <- game$microbes$Yield[microbe_index]
    duplication_cost  <- game$microbes$Duplication_cost[microbe_index]
    
    # -------------------------------------------------------------------------
    # Resource pool (SOM is pooled by player x guild)
    # -------------------------------------------------------------------------
    
    pool_index <- which(
      game$resource_pools$player == player &
        game$resource_pools$guild  == guild
    )
    
    if (length(pool_index) != 1) {
      stop("No unique resource pool for Player ", player, " / ", guild, ".")
    }
    
    available_SOM <- game$resource_pools$SOM[pool_index]
    SOM_required  <- times * duplication_cost
    
    # ---- ILLEGAL MOVE CHECK: reject overspending outright, don't cap it ----
    if (SOM_required > available_SOM) {
      stop(sprintf(
        paste(
          "Illegal move: Player %d cannot afford %d duplication event(s)",
          "of %s on tile %d (%d SOM required, %.0f SOM available)."
        ),
        player, times, guild, tile, SOM_required, available_SOM
      ))
    }
    
    # -------------------------------------------------------------------------
    # Apply duplication
    # -------------------------------------------------------------------------
    
    n_added          <- times * yield
    abundance_start  <- game$populations$n[population_index]
    abundance_end    <- abundance_start + n_added
    
    game$populations$n[population_index] <- abundance_end
    game$resource_pools$SOM[pool_index]  <- available_SOM - SOM_required
    
    demographic_rows[[i]] <- data.frame(
      turn              = game$turn,
      process           = "duplication",
      tile              = tile,
      player            = player,
      guild             = guild,
      abundance_start   = abundance_start,
      abundance_change  = n_added,
      abundance_end     = abundance_end,
      stringsAsFactors  = FALSE
    )
  }
  
  demographic_rows <- demographic_rows[!vapply(demographic_rows, is.null, logical(1))]
  
  if (length(demographic_rows) > 0) {
    game$demographic_history <- rbind(
      game$demographic_history,
      do.call(rbind, demographic_rows)
    )
  }
  
  return(game)
}