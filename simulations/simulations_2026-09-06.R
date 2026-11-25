# Microbiome test

# v0.1.3

rm(list = ls())

source("C:/Users/bspa44/OneDrive - Bangor University/Laptop/Documents/Other/Microbiome/turn_structure/00_initialize_game.R")
source("C:/Users/bspa44/OneDrive - Bangor University/Laptop/Documents/Other/Microbiome/turn_structure/01_start_turn.R")
source("C:/Users/bspa44/OneDrive - Bangor University/Laptop/Documents/Other/Microbiome/turn_structure/02_biomass_addition.R")
source("C:/Users/bspa44/OneDrive - Bangor University/Laptop/Documents/Other/Microbiome/turn_structure/03_resource_competition.R")
source("C:/Users/bspa44/OneDrive - Bangor University/Laptop/Documents/Other/Microbiome/turn_structure/04_duplication.R")
source("C:/Users/bspa44/OneDrive - Bangor University/Laptop/Documents/Other/Microbiome/turn_structure/90_census.R")
source("C:/Users/bspa44/OneDrive - Bangor University/Laptop/Documents/Other/Microbiome/turn_structure/99_game_end.R")

source("C:/Users/bspa44/OneDrive - Bangor University/Laptop/Documents/Other/Microbiome/turn_plots/turn_plots.R")


###############################################################################
# MICROBIOME BOARD GAME SIMULATION
###############################################################################
#
# Ecological vocabulary used throughout this file:
#
#   SOM        : Soil Organic Matter -- the shared, tile-level resource pool
#   guild      : a microbial functional strategy (Opportunist / Competitor /
#                Oligotroph), each with its own Acquisition, Yield,
#                Duplication_cost and stress-mortality traits
#   resource_history     : per turn x tile x player x guild record of SOM
#                          demand / acquisition / limitation
#   demographic_history  : per turn record of abundance-changing EVENTS,
#                          each tagged with the ecological `process` that
#                          caused it ("resource_mortality" or "duplication")
#   abundance_census     : a plain snapshot of standing population per
#                          turn x tile x player x guild, for easy plotting
#
# Design note on illegal moves:
#   Any request that would overspend a player's SOM pool is REJECTED with
#   stop() rather than silently reduced. Because R functions are
#   copy-on-modify, an aborted call never partially mutates the `game`
#   object the caller holds -- so a rejected move leaves the game state
#   untouched, and the caller must resubmit a legal move.
#
###############################################################################


##############################
#### Automated multi-turn ####
##############################

# ------------------------------------------------------------------------- #
# Only proposes duplications the player can actually afford, so the
# automated loop below never trips the illegal-move guard in
# duplication_phase(). A real player's UI would do the equivalent
# affordability check before submitting a move.
# ------------------------------------------------------------------------- #
generate_random_duplication <- function(game, player) {
  
  proposals <- lapply(player, function(p) {
    
    eligible <- game$populations[
      game$populations$player == p &
        game$populations$n > 0,
      ,
      drop = FALSE
    ]
    
    if (nrow(eligible) == 0) return(NULL)
    
    affordable <- vapply(seq_len(nrow(eligible)), function(i) {
      
      guild <- eligible$guild[i]
      
      cost <- game$microbes$Duplication_cost[
        match(guild, game$microbes$Guild)
      ]
      
      pool <- game$resource_pools$SOM[
        game$resource_pools$player == p &
          game$resource_pools$guild  == guild
      ]
      
      length(pool) == 1 && pool >= cost
      
    }, logical(1))
    
    eligible <- eligible[affordable, , drop = FALSE]
    
    if (nrow(eligible) == 0) return(NULL)
    
    x <- eligible[sample(seq_len(nrow(eligible)), 1), , drop = FALSE]
    
    data.frame(
      player = p,
      tile   = x$tile,
      guild  = x$guild,
      times  = 1,
      stringsAsFactors = FALSE
    )
  })
  
  proposals <- proposals[!vapply(proposals, is.null, logical(1))]
  
  if (length(proposals) == 0) {
    return(data.frame(
      player = integer(),
      tile   = integer(),
      guild  = character(),
      times  = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  do.call(rbind, proposals)
}



do_game <- function(players = 1:2, turns = 100) {
  # TODO: Add stress to tiles
  game <- initialize_game(players = players)
  
  for (i in 1:turns) {
    game <- start_turn(game)
    
    for (p in players){
      game <- add_microbes(
        game,
        tile = sample(game$tiles$ID, 1),
        player = p,
        guild = sample(game$microbes$Guild, 1)
      )
    }
    
    game <- resource_phase(game)
    dups = lapply(players, generate_random_duplication, game = game)
    
    dup_tactic <- do.call("rbind", dups)
    
    if (!is.null(dup_tactic)) {
      game <- duplication_phase(game, dup_tactic)
    }
    
    game <- record_abundance_census(game)
    # TODO: Game end
    
  }
  
  return(game)
}

###### Play the game ######

game_100 <- do_game(players = 1:4)

# Inspect the tile stats :)
# game_100$tiles

###############################################################################
###### DOWNSTREAM ANALYSIS (data.table) ######
###############################################################################

library(data.table)

# ---------------------------------------------------------------------------
# Guild abundance over time (from the abundance census, summed across tiles)
# ---------------------------------------------------------------------------

census_dt <- as.data.table(game_100$abundance_census)

abundance_by_turn <- census_dt[
  , .(abundance = sum(abundance)),
  keyby = .(turn, guild, player)
]

abundance_by_turn

ggplot(
  abundance_by_turn,
  aes(x = turn, y = abundance, colour = guild)
) +
  geom_line() +
  facet_grid(player ~ .) +
  labs(
    x = "Turn",
    y = "Total microbe abundance",
    colour = "Guild",
    title = "Guild abundance over time"
  )

# ---------------------------------------------------------------------------
# Resource acquisition over time (from resource_history, summed across tiles)
# ---------------------------------------------------------------------------

resource_dt <- as.data.table(game_100$resource_history)

SOM_acquired_by_turn <- resource_dt[
  , .(SOM_acquired = sum(SOM_acquired)),
  keyby = .(turn, guild, player)
]

ggplot(
  SOM_acquired_by_turn,
  aes(x = turn, y = SOM_acquired, colour = guild)
) +
  geom_line() +
  facet_grid(player ~ .) +
  labs(
    x = "Turn",
    y = "SOM acquired",
    colour = "Guild",
    title = "Resource acquisition over time"
  )

# ---------------------------------------------------------------------------
# Demographic events broken out by ecological process
# (mortality vs. duplication), for anyone who wants to see the components
# that produce the net abundance curve above.
# ---------------------------------------------------------------------------

demographic_dt <- as.data.table(game_100$demographic_history)

demographic_dt[
  , .(abundance_change = sum(abundance_change)),
  keyby = .(turn, process, guild, player)
]

ggplot(demographic_dt[
  , .(abundance_change = sum(abundance_change)),
  keyby = .(turn, process, guild, player)]
  , aes(x = turn, y = abundance_change, colour = process)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  facet_grid(player + guild ~ .) +
  theme_bw() +
  theme(strip.text.y.right = element_text(angle = 0)
        , legend.position = 'bottom')

###############################################################################
# Automated strategy
###############################################################################

choose_placement <- function(game, player) {
  
  # ---------------------------------------------------------------------------
  # Player 1:
  #   Always Competitor
  #
  # Player 2:
  #   SOM < 2  -> Oligotroph
  #   SOM >= 2 -> Opportunist
  # ---------------------------------------------------------------------------
  
  tile <- sample(game$tiles$ID, 1)
  
  if (player == 1) {
    
    guild <- "Competitor"
    
  } else if (player == 2) {
    
    SOM <- game$tiles$SOM[
      game$tiles$ID == tile
    ]
    
    guild <- if (SOM < 3) {
      "Oligotroph"
    } else {
      "Opportunist"
    }
    
  } else {
    
    stop("This strategy is only defined for players 1 and 2.")
  }
  
  data.frame(
    player = player,
    tile = tile,
    guild = guild,
    stringsAsFactors = FALSE
  )
}

choose_duplication <- function(game, player) {
  
  # Populations belonging to this player
  eligible <- game$populations[
    game$populations$player == player &
      game$populations$n > 0,
    ,
    drop = FALSE
  ]
  
  if (nrow(eligible) == 0) {
    return(NULL)
  }
  
  # Determine resource available to each population
  eligible$resources <- NA_real_
  
  for (i in seq_len(nrow(eligible))) {
    
    tile <- eligible$tile[i]
    guild <- eligible$guild[i]
    
    # History for this population on the current turn
    current_history <- game$history[
      game$history$turn == game$turn &
        game$history$tile == tile &
        game$history$player == player &
        game$history$guild == guild,
      ,
      drop = FALSE
    ]
    
    if (!is.null(current_history) && nrow(current_history) > 0) {
      
      # Use recorded resource acquisition
      eligible$resources[i] <- sum(
        current_history$n_added_resources,
        na.rm = TRUE
      )
      
    } else {
      
      # Turn 1 / no recorded history:
      # use the resource generated by the tile
      eligible$resources[i] <- game$tiles$SOM[
        game$tiles$ID == tile
      ]
    }
  }
  
  # Resource available per microbe
  eligible$resources_per_microbe <-
    eligible$resources / eligible$n
  
  # Determine duplication cost and resource pool
  eligible$duplication_cost <- game$microbes$Duplication_cost[
    match(eligible$guild, game$microbes$Guild)
  ]
  
  eligible$SOM_pool <- vapply(
    seq_len(nrow(eligible)),
    function(i) {
      
      pool <- game$resource_pools$SOM[
        game$resource_pools$player == player &
          game$resource_pools$guild == eligible$guild[i]
      ]
      
      if (length(pool) == 1) pool else NA_real_
    },
    numeric(1)
  )
  
  # Only populations that can afford a duplication
  eligible <- eligible[
    !is.na(eligible$SOM_pool) &
      eligible$SOM_pool >= eligible$duplication_cost,
    ,
    drop = FALSE
  ]
  
  if (nrow(eligible) == 0) {
    return(NULL)
  }
  
  # Choose population with highest resource availability per microbe
  i <- which.max(eligible$resources_per_microbe)
  
  data.frame(
    player = player,
    tile = eligible$tile[i],
    guild = eligible$guild[i],
    times = 1,
    stringsAsFactors = FALSE
  )
}

do_game_strat_1 <- function(
    players = 1:2,
    turns = 100,
    seed = NULL
    
) {
  
  game <- initialize_game(
    players = players
  )
  
  if(is.null(seed)){set.seed(Sys.time())}
  set.seed(seed)
  
  for (i in seq_len(turns)) {
    
    game <- start_turn(game)
    
    
    # -------------------------------------------------------------------------
    # Biomass addition
    # -------------------------------------------------------------------------
    
    for (p in players) {
      
      placement <- choose_placement(
        game = game,
        player = p
      )
      
      game <- add_microbes(
        game,
        tile = placement$tile,
        player = placement$player,
        guild = placement$guild
      )
    }
    
    
    # -------------------------------------------------------------------------
    # Resource acquisition + mortality
    # -------------------------------------------------------------------------
    
    game <- resource_phase(game)
    
    
    # -------------------------------------------------------------------------
    # Duplication
    # -------------------------------------------------------------------------
    
    duplication <- lapply(
      players,
      function(p) {
        choose_duplication(
          game = game,
          player = p
        )
      }
    )
    
    duplication <- duplication[
      !vapply(
        duplication,
        is.null,
        logical(1)
      )
    ]
    
    if (length(duplication) > 0) {
      
      duplication <- do.call(
        rbind,
        duplication
      )
      
      game <- duplication_phase(
        game,
        duplication
      )
    }
    
    
    # -------------------------------------------------------------------------
    # Census
    # -------------------------------------------------------------------------
    
    game <- record_abundance_census(game)
    
    # TODO: Game end
  }
  
  return(game)
}


game_100_strat_1 = do_game_strat_1()

plot_microbiome(game_100_strat_1)


census_dt_s1 <- as.data.table(game_100_strat_1$abundance_census)

abundance_by_turn_s1 <- census_dt_s1[
  , .(abundance = sum(abundance)),
  keyby = .(turn, guild, player)
]

abundance_by_turn_s1

ggplot(
  abundance_by_turn_s1,
  aes(x = turn, y = abundance, colour = guild)
) +
  geom_line() +
  facet_grid(player ~ .) +
  labs(
    x = "Turn",
    y = "Total microbe abundance",
    colour = "Guild",
    title = "Guild abundance over time"
  )

#### Simulate for 200 game ####

s1_analysis = lapply(1:200, \(i){out = do_game_strat_1();return(out)})

###############################################################################
# DOWNSTREAM ANALYSIS (data.table) ############################################
###############################################################################

library(data.table)

# ---------------------------------------------------------------------------
# Guild abundance over time (from the abundance census, summed across tiles)
# ---------------------------------------------------------------------------

census_dt <- as.data.table(
  rbindlist(
    lapply(s1_analysis, \(x){
      x$abundance_census}
    )
    , idcol = 'game')
)

abundance_by_turn <- census_dt[
  , .(abundance = sum(abundance)),
  keyby = .(game, turn, guild, player)
]

ggplot(
  abundance_by_turn,
  aes(x = turn, y = abundance, colour = guild)
) +
  facet_grid(player ~ .) +
  geom_line(aes(group = paste0(game, player, guild)), alpha = 0.2) +
  geom_smooth(aes(group = paste0(player, guild))) +
  labs(
    x = "Turn",
    y = "Total microbe abundance",
    colour = "Guild",
    title = "Guild abundance over time"
  ) +
  theme_classic()

# ---------------------------------------------------------------------------
# Resource acquisition over time (from resource_history, summed across tiles)
# ---------------------------------------------------------------------------

resource_dt <- as.data.table(
  rbindlist(
    lapply(s1_analysis, \(x){
      x$resource_history}
    )
    , idcol = 'game')
)

SOM_acquired_by_turn <- resource_dt[
  , .(SOM_acquired = sum(SOM_acquired)),
  keyby = .(game, turn, guild, player)
]

ggplot(
  SOM_acquired_by_turn,
  aes(x = turn, y = SOM_acquired, colour = guild)
) +
  geom_line(aes(group = paste0(game, player, guild))
            , alpha = 0.1) +
  geom_smooth() +
  facet_grid(player ~ .) +
  labs(
    x = "Turn",
    y = "SOM acquired",
    colour = "Guild",
    title = "Resource acquisition over time"
  ) +
  theme_classic()

SOM_acquired_by_turn_total <- resource_dt[
  , .(SOM_acquired = sum(SOM_acquired)),
  keyby = .(game, turn, player)
]

ggplot(
  SOM_acquired_by_turn_total,
  aes(x = turn, y = SOM_acquired)
) +
  geom_line(aes(group = paste0(game, player))
            , alpha = 0.1) +
  geom_smooth() +
  facet_grid(player ~ .) +
  labs(
    x = "Turn",
    y = "SOM acquired",
    title = "Resource acquisition over time"
  ) +
  theme_classic()

# ---------------------------------------------------------------------------
# Demographic events broken out by ecological process
# (mortality vs. duplication), for anyone who wants to see the components
# that produce the net abundance curve above.
# ---------------------------------------------------------------------------

demographic_dt <- as.data.table(game_100$demographic_history)

demographic_dt[
  , .(abundance_change = sum(abundance_change)),
  keyby = .(turn, process, guild, player)
]

ggplot(demographic_dt[
  , .(abundance_change = sum(abundance_change)),
  keyby = .(turn, process, guild, player)]
  , aes(x = turn, y = abundance_change, colour = process)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  facet_grid(player + guild ~ .) +
  theme_bw() +
  theme(strip.text.y.right = element_text(angle = 0)
        , legend.position = 'bottom')


