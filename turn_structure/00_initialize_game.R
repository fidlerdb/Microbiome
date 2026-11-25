# initialize_game <- function(
#     players = c(1, 2),
#     guilds = c(
#       "Opportunist",
#       "Competitor",
#       "Oligotroph"
#     ),
#     n = 0,
#     seed = 20260831
# ) {
#   
#   set.seed(seed)
#   
#   # ---------------------------------------------------------------------------
#   # Environment
#   # ---------------------------------------------------------------------------
#   
#   tiles <- data.frame(
#     ID = 1:20,
#     SOM = rpois(20, 4)
#   )
#   
#   # For testing
#   # tiles <- data.frame(
#   #   ID = 1:6,
#   #   SOM = rpois(n = 6, lambda = 4)
#   # )
#   
#   # Maximum / normal replenishment of SOM each turn
#   tiles$SOM_generation <- tiles$SOM
#   
#   
#   # ---------------------------------------------------------------------------
#   # Guild traits
#   # ---------------------------------------------------------------------------
#   
#   # resource_priority encodes a STRICT SOM-capture order, independent of
#   # the Acquisition trait: lower number = captures SOM first. Competitors
#   # always draw down a tile's SOM before Opportunists get anything, and
#   # Opportunists before Oligotrophs -- there is no trait-based tie-breaking,
#   # this ordering is absolute.
#   microbes <- data.frame(
#     Guild = c(
#       "Opportunist",
#       "Competitor",
#       "Oligotroph"
#     ),
#     Yield = c(4, 2, 1),
#     Acquisition = c(3, 2, 1),
#     Stress_tolerance = c(0, 1, 1),
#     Duplication_cost = c(3, 3, 2),
#     resource_priority = c(2, 1, 3),
#     stringsAsFactors = FALSE
#   )
#   
#   # Guild-specific mortality suffered when a tile is resource-limited
#   # (i.e. total demand on the tile exceeded available SOM that turn).
#   mortality_traits <- data.frame(
#     Guild = c(
#       "Opportunist",
#       "Competitor",
#       "Oligotroph"
#     ),
#     Stress_mortality = c(2, 1, 1),
#     stringsAsFactors = FALSE
#   )
#   
#   
#   # ---------------------------------------------------------------------------
#   # Population state
#   #
#   # One row = one player x tile x guild population.
#   #
#   # We retain zero rows so that every possible combination exists.
#   # This makes adding microbes to a previously empty combination easy.
#   # ---------------------------------------------------------------------------
#   
#   populations <- expand.grid(
#     tile = tiles$ID,
#     player = players,
#     guild = guilds,
#     stringsAsFactors = FALSE
#   )
#   
#   populations$n <- n
#   
#   
#   # ---------------------------------------------------------------------------
#   # Resource pools
#   #
#   # One private SOM pool per player x guild.
#   # ---------------------------------------------------------------------------
#   
#   resource_pools <- expand.grid(
#     player = players,
#     guild = guilds,
#     stringsAsFactors = FALSE
#   )
#   
#   resource_pools$SOM <- 0
#   
#   
#   # ---------------------------------------------------------------------------
#   # Game object
#   # ---------------------------------------------------------------------------
#   
#   game <- list(
#     
#     # Current turn
#     turn = 0,
#     
#     # Random seed used to initialise game
#     seed = seed,
#     
#     # Environment
#     tiles = tiles,
#     
#     # Guild traits
#     microbes = microbes,
#     
#     # Guild-specific resource-stress mortality traits
#     mortality_traits = mortality_traits,
#     
#     # Microbial populations on board
#     populations = populations,
#     
#     # Private SOM reserves
#     resource_pools = resource_pools,
#     
#     # -------------------------------------------------------------------------
#     # Tracing tables (see header comment for the ecological meaning of each)
#     # -------------------------------------------------------------------------
#     
#     resource_history = data.frame(
#       turn = integer(),
#       tile = integer(),
#       player = integer(),
#       guild = character(),
#       SOM_available = numeric(),
#       SOM_demand = numeric(),
#       SOM_acquired = numeric(),
#       resource_limited = logical(),
#       stringsAsFactors = FALSE
#     ),
#     
#     demographic_history = data.frame(
#       turn = integer(),
#       process = character(),
#       tile = integer(),
#       player = integer(),
#       guild = character(),
#       abundance_start = numeric(),
#       abundance_change = numeric(),
#       abundance_end = numeric(),
#       stringsAsFactors = FALSE
#     ),
#     
#     abundance_census = data.frame(
#       turn = integer(),
#       tile = integer(),
#       player = integer(),
#       guild = character(),
#       abundance = numeric(),
#       stringsAsFactors = FALSE
#     )
#   )
#   
#   return(game)
# }

initialize_game <- function(
    players = c(1, 2),
    n_tiles = 7,
    guilds = c(
      "Opportunist",
      "Competitor",
      "Oligotroph"
    ),
    n = 0,
    seed = 20260831
) {
  
  set.seed(seed)
  
  # ---------------------------------------------------------------------------
  # Environment
  # ---------------------------------------------------------------------------
  
  tiles <- data.frame(
    ID = 1:n_tiles,
    SOM = rpois(n_tiles, 4)
  )
  # Deal with high numbers of 0 SOM tiles
  tiles$SOM <- ifelse(tiles$SOM == 0
                 , yes = round(rbinom(n = 1, size = 1, prob = 0.7))
                 , no = tiles$SOM
                 )
  
  # For testing
  # tiles <- data.frame(
  #   ID = 1:6,
  #   SOM = rpois(n = 6, lambda = 4)
  # )
  
  # Maximum / normal replenishment of SOM each turn
  tiles$SOM_generation <- tiles$SOM
  
  # Environmental stress level per tile, generated the same way as SOM for
  # now (used by game_end()'s "high stress tiles" condition already).
  # Nothing in the turn pipeline reads this yet -- see "(6) FUTURE
  # MECHANICS" in 90_census.R for the open design questions (does it
  # regenerate each turn like SOM, is it consumed by anything, how does it
  # combine with the guilds' Stress_tolerance trait) before wiring it up.
  tiles$stress <- rpois(n_tiles, 4)
  
  
  # ---------------------------------------------------------------------------
  # Guild traits
  # ---------------------------------------------------------------------------
  
  # resource_priority encodes a STRICT SOM-capture order, independent of
  # the Acquisition trait: lower number = captures SOM first. Competitors
  # always draw down a tile's SOM before Opportunists get anything, and
  # Opportunists before Oligotrophs -- there is no trait-based tie-breaking,
  # this ordering is absolute.
  microbes <- data.frame(
    Guild = c(
      "Opportunist",
      "Competitor",
      "Oligotroph"
    ),
    Yield = c(4, 2, 1),
    Acquisition = c(3, 2, 1),
    # Stress_tolerance: not read anywhere yet -- intended to offset
    # tiles$stress once that mechanic is defined; see "(6) FUTURE
    # MECHANICS" in 90_census.R.
    Stress_tolerance = c(0, 1, 1),
    Duplication_cost = c(3, 3, 2),
    resource_priority = c(2, 1, 3),
    stringsAsFactors = FALSE
  )
  
  
  # ---------------------------------------------------------------------------
  # Population state
  #
  # One row = one player x tile x guild population.
  #
  # We retain zero rows so that every possible combination exists.
  # This makes adding microbes to a previously empty combination easy.
  # ---------------------------------------------------------------------------
  
  populations <- expand.grid(
    tile = tiles$ID,
    player = players,
    guild = guilds,
    stringsAsFactors = FALSE
  )
  
  populations$n <- n
  
  
  # ---------------------------------------------------------------------------
  # Resource pools
  #
  # One private SOM pool per player x guild.
  # ---------------------------------------------------------------------------
  
  resource_pools <- expand.grid(
    player = players,
    guild = guilds,
    stringsAsFactors = FALSE
  )
  
  resource_pools$SOM <- 0
  
  
  # ---------------------------------------------------------------------------
  # Game object
  # ---------------------------------------------------------------------------
  
  game <- list(
    
    # Current turn
    turn = 0,
    
    # Random seed used to initialise game
    seed = seed,
    
    # Environment
    tiles = tiles,
    
    # Guild traits
    microbes = microbes,
    
    # Microbial populations on board
    populations = populations,
    
    # Private SOM reserves
    resource_pools = resource_pools,
    
    # -------------------------------------------------------------------------
    # Tracing tables (see header comment for the ecological meaning of each)
    # -------------------------------------------------------------------------
    
    resource_history = data.frame(
      turn = integer(),
      tile = integer(),
      player = integer(),
      guild = character(),
      SOM_available = numeric(),
      SOM_demand = numeric(),
      SOM_acquired = numeric(),
      resource_limited = logical(),
      stringsAsFactors = FALSE
    ),
    
    demographic_history = data.frame(
      turn = integer(),
      process = character(),
      tile = integer(),
      player = integer(),
      guild = character(),
      abundance_start = numeric(),
      abundance_change = numeric(),
      abundance_end = numeric(),
      stringsAsFactors = FALSE
    ),
    
    abundance_census = data.frame(
      turn = integer(),
      tile = integer(),
      player = integer(),
      guild = character(),
      abundance = numeric(),
      stringsAsFactors = FALSE
    )
  )
  
  return(game)
}
