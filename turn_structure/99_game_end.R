###############################################################################
#### GAME END #################################################################
###############################################################################

game_end <- function(game,
                     occupy_n = 5,
                     dominate_n = 3,
                     low_resource_cutoff = 3,
                     high_stress_cutoff = 5,
                     stress_n = 2,
                     symbiosis_n = 3,
                     exclusion_n = 2) {
  
  populations <- game$populations
  tiles <- game$tiles
  players <- sort(unique(populations$player))
  
  # ---------------------------------------------------------------------------
  # Community abundance: one row per player x tile (zeros included)
  # ---------------------------------------------------------------------------
  abundance <- aggregate(n ~ player + tile, data = populations, FUN = sum)
  
  abundance <- merge(
    expand.grid(player = players, tile = tiles$ID),
    abundance,
    by = c("player", "tile"),
    all.x = TRUE
  )
  abundance$n[is.na(abundance$n)] <- 0
  
  # Tile totals and domination flag
  abundance$total_n <- ave(abundance$n, abundance$tile, FUN = sum)
  abundance$dominates <- abundance$n > 5 & abundance$n > abundance$total_n / 2
  
  # Helper: build a per-player result from a per-player count function
  per_player <- function(count_fn, threshold) {
    n_tiles <- vapply(players, count_fn, numeric(1))
    data.frame(player = players, n_tiles = n_tiles, met = n_tiles >= threshold)
  }
  
  # ---------------------------------------------------------------------------
  # Occupation: community abundance > 3 on at least occupy_n tiles
  # ---------------------------------------------------------------------------
  occupy <- per_player(
    function(p) sum(abundance$player == p & abundance$n > 3),
    occupy_n
  )
  
  # ---------------------------------------------------------------------------
  # Domination: abundance > 5 and > half the tile's microbes
  # ---------------------------------------------------------------------------
  dominate <- per_player(
    function(p) sum(abundance$player == p & abundance$dominates),
    dominate_n
  )
  
  # ---------------------------------------------------------------------------
  # Dominate ALL low-resource tiles (SOM <= cutoff)
  # ---------------------------------------------------------------------------
  low_resource_tiles <- tiles$ID[tiles$SOM_generation <= low_resource_cutoff]
  # low_resource_tiles <- tiles$ID[tiles$SOM <= low_resource_cutoff]
  
  low_resource <- data.frame(
    player = players,
    met = vapply(
      players,
      function(p) {
        # If there are no low-resource tiles, nobody can meet this objective
        length(low_resource_tiles) > 0 &&
          all(abundance$dominates[abundance$player == p &
                                    abundance$tile %in% low_resource_tiles])
      },
      logical(1)
    )
  )
  
  # ---------------------------------------------------------------------------
  # Dominate N high-stress tiles (stress > cutoff)
  # ---------------------------------------------------------------------------
  high_stress_tiles <- tiles$ID[tiles$stress > high_stress_cutoff]
  
  stress <- per_player(
    function(p) sum(abundance$player == p &
                      abundance$tile %in% high_stress_tiles &
                      abundance$dominates),
    stress_n
  )
  
  # ---------------------------------------------------------------------------
  # Symbiosis: 2+ players each with abundance > 5 on the same tile
  # ---------------------------------------------------------------------------
  strong <- abundance$n > 5
  players_strong_per_tile <- tapply(strong, abundance$tile, sum)
  symbiosis_tiles <- as.numeric(names(players_strong_per_tile)[players_strong_per_tile >= 2])
  
  symbiosis_result <- per_player(
    function(p) sum(abundance$player == p &
                      abundance$tile %in% symbiosis_tiles &
                      strong),
    symbiosis_n
  )
  
  # ---------------------------------------------------------------------------
  # Competitive exclusion: placeholder until adjacency is defined
  # ---------------------------------------------------------------------------
  exclusion <- data.frame(player = players, n_groups = 0, met = FALSE)
  
  list(
    turn = game$turn,
    occupy = occupy,
    dominate = dominate,
    low_resource = low_resource,
    high_stress = stress,
    symbiosis = symbiosis_result,
    competitive_exclusion = exclusion
  )
}
