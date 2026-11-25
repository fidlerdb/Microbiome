###############################################################################
# (3) RESOURCE COMPETITION ###################################################
###############################################################################

calculate_SOM_demand <- function(game) {
  
  x <- game$populations[which(game$populations$n > 0), , drop = FALSE]
  
  x$Acquisition <- game$microbes$Acquisition[
    match(x$guild, game$microbes$Guild)
  ]
  
  x$resource_priority <- game$microbes$resource_priority[
    match(x$guild, game$microbes$Guild)
  ]
  
  if (any(is.na(x$Acquisition)) || any(is.na(x$resource_priority))) {
    bad_guilds <- unique(x$guild[is.na(x$Acquisition) | is.na(x$resource_priority)])
    stop("Unknown guild(s): ", paste(bad_guilds, collapse = ", "))
  }
  
  x$SOM_demand <- x$n * x$Acquisition
  
  return(x)
}

# ------------------------------------------------------------------------- #
# Resolve competition for SOM on a single tile.
#
# If demand <= supply, everyone gets what they asked for.
#
# Otherwise, SOM is captured in STRICT resource_priority order (see
# `microbes$resource_priority`): every population at priority 1
# (Competitor) is filled in full before priority 2 (Opportunist) gets
# anything, and priority 2 is filled in full before priority 3
# (Oligotroph) gets anything. This is a hard precedence rule, not a
# trait-weighted tie-break -- ties WITHIN the same priority tier are
# broken arbitrarily (stable order of appearance), since the rule says
# nothing about how same-guild populations from different players should
# split a shortfall.
# ------------------------------------------------------------------------- #
allocate_SOM_on_tile <- function(tile_SOM_available, demand_tile) {
  
  total_demand <- sum(demand_tile$SOM_demand)
  
  demand_tile$resource_limited <- total_demand > tile_SOM_available
  demand_tile$SOM_available    <- tile_SOM_available
  
  if (!demand_tile$resource_limited[1]) {
    
    demand_tile$SOM_acquired <- demand_tile$SOM_demand
    
  } else {
    
    # Strict guild precedence: Competitor (1) before Opportunist (2)
    # before Oligotroph (3). order() is stable, so populations within the
    # same priority tier keep their original (row) order.
    demand_tile <- demand_tile[order(demand_tile$resource_priority), , drop = FALSE]
    
    remaining_SOM <- tile_SOM_available
    demand_tile$SOM_acquired <- 0
    
    for (i in seq_len(nrow(demand_tile))) {
      
      if (remaining_SOM <= 0) break
      
      acquired <- min(demand_tile$SOM_demand[i], remaining_SOM)
      demand_tile$SOM_acquired[i] <- acquired
      remaining_SOM <- remaining_SOM - acquired
    }
  }
  
  return(demand_tile)
}

# ------------------------------------------------------------------------- #
# Full resource phase: demand -> tile-by-tile allocation -> resource-induced
# mortality -> SOM banked into player x guild resource pools.
# ------------------------------------------------------------------------- #
resource_phase <- function(game, verbose = TRUE) {
  
  demand <- calculate_SOM_demand(game)
  if (nrow(demand) == 0) return(game)
  
  tile_ids <- sort(unique(demand$tile))
  resource_rows    <- vector("list", length(tile_ids))
  demographic_rows <- vector("list", length(tile_ids))
  
  if (verbose) cat("\n---------- Resource competition ----------\n")
  
  for (i in seq_along(tile_ids)) {
    
    tile_id            <- tile_ids[i]
    tile_index         <- which(game$tiles$ID == tile_id)
    tile_SOM_available <- game$tiles$SOM[tile_index]
    
    demand_tile <- demand[demand$tile == tile_id, , drop = FALSE]
    allocation  <- allocate_SOM_on_tile(tile_SOM_available, demand_tile)
    
    # Deduct consumed SOM from the tile
    game$tiles$SOM[tile_index] <- tile_SOM_available - sum(allocation$SOM_acquired)
    
    # -----------------------------------------------------------------------
    # Mortality: a population that did not get all the SOM it demanded
    # has its abundance halved (rounded down).
    # -----------------------------------------------------------------------
    allocation$shortfall     <- allocation$SOM_demand - allocation$SOM_acquired
    allocation$halved        <- allocation$shortfall > 1e-9
    allocation$abundance_start <- allocation$n
    allocation$abundance_end   <- ifelse(allocation$halved,
                                         floor(allocation$n / 2),
                                         allocation$n)
    
    # ---- Print the maths ---------------------------------------------------
    if (verbose) {
      total_demand <- sum(allocation$SOM_demand)
      cat(sprintf("\nTile %s: %g SOM available, total demand %g -> %s\n",
                  tile_id, tile_SOM_available, total_demand,
                  if (total_demand > tile_SOM_available)
                    "SHORTFALL, strict priority order (1 first)"
                  else "enough for everyone"))
      remaining <- tile_SOM_available
      for (j in seq_len(nrow(allocation))) {
        cat(sprintf(
          "  [priority %d] P%s %-11s: %g cells x acquisition %g = demand %g | pool before %g | gets %g | short %g -> %s\n",
          allocation$resource_priority[j], allocation$player[j], allocation$guild[j],
          allocation$n[j], allocation$Acquisition[j], allocation$SOM_demand[j],
          remaining, allocation$SOM_acquired[j], allocation$shortfall[j],
          if (allocation$halved[j])
            sprintf("HALVED %g -> %g", allocation$n[j], allocation$abundance_end[j])
          else "unchanged"))
        remaining <- remaining - allocation$SOM_acquired[j]
      }
      cat(sprintf("  Tile SOM left: %g\n", tile_SOM_available - sum(allocation$SOM_acquired)))
    }
    
    # Apply to persistent population state
    for (j in seq_len(nrow(allocation))) {
      pop_index <- which(
        game$populations$tile   == allocation$tile[j] &
          game$populations$player == allocation$player[j] &
          game$populations$guild  == allocation$guild[j]
      )
      game$populations$n[pop_index] <- allocation$abundance_end[j]
    }
    
    resource_rows[[i]] <- data.frame(
      turn             = game$turn,
      tile             = allocation$tile,
      player           = allocation$player,
      guild            = allocation$guild,
      SOM_available    = allocation$SOM_available,
      SOM_demand       = allocation$SOM_demand,
      SOM_acquired     = allocation$SOM_acquired,
      resource_limited = allocation$resource_limited,
      stringsAsFactors = FALSE
    )
    
    demographic_rows[[i]] <- data.frame(
      turn             = game$turn,
      process          = "resource_mortality",
      tile             = allocation$tile,
      player           = allocation$player,
      guild            = allocation$guild,
      abundance_start  = allocation$abundance_start,
      abundance_change = allocation$abundance_end - allocation$abundance_start,
      abundance_end    = allocation$abundance_end,
      stringsAsFactors = FALSE
    )
  }
  
  resource_rows    <- do.call(rbind, resource_rows)
  demographic_rows <- do.call(rbind, demographic_rows)
  
  game$resource_history    <- rbind(game$resource_history, resource_rows)
  game$demographic_history <- rbind(game$demographic_history, demographic_rows)
  
  # Bank acquired SOM into player x guild pools
  for (k in seq_len(nrow(resource_rows))) {
    if (resource_rows$SOM_acquired[k] <= 0) next
    pool_index <- which(
      game$resource_pools$player == resource_rows$player[k] &
        game$resource_pools$guild  == resource_rows$guild[k]
    )
    if (length(pool_index) != 1) stop("Could not find resource pool for player/guild while banking SOM.")
    game$resource_pools$SOM[pool_index] <-
      game$resource_pools$SOM[pool_index] + resource_rows$SOM_acquired[k]
  }
   
  game
}