
# ###############################################################################
# # (5) ABUNDANCE CENSUS ########################################################
# ###############################################################################
# 
# # ------------------------------------------------------------------------- #
# # Snapshot the current standing population (game$populations) into
# # game$abundance_census, tagged with the current turn. Call this once at
# # the end of each turn (see do_game() below) to build a clean per-turn
# # time series independent of individual demographic events.
# # ------------------------------------------------------------------------- #
# record_abundance_census <- function(game) {
#   
#   census_rows <- game$populations[, c("tile", "player", "guild", "n")]
#   census_rows$turn <- game$turn
#   names(census_rows)[names(census_rows) == "n"] <- "abundance"
#   census_rows <- census_rows[, c("turn", "tile", "player", "guild", "abundance")]
#   
#   game$abundance_census <- rbind(game$abundance_census, census_rows)
#   
#   return(game)
# }

###############################################################################
# (5) ABUNDANCE CENSUS ########################################################
###############################################################################

# ------------------------------------------------------------------------- #
# Snapshot the current standing population (game$populations) into
# game$abundance_census, tagged with the current turn. Call this once at
# the end of each turn (see do_game() below) to build a clean per-turn
# time series independent of individual demographic events.
# ------------------------------------------------------------------------- #
record_abundance_census <- function(game) {
  
  census_rows <- game$populations[, c("tile", "player", "guild", "n")]
  census_rows$turn <- game$turn
  names(census_rows)[names(census_rows) == "n"] <- "abundance"
  census_rows <- census_rows[, c("turn", "tile", "player", "guild", "abundance")]
  
  game$abundance_census <- rbind(game$abundance_census, census_rows)
  
  return(game)
}