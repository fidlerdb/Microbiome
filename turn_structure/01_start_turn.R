
start_turn <- function(game) {
  
  # Advance turn
  game$turn <- game$turn + 1
  
  # Replenish SOM on every tile
  game$tiles$SOM <- game$tiles$SOM_generation
  
  return(game)
}