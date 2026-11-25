
# add_microbes <- function(
#     game,
#     tile,
#     player,
#     guild,
#     n = 1
# ) {
#   
#   if (n <= 0) {
#     stop(
#       "Illegal move: cannot add a non-positive number of microbes (n = ",
#       n, ")."
#     )
#   }
#   
#   index <- which(
#     game$populations$tile == tile &
#       game$populations$player == player &
#       game$populations$guild == guild
#   )
#   
#   if (length(index) != 1) {
#     stop(
#       "Could not uniquely identify tile/player/guild combination ",
#       "(tile = ", tile, ", player = ", player, ", guild = ", guild, ")."
#     )
#   }
#   
#   game$populations$n[index] <- game$populations$n[index] + n
#   
#   return(game)
# }

add_microbes <- function(
    game,
    tile,
    player,
    guild,
    n = 1
) {
  
  if (n <= 0) {
    stop(
      "Illegal move: cannot add a non-positive number of microbes (n = ",
      n, ")."
    )
  }
  
  index <- which(
    game$populations$tile == tile &
      game$populations$player == player &
      game$populations$guild == guild
  )
  
  if (length(index) != 1) {
    stop(
      "Could not uniquely identify tile/player/guild combination ",
      "(tile = ", tile, ", player = ", player, ", guild = ", guild, ")."
    )
  }
  
  game$populations$n[index] <- game$populations$n[index] + n
  
  return(game)
}