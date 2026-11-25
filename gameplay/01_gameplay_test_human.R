rm(list = ls())
source("load_game.R")

set.seed(15)
game <- initialize_game(players = 1:2)

# Visualise the tile values
ggplot(game$tiles, aes(x = factor(ID))) +
  geom_col(aes(y = SOM), fill = "brown4") +
  geom_col(aes(y = stress), fill = "salmon")

outcome <- play_game(game, players = 1:2
                     #, win_objectives = 3, max_turns = 20
                     )




######


game <- initialize_game(players = 1:2, n_tiles = 7)
game <- start_turn(game)
game <- add_microbes(game, tile = 6, player = 1, guild = "Opportunist")
game <- add_microbes(game, tile = 4, player = 2, guild = "Competitor")

str(game$mortality_traits)          # NA, factor, or character columns?
str(game$populations)               # type of n: integer / numeric?

before <- game$populations
game <- resource_phase(game)
game$populations[game$populations$tile == 11, ]
tail(game$demographic_history)      # abundance_end for the resource_mortality rows

######

names(game)   # is mortality stored under another name?

game <- initialize_game(players = 1:2)
game <- start_turn(game)
low <- game$tiles$ID[which.min(game$tiles$SOM)]
game$tiles$SOM[low]
game <- add_microbes(game, tile = low, player = 1, guild = "Opportunist", n = 10)
game <- add_microbes(game, tile = low, player = 2, guild = "Competitor", n = 10)
game <- resource_phase(game)
game$populations[game$populations$tile == low & game$populations$n != 0, ]
tail(game$demographic_history)



