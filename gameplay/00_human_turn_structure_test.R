
# A game script

rm(list = ls())

# Enable the game
source("load_game.R")

# Start the game
set.seed(15)
game <- initialize_game(players = 1:2)

# Visualise the tile values
ggplot(game$tiles, aes(x = factor(ID))) +
  geom_col(aes(y = SOM), fill = 'brown4') +
  geom_col(aes(y = stress), fill = 'salmon')

## Turn structure for two players
game <- start_turn(game)

# How the board
plot_microbiome(game)

# P1 adds microbes
game <- add_microbes(
  game,
  # TODO Which tile will you put a microbe on?
  tile = 7,# c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
  player = 1,
  # TODO Choose a guiild!
  guild = "Opportunist" # c("Opportunist", "Competitor", "Oligotroph")
)

# P2 adds microbes
game <- add_microbes(
  game,
  # TODO Which tile will you put a microbe on?
  tile = 11, #c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
  player = 2,
  # TODO Choose a guiild!
  guild = "Competitor" # c("Opportunist", "Competitor", "Oligotroph")
)

# Resources are competed over
game <- resource_phase(game)

# How the board
plot_microbiome(game)

# TODO Choose duplication events for P1 and P2
dup_selection <- data.frame(
  player = c(1) #, 2)
  , tile = c(7) # , 11)
  , guild = c("Opportunist") #, "Competitor") # c(c("Opportunist", "Competitor", "Oligotroph"), c("Opportunist", "Competitor", "Oligotroph"))
  , times = c(1) #, 1)
  )

# Enact duplication events if they are legal
if (!is.null(dup_selection)) {
  game <- duplication_phase(game, dup_selection)
}

# Keep track of what has happened
game <- record_abundance_census(game)

# How the board
plot_microbiome(game)

# Show player resources
plot_resources(game)

# TODO Check for game end
game_end(game)




##########################################



# Add microbes pgame = # Add microbes phase

ggplot(game$tiles, aes(x = reorder(factor(ID), SOM))) +
  geom_col(aes(y = SOM), fill = 'brown4') +
  geom_col(aes(y = stress), fill = 'red')





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




microbiome_turn <- function(game, player) {
  
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