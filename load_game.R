
# Load game functions

# Turn structure
source(here::here("turn_structure/00_initialize_game.R"))
source(here::here("turn_structure/01_start_turn.R"))
source(here::here("turn_structure/02_biomass_addition.R"))
source(here::here("turn_structure/03_resource_competition.R"))
source(here::here("turn_structure/04_duplication.R"))
source(here::here("turn_structure/90_census.R"))
source(here::here("turn_structure/99_game_end.R"))

# Game wrapper
source(here::here("turn_wrapper/turn_wrapper.R"))


# Visualization
source(here::here("turn_plots/turn_plots.R"))
