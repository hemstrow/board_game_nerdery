library(googlesheets); library(dplyr); source("D:/Users/hemst/Documents/GitHub/Board_game_nerdery/board_game_suggest_fun.R")

# set if players are here or not
players <- c(Will = T,
            Jessica = T,
            Haley = F,
            Adam = T,
            Amy = T,
            Tez = F,
            Yuzo = F,
            Melissa = F,
            Andrea = F)

# set weighting parameters
novelty <- NULL
time_since_played <- NULL
bgg.score <- NULL

# set cuttoffs 
time <- 200 # (number or NULL)
n.players <- "play_all" # play_all: plays everyone present, number: plays at least that number, NULL: any
complexity <- NULL # (number or NULL)

# get logged in and set data
gs_ls()
bg <- as.data.frame(gs_read(ss=gs_title("Boardgames+others"), ws = "Sheet1"), stringsAsFactors = F)

# suggest a game!
(suggestions <- suggest_games(bg, players, novelty, time_since_played, bgg.score, time, n.players, complexity))

