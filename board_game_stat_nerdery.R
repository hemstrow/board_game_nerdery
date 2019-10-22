library(ggplot2); library(reshape2); library(GGally); library(googlesheets); library(dplyr); source("D:/Users/hemst/Documents/GitHub/Board_game_nerdery/board_game_suggest_fun.R")


# read in data
bg <- as.data.frame(gs_read(ss=gs_title("Boardgames+others"), ws = "Sheet1"), stringsAsFactors = F)

# format
player.cols <- (which(colnames(bg) == "Last Played") + 1):(which(colnames(bg) == "BGG") - 1)
bg[,player.cols] <- matrix(as.numeric(unlist(bg[,player.cols])), nrow = nrow(bg))
colnames(bg)[1] <- "name"

# get a melted data frame
bgm <- melt(bg, id.vars = colnames(bg)[c(1:(player.cols[1] - 1))])
colnames(bgm)[(ncol(bgm)-1):ncol(bgm)]<- c("player", "rating")
bgm$rating <- as.numeric(bgm$rating)

# plot each players complexity preferences
ggplot(bgm, aes(y = rating, x = Complexity)) + geom_point() + theme_bw() + facet_wrap(~player) + geom_smooth()

# mlm on this?
summary(glm(rating ~ Complexity + player + player:Complexity, data = bgm))

# correlation plot
ggpairs(bg[,player.cols]) + theme_bw()

# plot o' games
ggplot(bgm, aes(y = rating, x = player, color = player)) + geom_point() + facet_wrap(~name) + theme_bw() + 
  scale_color_viridis_d()


