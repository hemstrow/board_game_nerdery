library(data.table)
get_part <- function(x, element, attr, id, val){
  selected <- rvest::html_elements(x, element)
  selected <- rvest::html_attrs(selected)
  selected_part <- unlist(purrr::map(selected, attr))
  selected <- selected[which(selected_part == id)]
  selected <- unlist(purrr::map(selected, val))
  return(selected)
}

swaps <- matrix(c("Tzolk'in", "Tzolk'in: The Mayan Calendar",
                  "Railroad Ink", "Railroad Ink: Deep Blue Edition"), byrow = T, ncol = 2)


# BGG
download.file(paste0("https://raw.githubusercontent.com/beefsack/bgg-ranking-historicals/master/", Sys.Date(), ".csv"), destfile = "bgg_games.csv")
bgg <- read.csv("bgg_games.csv")
bgg <- bgg[which(bgg$Users.rated > 2000),]
nqueries <- ceiling(nrow(bgg)/250)
ids <- split(bgg$ID, f = 1:nqueries)

bgg_games <- vector("list", nqueries)
for(i in 1:length(ids)){
  query <- paste0("https://www.boardgamegeek.com/xmlapi2/thing?id=",
                  paste0(ids[[i]], collapse = ","))
  bgg_part <- rvest::read_html(query)
  bgg_games[[i]] <- data.table::data.table(
    Name = get_part(bgg_part, "name", "type", "primary", "value"),
    Year = as.numeric(rvest::html_attr(rvest::html_elements(bgg_part, "yearpublished"), "value")),
    MinPlayers = as.numeric(rvest::html_attr(rvest::html_elements(bgg_part, "minplayers"), "value")),
    MaxPlayers = as.numeric(rvest::html_attr(rvest::html_elements(bgg_part, "maxplayers"), "value"))
  )
  Sys.sleep(2)
}
bgg_games <- data.table::rbindlist(bgg_games)

bgg_games <- data.table::merge.data.table(bgg_games, data.table::as.data.table(bgg), by = c("Name", "Year"))


# BGA
bga_query <- paste0("https://boardgamearena.com/gamelist")
bga <- rvest::read_html(bga_query)
bga_node <- rvest::html_elements(bga, "option")
bga_node <- as.character(bga_node)
bga_games <- grep("gameselect", bga_node)
bga_games <- bga_node[bga_games]
bga_games <- gsub(".+\\\">", "", bga_games)
bga_games <- gsub("</option.+", "", bga_games)
bga_games[which(bga_games %in% swaps[,1])] <- swaps[match(bga_games[which(bga_games %in% swaps[,1])], swaps[,1]), 2]


# Combine
matches <- which(bgg_games$Name %in% bga_games)
hit_games <- bgg_games[matches,]
hit_games <- dplyr::arrange(hit_games, desc(Bayes.average))
hit_games$URL <- paste0("https://boardgamegeek.com/", hit_games$URL)
hit_games$Name <- paste0("<a href='",hit_games$URL,"'>",hit_games$Name,"</a>")
hit_games$Thumbnail <- paste0("<img src='", hit_games$Thumbnail, "' height='52'></img>")
hit_games$ID <- NULL
col_sort <- c(10, 1, 2, 3, 4, 5, 6, 8)
hit_games <- hit_games[,..col_sort]
colnames(hit_games)[ncol(hit_games)] <- "NumberRatings"
colnames(hit_games)[7] <- "Score"
save(hit_games, file = "bgg_bga_shiny/data/games_info.rda")

