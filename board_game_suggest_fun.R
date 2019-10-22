suggest_games <- function(bg.dat, players, novelty = 0.5, time_since_played = 0.5, bgg.score = 0.5, time = NULL, n.players = "play_all", complexity = NULL){
  range.scale <- function(x, n.max){
    x <- (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)) * (n.max)
    return(x)
  }

  ############################# check cuttoffs ###########
  #n.players
  if(n.players == "play_all"){
    bg.dat <- bg.dat[bg.dat$N.Players >= sum(players),]
  }
  else if(!is.null(n.players)){
    bg.dat <- bg.dat[bg.dat$N.Players >= n.players, ]
  }
  
  #time
  if(!is.null(time)){
    bg.dat <- bg.dat[bg.dat$Time <= time, ]
  }
  
  #complexity
  if(!is.null(complexity)){
    bg.dat <- bg.dat[bg.dat$Complexity <= complexity, ]
  }
  
  ############################# weight by taste #########
  # which columns contain scores?
  match.col <- which(colnames(bg.dat) %in% names(players)[players == T])
  scores <- bg.dat[,match.col]
  scores <- as.matrix(scores)
  scores[scores == "N/A"] <- NA # fix NA values
  
  
  # convert a,b,c,d to number
  scores[scores == "D-"] <- 6.25
  scores[scores == "D"] <- 6.5
  scores[scores == "D+"] <- 6.75
  scores[scores == "C-"] <- 7.25
  scores[scores == "C"] <- 7.5
  scores[scores == "C+"] <- 7.75
  scores[scores == "B-"] <- 8.25
  scores[scores == "B"] <- 8.5
  scores[scores == "B+"] <- 8.75
  scores[scores == "A-"] <- 9.25
  scores[scores == "A"] <- 9.5
  scores[scores == "A+"] <- 9.75
  scores[scores == "no"] <- 1
  scores <- matrix(as.numeric(scores), nrow(scores))
  colnames(scores) <- colnames(bg.dat)[match.col]
  # standardize player scores so that everyone is equally weighted.
  scores <- apply(scores, 2, function(x){x/mean(x, na.rm = T)}) # scale for mean
  
  # get means score per game as new weight
  means <- rowMeans(scores, na.rm = T)
  means[is.nan(means)] <- mean(means, na.rm = T)
  m_score <- means
  o_m_score <- m_score # save for later
  out <- as.data.frame(cbind(game = bg.dat$X1, player_score = means), stringsAsFactors = F)
  
  ############################## bgg.score ###########
  if(!is.null(bgg.score)){
    bgs <- as.numeric(bg.dat$BGG)
    bgs[is.na(bgs)] <- mean(bgs, na.rm = T)
    bgs <- bgs/mean(bgs)
    m_score <- m_score + bgs*bgg.score
    o_m_score <- m_score
    out <- cbind(out, bgg = bg.dat$BGG)
  }
  
  ############################## weight by novelty ###########
  if(!is.null(novelty)){
    nov <- rowSums(ifelse(is.na(bg.dat[,match.col]), 1, 0))
    out <- cbind(out, novelty = nov)
    nov <- range.scale(nov, max(o_m_score, na.rm = T)) # scale novelty contribution relative to game mean scores.
    m_score <- m_score + nov*novelty
  }
  
  ############################## time since played ###########
  if(!is.null(time_since_played)){
    tsp <- as.Date(bg.dat$`Last Played`)
    tsp[is.na(tsp)] <- sort(tsp)[1] # NA is set to the oldest date.
    tsp <- Sys.Date() - tsp
    out <- cbind(out, time_since_played = tsp)
    tsp <- range.scale(as.numeric(tsp), max(o_m_score, na.rm = T))
    m_score <- m_score + tsp*time_since_played
  }
  
  ############################## final score ##########
  m_score <- range.scale(m_score, 10)
  out <- cbind(final_score = m_score, out)
  out <- out[order(out$final_score, decreasing = T),]
  
  cat("I suggest ", out$game[1], "!\n")
  return(out)
}
