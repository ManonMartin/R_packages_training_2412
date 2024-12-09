advise_movie <- function(similar_to, how_many, draw_scores = FALSE, ...){
  
  dots <- list(...)
  # weights = rep(1, 4)
  weights <- if(is.null(dots$weights)) rep(-1, 4) else dots$weights
  
  movies <- moviesdemo::movies_DB
  
  names(weights) <- c("genre", "popularity", "rating", "production company")
  movie <- match.arg(similar_to, movies$title)
  
  if(!isTRUE(is.numeric(how_many) & (length(how_many) == 1))) {
    stop("Argument 'how_many' should be a number...")
  }
  
  for(x in names(weights)){
    
    index <- which(names(weights) == x)
    while(weights[index] < 0){
      weight <- readline(paste0("From 0 to 5, how important is the ", x, "?"))
      weight <- (if(isTRUE((as.numeric(weight) %in% 0:5) & (length(weight) == 1))){
        as.numeric(weight) } else -1)
      if(weight < 0){
        message("Try again, please provide an integer between 0 and 5...")
      }
      weights[index] <- weight
    }
  }
  
  indices <- (1:dim(movies)[1])[-which(movies$title == movie)]
  
  # sim_genres
  genre <- purrr::map_dbl(indices, ~ sim_genres(movie, movies$title[.x]))
  genre <- c(scale(log(genre + 0.01)))
  
  pop <- c(scale(log(movies$popularity[indices] + 0.01)))
  
  rating <- c(scale(movies$vote[indices]))
  
  # sim_producers
  producer <- purrr::map_dbl(indices, ~ sim_producers(movie, movies$title[.x]))
  producer <- c(scale(log(producer + 0.01)))
  
  total_scores <- weights[1] * genre + (weights[2] / 4) * pop +
    (weights[3] / 4) * rating + (weights[4] / 2) * producer
  top_indices <- indices[order(total_scores, decreasing = T)[1:how_many]]
  
  scores <- sort(total_scores, decreasing = T)[1:how_many]
  to_watch <- movies$title[top_indices]
  
  to_watch_plots <- movies$plot[top_indices]
  to_watch_keywords <- movies$keywords[top_indices]
  
  if (draw_scores) {
    names(scores) <- to_watch
    m <- graphics::barplot(scores, ylim=c(0, ceiling(max(scores))), col = "blue", main = "Movie scores")
    text(m, scores * 0.9, labels = round(scores, 2), col = "white")
  }
  
  # adv <- list(movie_title = to_watch,
  #             plot = to_watch_plots,
  #             keywords = to_watch_keywords)
  
  adv <- new("advice", movie_title = to_watch,
             plot = to_watch_plots,
             keywords = to_watch_keywords)
  
  return(adv)
  
}
