#' title
#'
#' description
#'
#' @param keywords bla
#' @param how_many bla
#'
#' @return list
#'
#' @examples
#' advise_movie_k(keywords = "christmas", how_many = 2)
#'
#' @export
advise_movie_k <- function(keywords, how_many){

  movies <- moviesdemo::movies_DB

  if(!isTRUE(is.numeric(how_many) & (length(how_many) == 1))) {
    stop("Argument 'how_many' should be a number...")
  }

  l <- lapply(keywords, function(x) grep(pattern = x, x = movies$keywords))
  ids <- Reduce(intersect, l)

  adv <- list()
  if (length(ids)>0){
    id <- sample(ids, size = min(length(ids), how_many))
    df <- movies[id,c("title","plot", "keywords")]

    # adv <- list(movie_title = df$title, plot = df$plot, keywords = df$keywords)

    adv <- new("advice", movie_title = df$title,
               plot = df$plot,
               keywords = df$keywords)
  }

  return(adv)
}
