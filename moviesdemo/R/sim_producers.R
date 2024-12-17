# Movie similarity based on production companies
#
# The similarity of 2 movies A and B is measured based on their potential common production companies.
#
# parameters
################
# movie_A character, one movie title from the movie database
# movie_B character, another movie title from the movie database
#
# return
################
# A scalar : the similarity based on production companies
#

sim_producers <- function(movie_A, movie_B){

  movies <- moviesdemo::movies_DB

  movie_A <- match.arg(movie_A, movies$title)
  # movie_B <- match.arg(movie_B, movies$title)

  producers_A <- movies$producers[[which(movies$title == movie_A)]]
  producers_B <- movies$producers[[which(movies$title == movie_B)]]

  return(length(intersect(producers_A, producers_B)) / length(union(producers_A, producers_B)))
}
