#' @title An S4 class to represent a movie advice
#'
#' @name advice
#'
#' @rdname advice-class
#'
#' @param movie_title movie_title
#' @param plot plot
#' @param keywords keywords
#' @param object an object of class advice
#' @param value value to assign to the movie title
#' @param movie_title movie title
#'
#' @aliases advice advice-class class:advice
#' @aliases advice-method movie_title<- advice-method show,advice-method
#' @seealso \code{\link{advice}}
#' @import methods
#' @export advice

methods::setClass("advice", # define the S4 class
         slots = c( # describes the names and classes of the slots
           movie_title = "character",
           plot = "character",
           keywords = "character"
         ),
         prototype = list( # default values for each slot
           movie_title = NA_character_,
           plot = NA_character_,
           keywords = NA_character_
         )
)


# define a method for an existing S4 generic (show)
#' @describeIn advice-class show the advice
#' @export show
methods::setMethod("show", "advice", function(object) {
  nn <- length(object@movie_title)
  titles <- paste(object@movie_title, collapse = ", ")
  plots <- paste(paste(object@movie_title,object@plot,
                       sep = ": "),collapse = "\n")
  cat(methods::is(object)[[1]], " with ",
      nn," suggestion(s)","\n",
      "==========================","\n",
      "selected movie movie_title(s): ", titles, "\n",
      "==========================","\n",
      "plots:  ",plots, "\n",
      sep = "")
})


methods::setGeneric("movie_title", function(object) standardGeneric("movie_title")) # getter
#' @describeIn advice-class movie_title of the advice
methods::setMethod("movie_title", "advice", function(object) object@movie_title)


methods::setGeneric("movie_title<-", function(object, value) standardGeneric("movie_title<-"))
#' @describeIn advice-class movie_title of the advice
methods::setMethod("movie_title<-", "advice", function(object, value) {
  object@movie_title <- value
  methods::validObject(object)
  object
})

# movie_title(new_adv)
# movie_title(new_adv) <- "title2"

#' @rdname advice-class
#'
#' @examples
#' new_adv <- advice("title1")
#' new_adv
#' # advice(32) # not working (internal validation)
#' @returns An \code{\link{advice-class}}
#' @export
advice <- function(movie_title,
                   plot = NA_character_,
                   keywords = NA_character_) {

  methods::new("advice", movie_title = movie_title,
               plot = plot,
               keywords = keywords)
}




