#' An S4 class to represent a movie advice
#'
#' @slot movie_title movie_title of the advised movie
#' @slot plot plot of the advised movie
#' @slot keywords keywords of the advised movie
#'
#' @import methods


# define the S4 class
advice <- methods::setClass("advice",
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

# helper (user friendly) to create a new advice
advice <- function(movie_title,
                   plot = NA_character_,
                   keywords = NA_character_) {

  methods::new("advice", movie_title = movie_title,
      plot = plot,
      keywords = keywords)
}

# new_adv <- advice("test")
# new_adv

# advice(32) # not working (internal validation)

# getter
methods::setGeneric("movie_title", function(x) standardGeneric("movie_title"))
methods::setMethod("movie_title", "advice", function(x) x@movie_title)

# setter
methods::setGeneric("movie_title<-", function(x, value) standardGeneric("movie_title<-"))
methods::setMethod("movie_title<-", "advice", function(x, value) {
  x@movie_title <- value
  methods::validObject(x)
  x
})

# movie_title(new_adv)
# movie_title(new_adv) <- "test2"


