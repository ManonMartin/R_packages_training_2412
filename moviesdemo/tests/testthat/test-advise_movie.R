test_that("Output advise_movie function", {
  out <- advise_movie(movies_DB$title[1], 5, weights = rep(1,4))
  expect_output(str(out),"with 3 slots")
  expect_match(out@movie_title[1], movies_DB$title[1606])
  expect_equal(length(out@movie_title), 5)
  expect_type(out@movie_title, "character")
  expect_error(advise_movie(movies_DB$title[1], NA),
               "Argument 'how_many' should be a number...")
})

