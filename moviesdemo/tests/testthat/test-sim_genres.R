test_that("Output sim_genres measures", {
  expect_type(sim_genres(movies_DB$title[1], movies_DB$title[2]), "double")
  expect_equal(sim_genres(movies_DB$title[1], movies_DB$title[1]), 1)
  expect_error(sim_genres(NA, movies_DB$title[1]))
})
