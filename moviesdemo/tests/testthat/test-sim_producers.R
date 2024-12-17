test_that("Output sim_producers measures", {
  expect_type(sim_producers(movies_DB$title[1], movies_DB$title[2]), "double")
  expect_equal(sim_producers(movies_DB$title[1], movies_DB$title[1]), 1)
  expect_error(sim_producers(NA, movies_DB$title[1]))
})
