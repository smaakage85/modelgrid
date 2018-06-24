context("Correct structure and type")

test_that("model_grid inherits from correct class", {
  expect_is(model_grid(), "model_grid")
})
