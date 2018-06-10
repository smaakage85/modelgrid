context("model grid tests")

test_that("Check if output is correct", {

  expect_is(model_grid(), "model_grid")

}
)

test_that("Addition of model configuration(s) to model grid", {

  # automatic naming
  expect_equal(c("Model0", "Model1"),
               model_grid() %>%
                 add_model(method = "rf") %>%
                 add_model(method = "nnet") %>%
                 magrittr::extract2("models") %>%
                 names(.))

  # bogus model
  expect_error(model_grid() %>%
                 add_model(method = "bogus"))

}
)

test_that("Editing of models within a model grid", {

  # check simple modification
  expect_equal(10,
               model_grid() %>%
                 add_model(method = "rf", tuneLength = 5) %>%
                 edit_model(model_name = "Model0", tuneLength = 10) %>%
                 magrittr::extract2(c("models", "Model0", "tuneLength")))
}
)

