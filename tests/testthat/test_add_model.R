library(magrittr)

context("Adding models to model grid")

test_that("the addition of two model configuration(s) to model grid is as expected", {
  expect_equal(c("Model0", "Model1"),
               model_grid() %>%
                 add_model(method = "rf") %>%
                 add_model(method = "nnet") %>%
                 extract2("models") %>%
                 names(.))
})

test_that("error occurs, when model type is not supported by caret", {

  # single setting.
  expect_error(model_grid() %>%
                 add_model(method = "bogus"))

  # multiple settings.
  expect_error(model_grid() %>%
                 add_model(method = "bogus",
                           tuneLength = 10))
})

test_that("the automatic naming of models does not cause any trouble", {
  expect_equal(c("Model0", "Model1"),
               model_grid() %>%
                 add_model("Model1", method = "rf") %>%
                 add_model(method = "nnet") %>%
                 extract2("models") %>%
                 names(.))
})

test_that("model names must be unique", {
  expect_error(model_grid() %>%
                 add_model("m1") %>%
                 add_model("m2") %>%
                 add_model("m1"))
})

test_that("setting 'custom_control' for an individual model, whilst 'trControl' has
          not (yet) been set as part of the 'shared_settings' of the model_grid
          throws a warning", {
            expect_warning(model_grid() %>%
                             share_settings(metric = "ROC") %>%
                             add_model(method = "rf", custom_control = list(method = "cv")))
            })


