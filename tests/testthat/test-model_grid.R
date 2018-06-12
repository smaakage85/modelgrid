context("Type tests")

test_that("model_grid inherits from correct class", {
  expect_is(modelgrid::model_grid(), "model_grid")
  })

context("Adding models to model grid")

test_that("the addition of two model configuration(s) to model grid is as expected", {
  expect_equal(c("Model0", "Model1"),
               modelgrid::model_grid() %>%
                 modelgrid::add_model(method = "rf") %>%
                 modelgrid::add_model(method = "nnet") %>%
                 magrittr::extract2("models") %>%
                 names(.))
  })

test_that("model grid fails, when model type is not supported by caret", {
  expect_error(modelgrid::model_grid() %>%
                 modelgrid::add_model(method = "bogus"))
  })

test_that("the automatic naming of models does not cause any trouble", {
  expect_equal(c("Model0", "Model1"),
               modelgrid::model_grid() %>%
                 modelgrid::add_model("Model1", method = "rf") %>%
                 modelgrid::add_model(method = "nnet") %>%
                 magrittr::extract2("models") %>%
                 names(.))
})

context("Editing models in model grid")

test_that("simple model edits are as expected", {
  expect_equal(10,
               modelgrid::model_grid() %>%
                 modelgrid::add_model(method = "rf", tuneLength = 5) %>%
                 modelgrid::edit_model(model_name = "Model0", tuneLength = 10) %>%
                 magrittr::extract2(c("models", "Model0", "tuneLength")))
  })

context("Customizations to trControl")

test_that("Customizations to trControl are as expected", {
  # set up model grid.
  mg <-
    modelgrid:: model_grid() %>%
    modelgrid::share_settings(trControl = caret::trainControl(method = "cv")) %>%
    modelgrid::add_model("tester", method = "rf", custom_control = list(method = "none"))

    # customizations to trControl using add_model().
    expect_equal("none",
    # consolidate model.
    modelgrid::consolidate_models(mg$shared_settings, mg$models) %>%
                  # extract trainControl resampling "method".
                  magrittr::extract2(c("tester", "trControl", "method"))
    )

    # edit existing model.
    mg <- mg %>% edit_model("tester", custom_control = list(method = "repeatedcv"))
    # customizations to trControl using edit_model().
    expect_equal("repeatedcv",
                  # consolidate model.
                  modelgrid::consolidate_models(mg$shared_settings, mg$models) %>%
                  # extract trainControl resampling "method".
                  magrittr::extract2(c("tester", "trControl", "method"))
                 )
    })






