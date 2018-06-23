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

test_that("error occurs, when model type is not supported by caret", {

  # single setting.
  expect_error(modelgrid::model_grid() %>%
                 modelgrid::add_model(method = "bogus"))

  # multiple settings.
  expect_error(modelgrid::model_grid() %>%
                 modelgrid::add_model(method = "bogus",
                                      tuneLength = 10))
  })

test_that("the automatic naming of models does not cause any trouble", {
  expect_equal(c("Model0", "Model1"),
               modelgrid::model_grid() %>%
                 modelgrid::add_model("Model1", method = "rf") %>%
                 modelgrid::add_model(method = "nnet") %>%
                 magrittr::extract2("models") %>%
                 names(.))
  })

test_that("model names must be unique", {
  expect_error(modelgrid::model_grid() %>%
                 modelgrid::add_model("m1") %>%
                 modelgrid::add_model("m2") %>%
                 modelgrid::add_model("m1"))
  })

test_that("setting 'custom_control' for an individual model, whilst 'trControl' has
          not (yet) been set as part of the 'shared_settings' of the model_grid
          throws a warning", {
  expect_warning(modelgrid::model_grid() %>%
                   modelgrid::share_settings(metric = "ROC") %>%
                   modelgrid::add_model(method = "rf", custom_control = list(method = "cv")))
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

test_that("Customizations to shared trControl are as expected", {
  # set up model grid.
  mg <-
    modelgrid:: model_grid() %>%
    modelgrid::share_settings(trControl = caret::trainControl(method = "cv")) %>%
    modelgrid::add_model("tester", method = "rf", custom_control = list(method = "none"))

    # customizations to trControl using add_model().
    expect_equal("none",
    # consolidate model.
    modelgrid::consolidate_model(mg$shared_settings, mg$models$tester) %>%
                  # extract trainControl resampling "method".
                  magrittr::extract2(c("trControl", "method"))
    )

    # edit existing model.
    mg <- mg %>% edit_model("tester", custom_control = list(method = "repeatedcv"))
    # customizations to trControl using edit_model().
    expect_equal("repeatedcv",
                  # consolidate model.
                  modelgrid::consolidate_model(mg$shared_settings, mg$models$tester) %>%
                  # extract trainControl resampling "method".
                  magrittr::extract2(c("trControl", "method"))
                 )
    })

test_that("Customizations to shared_settings -> trnControl -> preProcOptions are as expected", {
  # set up model grid.
  mg <-
    modelgrid::model_grid() %>%
    modelgrid::share_settings(trControl = caret::trainControl(),
                              metric = "ROC") %>%
    modelgrid::add_model("Dummy", method = "nb") %>%
    modelgrid::add_model("Tester", custom_control = list(preProcOptions = list(ICAcomp = 999)))

    expect_equal(999, {
      # consolidate model.
      modelgrid::consolidate_model(mg$shared_settings, mg$models$Tester) %>%
        # extract trainControl resampling "method".
        magrittr::extract2(c("trControl", "preProcOptions", "ICAcomp"))}
      )

    expect_equal(123, {
      mg <-
        edit_model(mg, "Dummy", custom_control = list(preProcOptions = list(thresh = 123)))

      # consolidate model.
      modelgrid::consolidate_model(mg$shared_settings, mg$models$Dummy) %>%
        # extract trainControl resampling "method".
        magrittr::extract2(c("trControl", "preProcOptions", "thresh"))}
    )

})

context("Training of models from model grid")

test_that("Training of simple models is successful", {

  # simulate data set.
  df <- matrix(rnorm(500), ncol = 5) %>% as.data.frame(.)
  names(df) <- letters[1:ncol(df)]

  # train with 'x' and 'y' inputs.
  expect_true({
    # create model grid.
    mg <-
      modelgrid::model_grid() %>%
      modelgrid::share_settings(
        y = df$a,
        x = df[c("b","c","d", "e")],
        trControl = caret::trainControl(),
        preProc = c("center", "scale", "pca")
        ) %>%
      add_model(
        "LinReg",
        method = "glm") %>%
    train(.)

    is.numeric(mg$model_fits$LinReg$finalModel$coefficients)}
    )

  # train with 'formula' input.
  expect_true({
    # create model grid.
    mg <-
      modelgrid::model_grid() %>%
      modelgrid::share_settings(
        data = df,
        trControl = caret::trainControl()
      ) %>%
      modelgrid::add_model(
        "LinReg",
        form = a ~ .,
        preProc = c("center", "scale"),
        method = "glm") %>%
      train(.)

    is.numeric(mg$model_fits$LinReg$finalModel$coefficients)}
  )

  # ! I don't have any idea, why the test below does not comply.
  # # train with 'recipe' input
  # expect_true({
  #
  #   df <- matrix(rnorm(500), ncol = 5) %>% as.data.frame(.)
  #   names(df) <- letters[1:ncol(df)]
  #
  #   # create recipe
  #   rec <-
  #     recipes::recipe(x = df, formula = a ~ .) %>%
  #     recipes::step_center(all_predictors()) %>%
  #     recipes::step_scale(all_predictors())
  #
  #   mg <-
  #     modelgrid::model_grid() %>%
  #     modelgrid::share_settings(
  #       data = df,
  #       trControl = caret::trainControl()
  #     ) %>%
  #     modelgrid::add_model(
  #       x = rec,
  #       "LinReg",
  #       method = "glm") %>%
  #     train(.)
  #
  #   is.numeric(mg$model_fits$LinReg$finalModel$coefficients)})

})

context("Removal of models from model grid")

test_that("model specifications are removed successfully", {

  # model grid with only one model
  expect_equal(0, {
    mg <-
    modelgrid::model_grid() %>%
    modelgrid::add_model("test1", method = "rf") %>%
    modelgrid::remove_model("test1")

    length(mg$models)
  })

  # model grid with multiple models
  expect_equal("test2", {
    mg <-
      modelgrid::model_grid() %>%
      modelgrid::add_model("test1", method = "rf") %>%
      modelgrid::add_model("test2", method = "glm") %>%
      modelgrid::remove_model("test1")

    names(mg$models)
  })

})

test_that("fitted models are removed succesfully", {

  # simulate data set.
  df <- matrix(rnorm(500), ncol = 5) %>% as.data.frame(.)
  names(df) <- letters[1:ncol(df)]

  mg <-
    modelgrid::model_grid() %>%
    modelgrid::share_settings(
      y = df$a,
      x = df[c("b","c","d", "e")],
      trControl = caret::trainControl(),
      preProc = c("center", "scale", "pca")
    ) %>%
    modelgrid::add_model(
      "LinReg1",
      method = "glm") %>%
    train(.)

  # model grid with only one model.
  expect_equal(0, {
    # remove model.
    mg %>%
      modelgrid::remove_model("LinReg1") %>%
      magrittr::extract2(c("model_fits")) %>%
      length(.)})

  # model grid with multiple models.
  expect_equal("LinReg2", {
    # remove model.
    mg %>%
      modelgrid::add_model(
        "LinReg2",
        method = "glm",
        preProc = c("center", "scale")) %>%
      train(.) %>%
      modelgrid::remove_model("LinReg1") %>%
      magrittr::extract2(c("model_fits")) %>%
      names(.)})

})
