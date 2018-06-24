library(caret)
library(recipes)
library(magrittr)

context("Type tests")

test_that("model_grid inherits from correct class", {
  expect_is(model_grid(), "model_grid")
  })

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


context("Editing models in model grid")

test_that("simple model edits are as expected", {
  expect_equal(10,
               model_grid() %>%
                 add_model(method = "rf", tuneLength = 5) %>%
                 edit_model(model_name = "Model0", tuneLength = 10) %>%
                 extract2(c("models", "Model0", "tuneLength")))
  })

context("Customizations to trControl")

test_that("Customizations to shared trControl are as expected", {
  # set up model grid.
  mg <-
     model_grid() %>%
    share_settings(trControl = trainControl(method = "cv")) %>%
    add_model("tester", method = "rf", custom_control = list(method = "none"))

  # customizations to trControl using add_model().
  expect_equal("none",
               # consolidate model.
               consolidate_model(mg$shared_settings, mg$models$tester) %>%
                 # extract trainControl resampling "method".
                 extract2(c("trControl", "method")))

  # edit existing model.
  mg <-
    mg %>%
    edit_model("tester", custom_control = list(method = "repeatedcv"))

  # customizations to trControl using edit_model().
  expect_equal("repeatedcv",
               # consolidate model.
               consolidate_model(mg$shared_settings, mg$models$tester) %>%
                 # extract trainControl resampling "method".
                 extract2(c("trControl", "method")))
    })

test_that("Customizations to shared_settings -> trnControl -> preProcOptions are as expected", {

  # set up model grid.
  mg <-
    model_grid() %>%
    share_settings(trControl = trainControl(),
                              metric = "ROC") %>%
    add_model("Dummy", method = "nb") %>%
    add_model("Tester", custom_control = list(preProcOptions = list(ICAcomp = 999)))

  expect_equal(999, {
    # consolidate model.
    consolidate_model(mg$shared_settings, mg$models$Tester) %>%
      # extract trainControl resampling "method".
      extract2(c("trControl", "preProcOptions", "ICAcomp"))
    })

  expect_equal(123, {
    mg <-
      edit_model(mg, "Dummy", custom_control = list(preProcOptions = list(thresh = 123)))

    # consolidate model.
    consolidate_model(mg$shared_settings, mg$models$Dummy) %>%
      # extract trainControl resampling "method".
      extract2(c("trControl", "preProcOptions", "thresh"))
    })

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
      model_grid() %>%
      share_settings(
        y = df$a,
        x = df[c("b","c","d", "e")],
        trControl = trainControl(),
        preProc = c("center", "scale", "pca")
        ) %>%
      add_model(
        "LinReg",
        method = "glm") %>%
    train(.)

    is.numeric(mg$model_fits$LinReg$finalModel$coefficients)
    })

  # train with 'formula' input.
  expect_true({
    # create model grid.
    mg <-
      model_grid() %>%
      share_settings(
        data = df,
        trControl = trainControl()
      ) %>%
      add_model(
        "LinReg",
        form = a ~ .,
        preProc = c("center", "scale"),
        method = "glm") %>%
      train(.)

    is.numeric(mg$model_fits$LinReg$finalModel$coefficients)
    })

  # train with 'recipe' input
  expect_true({

    df <- matrix(rnorm(500), ncol = 5) %>% as.data.frame(.)
    names(df) <- letters[1:ncol(df)]

    # create recipe
    rec <-
      recipe(x = df, formula = a ~ .) %>%
      step_center(all_predictors()) %>%
      step_scale(all_predictors())

    mg <-
      model_grid() %>%
      share_settings(
        data = df,
        trControl = trainControl()
      ) %>%
      add_model(
        x = rec,
        "LinReg",
        method = "glm") %>%
      train(.)

    is.numeric(mg$model_fits$LinReg$finalModel$coefficients)})

})

test_that("training of nonsense model returns expected output", {

  # simulate data set.
  df <- matrix(rnorm(500), ncol = 5) %>% as.data.frame(.)
  names(df) <- letters[1:ncol(df)]

  # train nonsense model and check that warning is thrown.
  expect_true({
    suppressWarnings(
    # create model grid.
    mg <-
      model_grid() %>%
      share_settings(
        y = df$a,
        x = df[c("b","c","d", "e")],
        trControl = trainControl(),
        preProc = c("center", "scale", "pca")
      ) %>%
      add_model(
        "LinReg",
        method = "glm",
        famliy = binomial(link = "logit")
      ) %>%
      train(.)
    )

    # test that output consists of 'result' and 'error' component.
    all(c(
    exists("result", mg$model_fits$LinReg),
    exists("error", mg$model_fits$LinReg)))
  })

})

context("Removal of models from model grid")

test_that("model specifications are removed successfully", {

  # model grid with only one model
  expect_equal(0, {
    mg <-
      model_grid() %>%
      add_model("test1", method = "rf") %>%
      remove_model("test1")

    length(mg$models)
  })

  # model grid with multiple models
  expect_equal("test2", {
    mg <-
      model_grid() %>%
      add_model("test1", method = "rf") %>%
      add_model("test2", method = "glm") %>%
      remove_model("test1")

    names(mg$models)
  })

})

test_that("fitted models are removed succesfully", {

  # simulate data set.
  df <- matrix(rnorm(500), ncol = 5) %>% as.data.frame(.)
  names(df) <- letters[1:ncol(df)]

  mg <-
    model_grid() %>%
    share_settings(
      y = df$a,
      x = df[c("b","c","d", "e")],
      trControl = trainControl(),
      preProc = c("center", "scale", "pca")
    ) %>%
    add_model(
      "LinReg1",
      method = "glm") %>%
    train(.)

  # model grid with only one model.
  expect_equal(0, {
    # remove model.
    mg %>%
      remove_model("LinReg1") %>%
      extract2(c("model_fits")) %>%
      length(.)})

  # model grid with multiple models.
  expect_equal("LinReg2", {
    # remove model.
    mg %>%
      add_model(
        "LinReg2",
        method = "glm",
        preProc = c("center", "scale")) %>%
      train(.) %>%
      remove_model("LinReg1") %>%
      extract2(c("model_fits")) %>%
      names(.)})

})
