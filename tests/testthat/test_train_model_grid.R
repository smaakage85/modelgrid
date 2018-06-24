library(caret)
library(recipes)
library(magrittr)

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

  # train with 'recipe' input.
  expect_true({

    df <- matrix(rnorm(500), ncol = 5) %>% as.data.frame(.)
    names(df) <- letters[1:ncol(df)]

    # create recipe.
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
