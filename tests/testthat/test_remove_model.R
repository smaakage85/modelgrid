library(magrittr)
library(caret)

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
