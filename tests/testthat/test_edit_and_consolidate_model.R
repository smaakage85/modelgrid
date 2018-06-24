library(magrittr)
library(caret)

context("Edits and consolidation of models")

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
