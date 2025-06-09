context("TOP_model and related functions")

test_that("TOP_model returns expected structure", {
  data(TOP_data_binary, package = "TOP")
  x_list <- list(TOP_data_binary$x1, TOP_data_binary$x2)
  y_list <- list(factor(TOP_data_binary$y1), factor(TOP_data_binary$y2))
  set.seed(23)
  model <- TOP_model(x_list, y_list)
  expect_true(is.list(model))
  expect_true(all(c("models", "feature") %in% names(model)))
  expect_s3_class(model$models, "cv.glmnet")
  expect_type(model$feature, "double")
})

test_that("predict_TOP returns valid predictions", {
  data(TOP_data_binary, package = "TOP")
  x_list <- list(TOP_data_binary$x1, TOP_data_binary$x2)
  y_list <- list(factor(TOP_data_binary$y1), factor(TOP_data_binary$y2))
  set.seed(23)
  model <- TOP_model(x_list, y_list)
  preds <- predict_TOP(model$models, newx = TOP_data_binary$x3)
  expect_type(preds, "double")
  expect_equal(length(preds), nrow(TOP_data_binary$x3))
  expect_true(all(preds >= 0 & preds <= 1))
})

test_that("filterFeatures returns requested number of features", {
  data(TOP_data_binary, package = "TOP")
  x_list <- list(TOP_data_binary$x1, TOP_data_binary$x2, TOP_data_binary$x3)
  y_list <- list(TOP_data_binary$y1, TOP_data_binary$y2, TOP_data_binary$y3)
  y_list <- lapply(y_list, function(x) {
    factor(x, levels = c("1", "0"), labels = c("Yes", "No"))
  })
  features <- filterFeatures(x_list, y_list, contrast = "Yes - No", nFeatures = 10)
  expect_equal(length(features), 10)
})

test_that("performance_TOP returns tibble of metrics", {
  data(TOP_data_binary, package = "TOP")
  x_list <- list(TOP_data_binary$x1, TOP_data_binary$x2)
  y_list <- list(factor(TOP_data_binary$y1), factor(TOP_data_binary$y2))
  set.seed(23)
  model <- TOP_model(x_list, y_list)
  perf <- performance_TOP(model$models, newx = TOP_data_binary$x3, newy = factor(TOP_data_binary$y3))
  expect_s3_class(perf, "data.frame")
  expect_true(all(c("Evaluation", "Value") %in% names(perf)))
})
