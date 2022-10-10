test_that("IRT scores are calculated correctly", {
  set.seed(1)
  item1 <- sample(c(0,1), replace=TRUE, size=100)
  item2 <- sample(c(0,1), replace=TRUE, size=100)
  item3 <- sample(c(0,1), replace=TRUE, size=100)
  temp_df <- data.frame(item1, item2, item3)
  irt_model <- info_scale(c("item1",
                            "item2",
                            "item3"),
                          temp_df)
  expect_equal(irt_model$know_scores[1], -0.06664299, tolerance = 1e-5)
})
