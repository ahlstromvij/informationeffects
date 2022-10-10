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

test_that("Marginal means are calculated correctly", {
  set.seed(1)
  knowledge <- rnorm(100,0,1)
  education <- sample(c("none","gcse","alevel","university"), replace=TRUE, size=100)
  income <- sample(c("Q1","Q2","Q3","Q4"), replace=TRUE, size=100)
  temp_df <- data.frame(knowledge, education, income)
  temp_means <- info_emmeans("knowledge",
                             c("education","income"),
                             temp_df)
  correct_vals_education <- c(0.42375170,0.08277421,-0.01245451,0.06541537)
  correct_vals_income <- c(0.17045887,0.12086070,0.22480204,0.04336515)
  expect_equal(summary(temp_means[[1]])$emmean, correct_vals_education, tolerance = 1e-5)
  expect_equal(summary(temp_means[[2]])$emmean, correct_vals_income, tolerance = 1e-5)
})

