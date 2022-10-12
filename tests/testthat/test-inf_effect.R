test_that("IRT scores are calculated correctly", {
  set.seed(1)
  item1 <- sample(c(0,1), replace=TRUE, size=100)
  item2 <- sample(c(0,1), replace=TRUE, size=100)
  item3 <- sample(c(0,1), replace=TRUE, size=100)
  df <- data.frame(item1, item2, item3)
  irt_model <- info_scale(c("item1",
                            "item2",
                            "item3"),
                          df)
  expect_equal(irt_model$know_scores[1], -0.06664299, tolerance = 1e-5)
})

test_that("Marginal means are calculated correctly", {
  set.seed(1)
  knowledge <- rnorm(100,0,1)
  education <- sample(c("none","gcse","alevel","university"), replace=TRUE, size=100)
  income <- sample(c("Q1","Q2","Q3","Q4"), replace=TRUE, size=100)
  df <- data.frame(knowledge, education, income)
  temp_means <- info_emmeans("knowledge",
                             c("education","income"),
                             df)
  correct_vals_education <- c(0.42375170,0.08277421,-0.01245451,0.06541537)
  correct_vals_income <- c(0.17045887,0.12086070,0.22480204,0.04336515)
  expect_equal(summary(temp_means[[1]])$emmean, correct_vals_education, tolerance = 1e-5)
  expect_equal(summary(temp_means[[2]])$emmean, correct_vals_income, tolerance = 1e-5)
})

test_that("Propensity scores are calculated correctly", {
  set.seed(1)
  knowledge_binary <- sample(c(1,0), replace=TRUE, size=100)
  education <- sample(c("none","gcse","alevel","university"), replace=TRUE, size=100)
  income <- sample(c("Q1","Q2","Q3","Q4"), replace=TRUE, size=100)
  df <- data.frame(knowledge_binary, education, income)
  prop_scores <- info_prop_scores(knowledge_var = "knowledge_binary",
                                  covariates = c("education","income"),
                                  data = df)
  expect_equal(prop_scores[1], 2.397574, tolerance = 1e-5)
})

test_that("Information effects are calculated correctly with CIs and survey weights", {
  set.seed(1)
  outcome_var <- sample(c(1,0), replace=TRUE, size=100)
  knowledge_binary <- sample(c(1,0), replace=TRUE, size=100)
  education <- sample(c("none","gcse","alevel","university"), replace=TRUE, size=100)
  income <- sample(c("Q1","Q2","Q3","Q4"), replace=TRUE, size=100)
  prop_scores <- abs(rnorm(100,0,1))
  survey_wt <- abs(rnorm(100,0,1))
  df <- data.frame(outcome_var, knowledge_binary, education, income, prop_scores, survey_wt)
  eff <- info_effect(outcome = "outcome_var",
                     knowledge_var = "knowledge_binary",
                     covariates = c("education",
                                    "income"),
                     prop_weight = "prop_scores",
                     survey_weight = "survey_wt",
                     data = df,
                     boot_ci = T)
  expect_equal(eff$actual_proportion, 0.5089763, tolerance = 1e-5)
  expect_equal(eff$actual_upr, 0.6577867, tolerance = 1e-5)
  expect_equal(eff$actual_lwr, 0.3531776, tolerance = 1e-5)
  expect_equal(eff$informed_proportion, 0.4903569, tolerance = 1e-5)
  expect_equal(eff$informed_upr, 0.5658839, tolerance = 1e-5)
  expect_equal(eff$informed_lwr, 0.412398, tolerance = 1e-5)
  expect_equal(eff$difference, -0.01861934, tolerance = 1e-5)
})

test_that("Information effects are calculated correctly with CIs and without survey weights", {
  set.seed(1)
  outcome_var <- sample(c(1,0), replace=TRUE, size=100)
  knowledge_binary <- sample(c(1,0), replace=TRUE, size=100)
  education <- sample(c("none","gcse","alevel","university"), replace=TRUE, size=100)
  income <- sample(c("Q1","Q2","Q3","Q4"), replace=TRUE, size=100)
  prop_scores <- abs(rnorm(100,0,1))
  df <- data.frame(outcome_var, knowledge_binary, education, income, prop_scores)
  eff <- info_effect(outcome = "outcome_var",
                     knowledge_var = "knowledge_binary",
                     covariates = c("education",
                                    "income"),
                     prop_weight = "prop_scores",
                     data = df,
                     boot_ci = T)
  expect_equal(eff$actual_proportion, 0.49, tolerance = 1e-5)
  expect_equal(eff$actual_upr, 0.59, tolerance = 1e-5)
  expect_equal(eff$actual_lwr, 0.39, tolerance = 1e-5)
  expect_equal(eff$informed_proportion, 0.5015549, tolerance = 1e-5)
  expect_equal(eff$informed_upr, 0.5315032, tolerance = 1e-5)
  expect_equal(eff$informed_lwr, 0.471042, tolerance = 1e-5)
  expect_equal(eff$difference, 0.01155488, tolerance = 1e-5)
})

test_that("Information effects are calculated correctly without CIs but with survey weights", {
  set.seed(1)
  outcome_var <- sample(c(1,0), replace=TRUE, size=100)
  knowledge_binary <- sample(c(1,0), replace=TRUE, size=100)
  education <- sample(c("none","gcse","alevel","university"), replace=TRUE, size=100)
  income <- sample(c("Q1","Q2","Q3","Q4"), replace=TRUE, size=100)
  prop_scores <- abs(rnorm(100,0,1))
  survey_wt <- abs(rnorm(100,0,1))
  df <- data.frame(outcome_var, knowledge_binary, education, income, prop_scores, survey_wt)
  eff <- info_effect(outcome = "outcome_var",
                     knowledge_var = "knowledge_binary",
                     covariates = c("education",
                                    "income"),
                     prop_weight = "prop_scores",
                     survey_weight = "survey_wt",
                     data = df)
  expect_equal(eff$actual_proportion, 0.5089763, tolerance = 1e-5)
  expect_equal(eff$informed_proportion, 0.4903569, tolerance = 1e-5)
  expect_equal(eff$difference, -0.01861934, tolerance = 1e-5)
})

test_that("Information effects are calculated correctly without CIs and survey weights", {
  set.seed(1)
  outcome_var <- sample(c(1,0), replace=TRUE, size=100)
  knowledge_binary <- sample(c(1,0), replace=TRUE, size=100)
  education <- sample(c("none","gcse","alevel","university"), replace=TRUE, size=100)
  income <- sample(c("Q1","Q2","Q3","Q4"), replace=TRUE, size=100)
  prop_scores <- abs(rnorm(100,0,1))
  survey_wt <- abs(rnorm(100,0,1))
  df <- data.frame(outcome_var, knowledge_binary, education, income, prop_scores)
  eff <- info_effect(outcome = "outcome_var",
                     knowledge_var = "knowledge_binary",
                     covariates = c("education",
                                    "income"),
                     prop_weight = "prop_scores",
                     data = df)
  expect_equal(eff$actual_proportion, 0.49, tolerance = 1e-5)
  expect_equal(eff$informed_proportion, 0.5015549, tolerance = 1e-5)
  expect_equal(eff$difference, 0.01155488, tolerance = 1e-5)
})

