#' Construct a knowledge scale using IRT modeling.
#'
#' \code{info_scale} calculates a knowledge scale using Item Response Theory (IRT) modeling
#' on the basis of a set of binary knowledge items. The function uses the \code{mirt} package
#' for IRT modeling.
#'
#' @param items A concatenated character string of names of the knowledge items.
#' @param data A data frame containing a set of binary knowledge items (\code{1} = true, \code{0} = false).
#' @param binary_cutoff Percentile value for binary knowledge scale, with respondents
#'     at or above that value coded as \code{1} and otherwise as \code{0}. Set to 90th
#'     percentile by default.
#' @return \code{inf_scale} returns a list of the following:
#'     \itemize{
#'       \item \code{model} The IRT model
#'       \item \code{model_summary} A summary of the IRT model
#'       \item \code{model_coef} The coefficients for the IRT model
#'       \item \code{know_scores} A vector of knowledge scores
#'       \item \code{know_scores_binary} A vector of binary knowledge scores
#'       \item \code{know_scores_binary_tbl} A table with the proportions on the binary knowledge variable
#'       \item \code{par_analysis} The plot for a paralell analysis of the knowledge items, to evaluate dimensionality.
#'       \item \code{q3} The Q3 values for the IRT model, to investigate local independence
#'       \item \code{empirical_plots} Empirical plots for the IRT model, to evaluate model fit
#'    }
#' @details Further diagnostics can be accessed through the \code{model} object returned by the function:
#' \code{plot(model, type = "trace")} gives the trace plot and \code{plot(model, type = "info")} gives the
#' information plot.
#' @examples
#' item1 <- sample(c(0,1), replace=TRUE, size=100)
#' item2 <- sample(c(0,1), replace=TRUE, size=100)
#' item3 <- sample(c(0,1), replace=TRUE, size=100)
#' df <- data.frame(item1, item2, item3)
#' irt_model <- info_scale(items = c("item1", "item2","item3"),
#'                         data = df)
info_scale <- function(items, data, binary_cutoff = 0.9) {
  # save all knowledge items to a data frame
  items_df <- data.frame(matrix(NA, nrow = dim(data)[1], ncol = length(items)))
  for (i in 1:length(items)) {
    items_df[,i] <- data[[items[i]]]
  }

  # fit irt model
  irt_mod <- mirt::mirt(data=items_df,
                  model=1,
                  itemtype = "2PL",
                  verbose=FALSE)

  # save knowledge scores
  know_scores <- mirt::fscores(irt_mod)[,1]

  # create binary knowledge variable
  knowledge_threshold <- stats::quantile(know_scores, binary_cutoff)
  know_scores_binary <- ifelse(know_scores >= knowledge_threshold, 1, 0)
  know_scores_binary_tbl <- prop.table(table("Proportion of observations in each category:" = know_scores_binary))

  # save empirical plots to list
  plot_list_empirical <- vector('list', length(items))
  for (i in 1:length(items)) {
    plot_list_empirical[[i]] <- local({
      i <- i
      print(mirt::itemfit(irt_mod, empirical.plot = i))
    })
  }
  empirical_plots <- ggpubr::ggarrange(plotlist = plot_list_empirical)

  # scree plot
  psych::fa.parallel(items_df, fa="fa")

  return(list("model" = irt_mod,
              "model_coef" = mirt::coef(irt_mod, IRTpars=TRUE),
              "model_summary" = summary(irt_mod),
              "know_scores" = know_scores,
              "know_scores_binary" = know_scores_binary,
              "know_scores_binary_tbl" = know_scores_binary_tbl,
              "empirical_plots" = empirical_plots,
              "par_analysis" = grDevices::recordPlot(),
              "q3" = data.frame(mirt::residuals(irt_mod, type="Q3"))))
}

#' Evaluate construct validity using marginal means.
#'
#' \code{info_emmeans} estimates marginal mean levels of knowledge using the \code{emmeans} package for different demographic
#' variables in order to evaluate construct validity for the underlying knowledge scale.
#'
#' @param knowledge_var A character string representing a knowledge variable.
#' @param covariates A concatenated character string of covariates across which marginal means will be estimated.
#' @param data A data frame containing aforementioned variables.
#' @return \code{info_emmeans} returns a list of marginal means for the covariates in question.
#' @details For a plausible knowledge scale, we would expect to see a positive relationship between knowledge and education and
#' income, and for men to know more than women.
#' @seealso See \code{\link{info_scale}} for how to construct a knowledge scale.
#' @examples
#' knowledge <- rnorm(100,0,1)
#' education <- sample(c("none","gcse","alevel","university"), replace=TRUE, size=100)
#' income <- sample(c("Q1","Q2","Q3","Q4"), replace=TRUE, size=100)
#' df <- data.frame(knowledge, education, income)
#' info_emmeans(knowledge_var = "knowledge",
#'              covariates = c("education","income"),
#'              data = df)
info_emmeans <- function(knowledge_var, covariates, data) {
  # construct formula
  f <- stats::as.formula(
    paste(knowledge_var,
          paste(covariates, collapse = " + "),
          sep = " ~ "))

  # fit model
  m <- stats::lm(f,
          data = data)

  # create list of emmeans by each covariate
  emmeans_list <- list()
  for (i in 1:length(covariates)) {
    emmeans_list[[i]] <- emmeans::emmeans(m, specs = covariates[i])
  }

  return(emmeans_list)
}

#' Calculate propensity scores for counterfactual modeling.
#'
#' \code{info_prop_scores} calculates propensity scores to be used as weights in subsequent, counterfactual modeling,
#' in order to improve balance. The function uses logistic regression to calculate the scores. It is recommended that
#' the calculated scores are independently evaluated, e.g., using the balance plots in \code{cobalt::bal.plot}.
#'
#' @param knowledge_var A character string representing a binary knowledge variable.
#' @param covariates A concatenated character string of covariates that can be expected to have an effect on knowledge.
#' @param data A data frame containing aforementioned variables.
#' @return \code{info_prop_scores} returns a vector of propensity scores.
#' @details The function calculates the probability of an observation being found in the 'informed' category, represented by 1 on the
#' binary knowledge variable, as a function of a set of covariates. Call that probability \code{p}. The scores are then calculated as
#' the inverse of that probability, e.g., \code{1/p}, for anyone in the 'informed' category, and \code{1/(1-p)} otherwise.
#' @seealso See \code{\link{info_scale}} for how to construct a knowledge scale.
#' @examples
#' knowledge_binary <- sample(c(1,0), replace=TRUE, size=100)
#' education <- sample(c("none","gcse","alevel","university"), replace=TRUE, size=100)
#' income <- sample(c("Q1","Q2","Q3","Q4"), replace=TRUE, size=100)
#' df <- data.frame(knowledge_binary, education, income)
#' info_prop_scores(knowledge_var = "knowledge_binary",
#'                  covariates = c("education","income"),
#'                  data = df)
info_prop_scores <- function(knowledge_var, covariates, data) {
  # construct formula
  f <- stats::as.formula(
    paste(knowledge_var,
          paste(covariates, collapse = " + "),
          sep = " ~ "))

  # calculate propensity scores
  p_scores <- stats::glm(f,
                         data = data,
                         family = "binomial")
  data$ps_value <- stats::predict(p_scores, type="response")

  # return propensity scores
  return(ifelse(data[[knowledge_var]] == 1, 1/data$ps_value, 1/(1-data$ps_value)))
}

#' Calculate information effects.
#'
#' \code{info_effect} calculates information effects on the basis of survey data with a binary knowledge
#' variable, propensity scores, and survey weights, while controlling for a set of covariates. It can
#' also generate bootstrapped confidence intervals.
#'
#' @param outcome A character string representing the outcome variable that you are looking to estimate an effect on.
#' @param knowledge_var A character string representing a binary knowledge variable.
#' @param covariates A concatenated character string of covariates.
#' @param prop_weight A character string representing a vector containing propensity scores.
#' @param survey_weight A character string representing a vector containing survey weights. Defaults to \code{1} if not supplied, which gives equal weight to each observation.
#' @param boot_ci Designates whether to calculate bootstrapped confidence intervals. Logical.
#' @param data A data frame containing aforementioned variables.
#' @return \code{info_effect} returns a list of the following:
#'     \itemize{
#'       \item \code{model} The model used to estimate the information effect
#'       \item \code{actual_proportion} The actual, survey weighted proportion on the \code{outcome} variable
#'       \item \code{informed_proportion} The informed, survey weighted proportion on the \code{outcome} variable
#'       \item \code{difference} The difference between \code{actual_proportion} and the \code{informed_proportion}, i.e., the
#'       information effect.
#'       \item \code{actual_upr}, \code{actual_lwr}, \code{informed_upr}, \code{informed_lwr} The upper and lower bounds of the
#'       bootstrapped confidence intervals on \code{actual_proportion} and \code{informed_proportion}.
#'    }
#' @seealso See \code{\link{info_scale}} for how to construct a knowledge scale, \code{\link{info_emmeans}} for evaluating construct
#' validity for such a scale, and \code{\link{info_prop_scores}} for calculating propensity scores.
#' @examples
#' outcome_var <- sample(c(1,0), replace=TRUE, size=100)
#' knowledge_binary <- sample(c(1,0), replace=TRUE, size=100)
#' education <- sample(c("none","gcse","alevel","university"), replace=TRUE, size=100)
#' income <- sample(c("Q1","Q2","Q3","Q4"), replace=TRUE, size=100)
#' prop_scores <- abs(rnorm(100,0,1))
#' survey_wt <- abs(rnorm(100,0,1))
#' df <- data.frame(outcome_var, knowledge_binary, education, income, prop_scores, survey_wt)
#' info_effect(outcome = "outcome_var",
#'             knowledge_var = "knowledge_binary",
#'             covariates = c("education",
#'                            "income"),
#'             prop_weight = "prop_scores",
#'             survey_weight = "survey_wt",
#'             data = df,
#'             boot_ci = TRUE)
info_effect <- function(outcome, knowledge_var, covariates, prop_weight, survey_weight = 1, boot_ci = F, data) {
  # construct formula
  f <- stats::as.formula(
    paste(outcome,
          paste(knowledge_var,
                paste(covariates, collapse = " + "), sep = " + "),
          sep = " ~ "))

  # check for survey_weight
  if (survey_weight == 1) {
    survey_wt_vector <- rep(1, length(data[[outcome]]))
  } else {
    survey_wt_vector <- data[[survey_weight]]
  }

  # fit model
  m <- stats::glm(f,
                  data = data,
                  family = "binomial",
                  weights = data[[prop_weight]])

  # update formula
  m$call <- call('glm', formula = stats::formula(f), data = substitute(data))

  # make everyone in the data set informed
  data[[knowledge_var]] <- 1

  # calculate actual and informed support
  actual <- stats::weighted.mean(data[[outcome]], survey_wt_vector)
  informed_outcome <- stats::predict(m, newdata = data, type = "response")
  informed <- stats::weighted.mean(informed_outcome, survey_wt_vector)

  # generate bootstrap confidence intervals
  if (boot_ci == TRUE) {
    meanfun <- function(data, indices) {
      d <- data[indices]
      return(mean(d))
    }
    mean_wt <- mean(survey_wt_vector)
    boot_actual <- boot::boot(data[[outcome]] * survey_wt_vector, meanfun, R=1000)
    boot_informed <- boot::boot(informed_outcome * survey_wt_vector, meanfun, R=1000)
    actual_lwr <- boot::boot.ci(boot_actual, conf = 0.95, type = "basic")$basic[4]/mean_wt
    actual_upr <- boot::boot.ci(boot_actual, conf = 0.95, type = "basic")$basic[5]/mean_wt
    informed_lwr <- boot::boot.ci(boot_informed, conf = 0.95, type = "basic")$basic[4]/mean_wt
    informed_upr <- boot::boot.ci(boot_informed, conf = 0.95, type = "basic")$basic[5]/mean_wt
    return(list("model" = m,
                "actual_proportion" = actual,
                "actual_upr" = actual_upr,
                "actual_lwr" = actual_lwr,
                "informed_proportion" = informed,
                "informed_upr" = informed_upr,
                "informed_lwr" = informed_lwr,
                "difference" = informed - actual))
  }
  else {
    return(list("model" = m,
                "actual_proportion" = actual,
                "informed_proportion" = informed,
                "difference" = informed - actual))
  }
}
