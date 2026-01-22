#' T-test using Effective Sample Size for Data with Autocorrelation
#'
#' @description Uses the t-test with effective sample size to calculate test statistic, p-value, and confidence intervals for autocorrelated data.
#'
#' @param x A numeric vector.
#' @param y A numeric vector. Optional for two sample tests.
#' @param alternative A character string for alternative hypothesis. Can be either "two.sided", "greater", or "less". Default is "two.sided".
#' @param conf.level A numeric value. For confidence level of the interval.
#' @param method A character string for method to calculate the factor equation for the effective samples sizes. Only "basic" and "hsdda" are available.
#' @param warnings A logical value. Change to remove warnings.
#'
#' @returns A list class object (trying to figure out attributes to make my own class):
#' * `statistic`: the t-statistic.
#' * `parameter`: degrees of freedom.
#' * `p.value`: p-value.
#' * `conf.int`: confidence interval for the mean.
#' * `estimate`: the mean.
#' * `samplesize`: sample size.
#' * `stderr`: the standard error of the mean.
#' * `autocorr`: autocorrelation calculated through the auto-covariance and correlation function (ACF).
#' * `ess.factor`: the calculated factor for effective sample size, used as a weighting term for the sample size.
#' * `adj.statistic`: the t-statistic, adjusted using effective sample size.
#' * `adj.parameter`: degrees of freedom, adjusted using effective sample size.
#' * `adj.p.value`: p-value, adjusted using effective sample size.
#' * `adj.conf.int`: confidence interval for the mean, adjusted using effective sample size.
#'
#' @export
#'
#' @examples
#' x <- rnorm(10, 0, 5)
#' ess_test(x)

ess_test <- function(x, y = NULL, alternative = c("two.sided", "less", "greater"), conf.level = 0.95, method = c("basic", "hsdda"), warnings = TRUE) {

  jspr_warning("Only use function if you know your data has significant autocorrelation. You can check with stats::acf().", warnings)

  # house keeping
  if(identical(method, c("basic", "hsdda"))) {method <- "hsdda"}
  if(identical(alternative, c("two.sided", "less", "greater"))) {alternative <- "two.sided"}
  if(!is.null(y)) {stop("Two sample testing is not available yet.")}
  if(alternative == "two.sided") {
    alpha <- (1-conf.level)/2
  } else if(alternative == "less" | alternative == "greater") {
    stop("One-sided test is not yet usable.")
  }

  # initial calculations
  n <- length(x)
  autocorr <- stats::acf(x, plot = FALSE)[[1]][2]
  mean <- mean(x)
  var <- var(x)
  stderr <- sqrt(var/n)
  if(method == "basic") {
    ess_factor <- 1+(n-1)*autocorr
  } else if(method == "hsdda") {
    ess_factor <- (1+autocorr)/(1-autocorr)-(2/n)*autocorr*(1-autocorr^n)/(1-autocorr)^2
  } else {
    stop("ESS method must be 'basic' or 'hsdda'.")
  }

  # t test calculations
  test <- stats::t.test(x, conf.level = conf.level)
  test_stat <- test$statistic[[1]]
  test_df <- test$parameter[[1]]
  test_p <- test$p.value[[1]]
  test_lci <- test$conf.int[[1]]
  test_uci <- test$conf.int[[2]]


  # ess specific calculations
  ess_df <- n/ess_factor
  ess_statistic <- test_stat/sqrt(ess_factor)
  ess_p <- 2 * stats::pt(-abs(ess_statistic), df = ess_df)
  cint <- stats::qt(1 - alpha, df = ess_df)
  cint <- ess_statistic + c(-cint, cint)
  ess_conf.int <- cint * stderr * sqrt(ess_factor)

  # return values
  rval <- list(statistic = test_stat,
               parameter = test_df,
               p.value = test_p,
               conf.int = c(test_lci, test_uci),
               estimate = mean,
               samplesize = n,
               stderr = stderr,
               autocorr = autocorr,
               ess.factor = ess_factor,
               adj.statistic = ess_statistic,
               adj.parameter = ess_df,
               adj.p.value = ess_p,
               adj.conf.int = ess_conf.int)

  return(rval)

}
