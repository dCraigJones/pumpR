#' Nash-Sutcliffe Coefficient
#'
#' \tabular{ccl}{
#' NSE Range \tab Calibration Rating \tab Model Application \cr
#' 1.00 to 0.50 \tab Excellent \tab Planning, Prelim Design, Final Design \cr
#' 0.49 to 0.40 \tab Very Good \tab Planning, Prelim Design, Final Design \cr
#' 0.39 to 0.30 \tab Good \tab Planning, Prelim Design \cr
#' 0.29 to 0.20 \tab Fair \tab Planning \cr
#' 0.21 or less \tab Poor \tab Screening \cr
#' }
#'
#'
#' @param mod, vector of model data
#' @param obs, vector of observed data
#'
#' @return Nash-Sutcliffe Coefficient
#' @export
#'
#' @examples
#' NSE(1:10,1:10)
NSE <- function(mod, obs) {
  # 1.00 to 0.50  Excellent - Final Design
  # 0.49 to 0.40  Very Good - Final Design
  # 0.39 to 0.30  Good - Prelim Design
  # 0.29 to 0.20  Fair - Planning
  # 0.21 or less  Poor - Screening

  SE <- sum((obs-mod)^2)
  MSE <- SE/length(obs)
  sig <- sd(obs)
  sig2 <- sig^2

  NSE <- 1 - MSE/sig2

  return(NSE)
}


ISE <- function(mod, obs) {
  #  0.0 to  3.0  Excellent - Final Design
  #  3.1 to  6.0  Very Good - Final Design
  #  6.1 to 10.0  Good - Prelim Design
  # 10.1 to 25.0  Fair - Planning
  # 25.1 or more  Poor - Screening

  SE <- sum((obs-mod)^2)
  MSE <- SE/length(obs)
  S <- sum(obs)

  ISE <- sqrt(MSE)/S * 100

  return(ISE)
}
