#' Confidence Intervals for Means
#'
#' Provides a formula interface for calculating means and associated confidence intervals within groups. Intended to return a data frame suitable for plotting or reporting.
#'
#' @param formula a two-sided formula of the form \code{variable ~ factor.1 + factor.2 + ...}
#' @param data a required data frame within which to find the variable and factor(s).
#' @param fun a function for calculating CIs. Default is \code{Hmisc::smean.cl.normal}
#' @param ... arguments to pass to the function given in the fun argument, such as \code{conf.int = 0.90}
#'
#' @return A data frame with columns for means, lower, upper, and grouping variables.
#' @importFrom Hmisc smean.cl.normal smean.cl.boot
#' @export
#'
#' @examples
#' mean_conf_int(mpg ~ cyl, data = mtcars)
#' mean_conf_int(mpg ~ am, data = mtcars)
#' mean_conf_int(mpg ~ cyl + am, data = mtcars)
#' mean_conf_int(mpg ~ cyl + am, data = mtcars, conf.int=.90)
#' mean_conf_int(mpg ~ cyl + am, data = mtcars,
#'               fun = Hmisc::smean.cl.boot, conf.int=.90)
#'
mean_conf_int <- function(formula, data, fun = Hmisc::smean.cl.normal, ...){
  mf <- model.frame(formula, data)
  targs <- list()
  targs[c("X", "INDEX", "FUN")] <- list(mf[, 1], mf[, -1],
                                        fun)
  targs <- c(targs, list(...))
  ci_grp <- do.call(tapply, targs)
  d <- as.data.frame(do.call(rbind, ci_grp))
  if(is.data.frame(mf[,-1])){
    nms <- attr(ci_grp, which = "dimnames")
    grp <- expand.grid(nms)
  } else {
    var <- all.vars(formula)[2]
    grp <- as.data.frame(names(ci_grp))
    colnames(grp) <- var
  }
  d <- cbind(d, grp)
  d
}
