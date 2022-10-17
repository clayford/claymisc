#' Multivariate Multiple Regression model confidence ellipse
#'
#' Plot ellipse to visualize the uncertainty in predictions for two responses from a Multivariate Multiple Regression model
#'
#' @param mod A Multivariate Multiple Regression model fitted with \code{lm}
#' @param newdata works the same as the newdata argument for predict
#' @param level confidence level between 0 and 1. The default is 0.95
#' @param ggplot logical. Create plot using ggplot2? Default is TRUE
#'
#' @return plot of confidence ellipse for two response variables from a Multivariate Multiple Regression model
#' @importFrom graphics points
#' @importFrom stats delete.response model.frame model.matrix na.pass predict qf resid terms
#' @references Johnson, R and Wichern, D (2007). \emph{Applied Multivariate Statistical Analysis, Sixth Edition.} Prentice-Hall.
#' @export
#'
#' @examples
#' \dontrun{
#' # From: https://data.library.virginia.edu/getting-started-with-multivariate-multiple-regression/
#' ami_data <- read.table("http://static.lib.virginia.edu/statlab/materials/data/ami_data.DAT")
#' names(ami_data) <- c("TOT","AMI","GEN","AMT","PR","DIAP","QRS")
#' mlm1 <- lm(cbind(TOT, AMI) ~ GEN + AMT + PR + DIAP + QRS, data = ami_data)
#' mlm2 <- update(mlm1, . ~ . - PR - DIAP - QRS)
#' plot_pred_ellipse(mod = mlm2, newdata = data.frame(GEN = 1, AMT = 1200))}
plot_pred_ellipse <- function(mod, newdata, level = 0.95, ggplot = TRUE){
  # labels
  lev_lbl <- paste0(level * 100, "%")
  resps <- colnames(mod$coefficients)
  title <- paste(lev_lbl, "confidence ellipse for", resps[1], "and", resps[2])

  # prediction
  p <- predict(mod, newdata)

  # center of ellipse
  cent <- c(p[1,1],p[1,2])

  # shape of ellipse
  Z <- model.matrix(mod)
  Y <- mod$model[[1]]
  n <- nrow(Y)
  m <- ncol(Y)
  r <- ncol(Z) - 1
  S <- crossprod(resid(mod))/(n-r-1)

  # radius of circle generating the ellipse
  # see Johnson and Wichern (2007), p. 399
  tt <- terms(mod)
  Terms <- delete.response(tt)
  mf <- model.frame(Terms, newdata, na.action = na.pass,
                    xlev = mod$xlevels)
  z0 <- model.matrix(Terms, mf, contrasts.arg = mod$contrasts)
  rad <- sqrt((m*(n-r-1)/(n-r-m)) * qf(level,m,n-r-m) *
                z0 %*% solve(t(Z)%*%Z) %*% t(z0))

  # generate ellipse using ellipse function in car package
  ell_points <- car::ellipse(center = c(cent), shape = S,
                             radius = c(rad), draw = FALSE)

  # ggplot2 plot
  if(ggplot){
    ell_points_df <- as.data.frame(ell_points)
    ggplot2::ggplot(ell_points_df, ggplot2::aes_string("x", "y")) +
      ggplot2::geom_path() +
      ggplot2::geom_point(ggplot2::aes_string(x = "TOT", y = "AMI"),
                          data = data.frame(p)) +
      ggplot2::labs(x = resps[1], y = resps[2],
           title = title)
  } else {
    # base R plot
    plot(ell_points, type = "l",
         xlab = resps[1], ylab = resps[2],
         main = title)
    points(x = cent[1], y = cent[2])
  }
}
