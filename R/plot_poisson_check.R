#' Poisson Model check
#'
#' Check if mean and variance of Poisson count model are roughly equivalent as described in Faraway (2006, p. 59).
#'
#' @param model A poisson model object fitted with \code{glm}
#' @param dispersion A logical (default FALSE); return estimated dispersion parameter?
#'
#' @return A plot of estimated variance versus fitted mean for a Poisson count model along with \code{abline(0,1)}. The estimated variance and fitted mean should have an approximate one-to-one relationship if the Poisson assumption is satisfied. (ie, they should plot closely to the diagonal line.) If dispersion = TRUE (the default), the estimated dispersion parameter is also returned, which can be used in the call to \code{summary} if necessary.
#' @importFrom stats fitted residuals
#' @importFrom graphics abline
#' @references Julian Faraway. (2016) \emph{Extending the Linear Model with R}. Chapman & Hall.
#' @export
#'
#' @examples
#' \dontrun{
#' library(faraway)
#' data(gala)
#' gala <- gala[,-2]
#' modp <- glm(Species ~ ., data = gala, family = poisson)
#' plot_poisson_check(modp)
#' plot_poisson_check(modp, dispersion = TRUE)
#' dp <- plot_poisson_check(modp)
#' summary(modp, dispersion = dp)
#' }
plot_poisson_check <- function(model, dispersion = FALSE){
  if(model$family$family != "poisson") stop("Model not poisson regression model.")
  plot(log(fitted(model)), log((model$y - fitted(model))^2),
       xlab = expression(hat(mu)), ylab = expression((y-hat(mu))^2),
       mgp = c(2.5, 1, 0))
  abline(0,1)
  if(dispersion){
    dp <- sum(residuals(model, type = "pearson")^2/model$df.residual)
    return(c("Estimated dispersion parameter" = dp))}
  }



