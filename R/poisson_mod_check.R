#' Poisson Model check
#'
#' check if mean and variance of poisson count model are roughly equivalent;
#' Faraway (2006) p. 59
#'
#' @param model A poisson model object fitted with `glm()`
#' @param dispersion A logical (default TRUE); return estimated dispersion parameter?
#'
#' @return A plot of estimated variance versus mean for a fitted poisson count model. If dispersion = TRUE (the default), the estimated dispersion parameter is also returned.
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
#' poisson_mod_check(modp)
#' poisson_mod_check(modp, dispersion = FALSE)
#' dp <- poisson_mod_check(modp)
#' summary(modp, dispersion = dp)
#' }
poisson_mod_check <- function(model, dispersion = TRUE){
  if(model$family$family != "poisson") stop("Model not poisson regression model.")
  plot(log(fitted(model)), log((model$y - fitted(model))^2),
       xlab = expression(hat(mu)), ylab = expression((y-hat(mu))^2),
       mgp = c(2.5, 1, 0))
  abline(0,1)
  if(dispersion){
    dp <- sum(residuals(model, type = "pearson")^2/model$df.residual)
    return(c("Estimated dispersion parameter" = dp))}
  }



