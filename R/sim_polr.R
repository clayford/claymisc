#' Simulate and plot data from a polr model
#'
#' @param mod a \code{polr} model; weighted fits are not supported
#' @param n number of simulations; default is 50
#' @param alpha transparency setting for simulated data points; default is 0.25. Ranges from 0 to 1.
#' @param width the jitter width; default is 0.1
#'
#' @return A plot of simulated proportions along with the observed proportions of the ordered response variable from the \code{polr} model.
#' @importFrom stats as.formula simulate xtabs
#' @importFrom insight find_response
#' @importFrom MASS polr
#' @export
#'
#' @examples
#' \dontrun{
#' library(carData)
#' m <- MASS::polr(poverty ~ gender + religion + degree, data=WVS)
#' sim_polr(m)
#' }
sim_polr <- function(mod, n = 50, alpha = 0.15, width = 0.1){
  # get response variable
  dv <- insight::find_response(mod)
  # observed proportions of response
  obs_p <- xtabs(as.formula(paste("~", dv)), mod$model) |>
    proportions() |>
    as.data.frame(responseName = "P")
  # function to get proportions of simulated responses
  p_df <- function(x){
    xtabs(~ x) |>
      proportions() |>
      as.data.frame(responseName = "P")
  }
  s <- simulate(mod, nsim = n)
  s_df <- lapply(s, p_df) |>
    do.call(what = "rbind",args = _)
  ggplot2::ggplot() +
    ggplot2::geom_point(mapping = ggplot2::aes_string(x = dv, y = "P"),
                        data = obs_p, size = 3, color = "blue") +
    ggplot2::geom_jitter(mapping = ggplot2::aes_string(x = "x", y = "P"),
                         data = s_df, alpha = alpha,
                         width = width, height = 0)
}

