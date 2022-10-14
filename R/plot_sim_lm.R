#' Simulate and plot data from a lm model
#'
#' @param mod Model fitted with \code{lm}
#' @param nsim Number of simulations for \code{simulate} function; default is 50
#' @param col color of simulated smooth density lines; default is "grey80". See \code{colors()} for list of available colors.
#'
#' @return A plot of simulated smooth densities from the model overlayed on a smooth density plot of the original data.
#' @importFrom stats simulate density
#' @importFrom graphics lines
#' @export
#'
#' @examples
#' m <- lm(dist ~ speed, data = cars)
#' plot_sim_lm(m)
plot_sim_lm <- function(mod, nsim = 50, col = "grey80"){
  sim <- simulate(mod, nsim = nsim)
  d <- lapply(seq(nsim), function(x)density(sim[[x]]))
  ymax <- max(sapply(d, function(x)max(x$y)))
  plot(density(mod$model[,1]), ylim = c(0, ymax))
  for(i in seq(nsim))lines(density(sim[[i]]), col = col)
}

