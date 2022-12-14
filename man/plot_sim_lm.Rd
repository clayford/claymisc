% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_sim_lm.R
\name{plot_sim_lm}
\alias{plot_sim_lm}
\title{Simulate and plot data from a lm model}
\usage{
plot_sim_lm(mod, nsim = 50, col = "grey80")
}
\arguments{
\item{mod}{Model fitted with \code{lm}}

\item{nsim}{Number of simulations for \code{simulate} function; default is 50}

\item{col}{color of simulated smooth density lines; default is "grey80". See \code{colors()} for list of available colors.}
}
\value{
A plot of simulated smooth densities from the model overlayed on a smooth density plot of the original data.
}
\description{
Simulate and plot data from a lm model
}
\examples{
m <- lm(dist ~ speed, data = cars)
plot_sim_lm(m)
}
