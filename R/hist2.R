#' Histogram with density and rug
#'
#' True histogram with overlayed density and rug. Based on code from section 8.2 of an Introduction to R.
#'
#' @param x numeric vector
#' @param bw bandwidth setting for \code{density}. Default is "SJ". See \code{?bw.SJ} for details and other options. Can also be a numeric value specifying the smoothing bandwidth to be used
#' @param ... additional arguments passed to \code{hist}
#'
#' @return histogram with density and rug.
#' @importFrom stats density
#' @importFrom graphics lines hist rug
#' @export
#'
#' @examples
#' hist2(faithful$eruptions)
#' hist2(faithful$eruptions, breaks = 30, bw = 0.1)
hist2 <- function(x, bw = "SJ", ...){
  main <- deparse(substitute(x))
  ymax <- max(c(hist(x, plot = FALSE, ...)$density,
                density(x, bw = bw)$y))
  hist(x, prob=TRUE, col = NULL, main = main, ylim = c(0, ymax),...)
  lines(density(x, bw = bw))
  rug(x)
}
