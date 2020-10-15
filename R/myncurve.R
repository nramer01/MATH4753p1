#' Normal Curve Function
#'
#' Creates a normal curve, the curve with the area shaded based upon a probability using ggplot
#' and the area.
#'
#' @param mu
#' @param sigma
#' @param a
#'
#' @return a normal curve, a shaded normal curve within the area of the probability, and the area
#' @export
#'
#' @examples
#' mu = 10
#' sigma = 5
#' a = 6
#' myncurve(10,5,6)
#'
#'
myncurve = function(mu, sigma, a){
  library(ggplot2)
  normcurve <-curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  normargs <- list(mean = mu, sd = sigma)

  n <- ggplot(data.frame(x = c(mu-3*sigma, mu + 3*sigma)), aes(x)) +
    stat_function(fun = dnorm, args = normargs , geom = "area", col = "black")


  n <- n + stat_function(fun = dnorm, args = normargs, geom = "area", fill = "green", alpha =
                           2,xlim = c(mu-3*sigma,a))

  n <- n + xlab("X") + ylab("Density")
  paste(normcurve)
  print(n)
  b <- pnorm(a,mu,sigma)
  paste("The area is", b)
}

