
#' Confidence Interval Function for One Sample
#'
#' Takes a sample of values and returns a 95% confidence (default) interval of the mean of the sample.
#'
#' @param x
#' @param alpha
#' @param ...
#'
#' @return 95% confidence interval, ci of the sample
#' @export
#'
#' @examples
#' x = rnorm(30,mean = 10,sd=12)
#' myci(x)
#'
myci = function(x, conf= 0.95,...){
  a = 1-((100-conf)/2)
  n = length(x)
  t=qt(a,(n-1))
  ci=c()
  ci[1]=mean(x)-t*sd(x)/sqrt(n)
  ci[2]=mean(x)+t*sd(x)/sqrt(n)
  ci
  t.test(x,conf.level=(conf))$conf.int
}
