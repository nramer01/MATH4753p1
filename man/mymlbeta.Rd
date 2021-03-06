% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mymlbeta.R
\name{mymlbeta}
\alias{mymlbeta}
\title{Beta Distribution Maximum Liklihood Function}
\usage{
mymlbeta(x, alpha, beta, ...)
}
\arguments{
\item{...}{}
}
\value{

}
\description{
This function takes the parameters alpha and beta and applies them over the given vector (x) via a matriz
using the outer function. In return, the function releases a graphical interpretation of the maximum likely
points for both alpha and beta.
}
\examples{
x = rbeta(100,shape1=2,shape2=5)
alpha = seq(1,4,length=100)
beta = seq(2,8,length=100)
mymlbeta(x,alpha,beta)

}
