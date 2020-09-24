#' Binomial Experiment Simulation
#'
#' The Binomial Experiment Simulation function comes from Lab 5. This function produces
#' a simulation of the Binomial experiment by placing random values of size n, iterated
#' iter times with a probability of p.
#'
#' @param iter
#' @param n
#' @param p
#'
#' @return The Bionomial Experiment plot, and n values
#' @export
#'
#' @examples
#' iter = 100
#' n = 5
#' p = 0.7
#' simbin(100,5,0.7)
#'
simbin=function(iter,n, p){
  my.matrix=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  S=c()
  for( i in 1:iter){
    my.matrix[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    S[i]=sum(my.matrix[,i])
  }
  S.tab=table(factor(S,levels=0:n))
  iter.lab = paste("iter = ", iter)
  n.lab = paste0("n = ", n)
  p.lab = paste0("p = ", p)
  lab = paste(iter.lab,n.lab,p.lab, sep = ", ")
  barplot(S.tab/(iter), col=cm.colors(n+1), main="Binomial simulation", sub = lab, xlab="Number of successes")
  S.tab/iter
}
