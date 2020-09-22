#' Binomial Experiment Simulation
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
  barplot(S.tab/(iter), col=cm.colors(n+1), main="Binomial simulation", xlab="Number of successes")
  S.tab/iter
}
