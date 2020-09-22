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
  # make a matrix to hold the samples
  #initially filled with NA's
  my.matrix=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()

  for( i in 1:iter){
    #Fill each column with a new sample
    my.matrix[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(my.matrix[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
