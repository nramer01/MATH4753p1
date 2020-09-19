#' Total Sum of Squares
#'
#' The total sum of squares function takes a variable of read in data and gives the sum of the square deviation
#' scores from the mean variable. It is the error calculation of the no predictor model.
#'
#' @param x
#'
#' @return The total sum of squares value
#' @export
#'
#' @examples
#' x = (1:20)
#' TSS(x)
TSS = function(x) {
   sum((x - mean(x))^2)
}
