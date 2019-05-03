#' Function that will find outliers in a vector (default: > 3 SD from mean), and return their indices
#' 
#' @param x A numeric vector
#' 
#' @export
#' 
#' @return The indices of the outliers
#' 
#' @examples
#' find.outliers(mtcars$mpg)

find.outliers <- function(x, sd = 3){
  m <- mean(x, na.rm = T)
  s <- sd(x, na.rm = T)
  up.out <- which(x > (m + sd*s))
  low.out <- which(x < (m - sd*s))
  all.out <- union(up.out, low.out)
  return(all.out)
}