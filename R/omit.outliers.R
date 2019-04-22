#' Function that will find outliers in a vector (default: > 3 SD from mean), set them to be NAs, and return the vector
#' 
#' @param x A numeric vector
#' 
#' @export
#' 
#' @return The same vector, with outliers changed to be NAs
#' 
#' @examples
#' omit.outliers(mtcars$mpg)

omit.outliers <- function(x, sd = 3){
  m <- mean(x)
  s <- sd(x)
  up.out <- which(x > (m + sd*s))
  low.out <- which(x < (m - sd*s))
  all.out <- union(up.out, low.out)
  x[all.out] <- NA
  return(x)
}