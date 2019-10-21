#' Takes the last n characters of a string.  Taken from: https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
#' 
#' @examples
#' substrRight('character', 3)


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
