#' Shapes the ddPCR data into a better format for ggplot
#' 
#' @param x the vector you're looking for duplicates in
#' 
#' @export
#' 
#' @return Returns indices of all duplicates
#' 
#' @examples
#' dup_indices <- getAllDups(x)

getAllDups <- function(x) {
  top <- which(duplicated(x))
  bot <- which(duplicated(x, fromLast = T))
  both <- union(top, bot)
  dup.indices <- both[order(both)]
  return(dup.indices)
}
