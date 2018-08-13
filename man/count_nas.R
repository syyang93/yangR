#' Counts the number of NAs in a vector
#' 
#' @param to_count vector to count NAs for
#' 
#' @export
#' 
#' @return returns number of nas
#' 
#' @examples
#' count_nas(with.pheno.noblue$center)

count_nas <- function(to_count)
{
  print(length(which(is.na(to_count) == T)))
}
