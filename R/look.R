#' Function that gives you the first five columns and first five rows of a dataframe
#' 
#' @export
#' @param df The data frame you wanna look at
#' 
#' @examples
#' look(df)

look <- function(df){
  df[1:5, 1:5]
}