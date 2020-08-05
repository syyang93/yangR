#' Function that gives you the first five columns and first five rows of a dataframe
#' 
#' @export
#' @param df The data frame you wanna look at
#' 
#' @examples
#' look(df)

look <- function(df){
  if(ncol(df) >= 5 & nrow(df) >= 5)
  {
  df[1:5, 1:5]
  } else if (nrow(df) >= 5) {
    df[1:5, 1:ncol(df)]
  } else if (ncol(df) >= 5) {
  	df[1:nrow(df), 1:5]
  	} else{df[1:nrow(df), 1:ncol(df)]}
}	