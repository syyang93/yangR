#' Allows boxplots to output the number of observations for each factor.
#' 
#' @param x 
#' 
#' @export
#' 
#' @return 
#' 
#' @examples
#' p <- ggplot(all2, aes(x=Group2, y=corr_mito_D)) + stat_summary(fun.data = give.n, geom = "text") + geom_boxplot()
give.n <- function(x){
  return(c(y = min(x)-(sd(x)/2), label = length(x)))
}

