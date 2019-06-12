#' Function to make quartiles from a column within a dataframe --> taken from fashaR
#'
#' @param test data that needs to be divided into quartiles
#'
#' @export
#'
#' @return output Dataframe with quartiles (categories and by number, 4 = highest quartile)
#'
#' @examples
#' test2=makequartiles(test$resid.mtDNA)

make.quartiles=function(test){
  quartiles= cut(test, breaks=quantile(test, probs=seq(0,1,by=0.25)), na.rm=T, include.lowest = T)
  num.quartiles=quartiles
  levels(num.quartiles)=c("1", '2', '3', '4')
  num.quartiles=factor(as.character(num.quartiles))
  test <- data.frame(quartiles = quartiles, num.quartiles = num.quartiles)
  return(test$num.quartiles)
}

