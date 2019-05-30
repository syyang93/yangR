#' Function to make quintiles from a column within a dataframe --> taken from fashaR
#'
#' @param test data that needs to be divided into quintiles
#'
#' @export
#'
#' @return output Dataframe with qunitiles (categories and by number, 5 = highest quintile)
#'
#' @examples
#' test2=makequintiles(test$resid.mtDNA)

make_quintiles=function(test){
  quintiles= cut(test, breaks=quantile(test, probs=seq(0,1,by=0.2), na.rm=T))
  num.quintiles=quintiles
  levels(num.quintiles)=c("1", '2', '3', '4', '5')
  num.quintiles=factor(as.character(num.quintiles))
  test <- data.frame(quintiles = quintiles, num.quintiles = num.quintiles)
  return(test)
}

