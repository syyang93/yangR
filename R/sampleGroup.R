#' Function for sampling by group, taken from stackexchange
#' 
#' @param df the df to sample from
#' @param size number of samples
#' 
#' @export
#' 
#' @examples
#' Will sample 2 observations per species in the iris dataframe
#' iris.sample<-ddply(iris,.(Species),function(df) sampleGroup(df,2))

sampleGroup<-function(df,size) {
  df[sample(nrow(df),size=size),]
}