#' Performs rank-order analysis on qPCR triplicates.  
#' Interpretation: significant pipetting order effect = 1st replicate pipetted is enriched for largest (max) deltaCT value. And 3rd replicate pipetted is enriched for smallest (min) deltaCT value

#' 
#' @param data.deltaCT a data frame that contains deltaCT information. 
#' @param removed a file path that you would like to write removed samples to
#' @param platename the name of the plate that you are working with
#' 
#' @export
#' 
#' @return Returns results of the rank-order analysis
#' 
#' @examples
#' 	ranks <- getRanks(mydata_deltaCT)


getMeanSD <- function(data.deltaCT, removed, platename){
  samplelist=levels(as.factor(data.deltaCT$Sample.Name))
  only.one.left <- data.frame()
  data.by.sample=data.frame(matrix(NA,ncol=3))
  names(data.by.sample)=c("Sample.Name","Mean.deltaCT","SD.deltaCT")
  for (i in 1:length(samplelist)){
    sample.test=samplelist[i]
    # if there's only one replicate that has a deltaCT value, flag the sample as NA
    if (nrow(na.omit(subset(data.deltaCT, Sample.Name==sample.test)))==1){
      only.one.left <- rbind(only.one.left, subset(data.deltaCT, Sample.Name==sample.test))
      mean.test="NaN"
      sd.test="NaN"
    } else {
      mean.test=mean(as.numeric(na.omit(subset(data.deltaCT, Sample.Name==sample.test)$deltaCT)))
      sd.test=sd(as.numeric(na.omit(subset(data.deltaCT, Sample.Name==sample.test)$deltaCT)))
    }
    data.by.sample[i,]=c(sample.test,mean.test,sd.test)
  }
  if(nrow(only.one.left) != 0){write.table(only.one.left, file = paste0(removed, 'only_one_left_', platename), quote = F, sep = '\t')}
  return(data.by.sample)
}
