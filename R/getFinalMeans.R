#' Gets the final means after adjusting for pipetting order

#' @param data.deltaCT a data frame that contains deltaCT information. 
#' @param platename the name of the plate that you are working with
#' 
#' @export
#' 
#' @return Returns a data frame that has removed samples with deltaCT > 0.5 
#' 
#' @examples

getFinalMeans <- function(data.deltaCT.PO.clean, platename){
  plate = platename
  meanlist=levels(as.factor(data.deltaCT.PO.clean$Sample.Name))
  data.means=data.frame(matrix(NA, ncol=6))
  names(data.means)=c("Sample.Name","deltaCT.mean","deltaCT.POadjusted.mean", "CT.Mt_ND1.mean", "CT.RPPH1.mean","Plate")
  merged.means <- data.frame(matrix(NA,ncol=6))
  colnames(merged.means) <- c("Sample.Name","deltaCT.mean","deltaCT.POadjusted.mean","CT.Mt_ND1.mean","CT.RPPH1.mean","Plate")
  
  for (i in 1:length(meanlist)){
    
    sample.test=meanlist[i]
    # this if statement tests to see if there's only one replicate left for this sample
    if (nrow(na.omit(subset(data.deltaCT.PO.clean, Sample.Name==sample.test)))==1){
      only_one <- subset(data.deltaCT.PO.clean, Sample.Name==sample.test)
      subject <- only_one$Subject[1]
      visit <- only_one$Visit[1]
      mean.delta=mean.adjusted=mean.ND1=mean.RPPH1=NA
      # only one replicate left for the sample
    } else {
      all_three <- subset(data.deltaCT.PO.clean, Sample.Name==sample.test)
      subject <- all_three$Subject[1]
      visit <- all_three$Visit[1]
      mean.delta=mean(as.numeric(na.omit(subset(data.deltaCT.PO.clean, Sample.Name==sample.test)$deltaCT)))
      mean.adjusted=mean(as.numeric(na.omit(subset(data.deltaCT.PO.clean, Sample.Name==sample.test)$deltaCT.POadjusted)))
      mean.ND1=mean(as.numeric(na.omit(subset(data.deltaCT.PO.clean, Sample.Name==sample.test)$CT.Mt_ND1)))
      mean.RPPH1=mean(as.numeric(na.omit(subset(data.deltaCT.PO.clean, Sample.Name==sample.test)$CT.RPPH1)))
      sd.test=sd(as.numeric(na.omit(subset(data.deltaCT, Sample.Name==sample.test)$deltaCT)))
    }
    data.means[i,]=c(subject, visit, sample.test,mean.delta,mean.adjusted, mean.ND1, mean.RPPH1, plate)
    data.means$deltaCT.mean <- as.numeric(data.means$deltaCT.mean)
    data.means$deltaCT.POadjusted.mean <- as.numeric(data.means$deltaCT.POadjusted.mean)
  }
  merged.means <- rbind(merged.means, data.means)
  merged.means <- na.omit(merged.means)
  return(merged.means)
}

