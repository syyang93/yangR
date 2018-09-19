#' Filters out samples that have CT.RPPH1 > 5SD away from mean

#' @param data.deltaCT a data frame that contains deltaCT information. 
#' @param removed a file path that you would like to write removed samples to
#' @param platename the name of the plate that you are working with
#' 
#' @export
#' 
#' @return Returns a data frame that has removed samples with deltaCT > 0.5 


filterRPPH <- function(data.deltaCT.PO.clean, platename) {
  cut.off.upper=mean(data.deltaCT.PO.clean$CT.RPPH1)+ (5* (sd(data.deltaCT.PO.clean$CT.RPPH1)))
  cut.off.lower=mean(data.deltaCT.PO.clean$CT.RPPH1)- (5* (sd(data.deltaCT.PO.clean$CT.RPPH1)))
  to.omit <- data.frame()
  while (nrow(subset(data.deltaCT.PO.clean, CT.RPPH1<cut.off.lower | CT.RPPH1>cut.off.upper))!= 0){
    to.rm=data.frame(matrix(NA, ncol=3))
    names(to.rm)=c('Sample.Name', "Well.Position", "Abs.deltaCT")
    for(i in 1:length(data.deltaCT.PO.clean$Well.Position)) {
      test=data.deltaCT.PO.clean$CT.RPPH1[i]-mean(data.deltaCT.PO.clean$CT.RPPH1)
      test2=abs(test)
      to.rm[i,1] <- data.deltaCT.PO.clean$Sample.Name[i]
      to.rm[i,2] <- data.deltaCT.PO.clean$Well.Position[i]
      to.rm[i,3] <- test2
    }
    bad.sample=subset(to.rm$Well.Position,to.rm$Abs.deltaCT==max(to.rm$Abs.deltaCT))
    data.deltaCT.PO.clean=data.deltaCT.PO.clean[!(data.deltaCT.PO.clean$Well.Position %in% bad.sample),]
    cut.off.upper=mean(data.deltaCT.PO.clean$CT.RPPH1)+ (5* (sd(data.deltaCT.PO.clean$CT.RPPH1)))
    cut.off.lower=mean(data.deltaCT.PO.clean$CT.RPPH1)- (5* (sd(data.deltaCT.PO.clean$CT.RPPH1)))
    to.omit <- rbind(to.omit, to.rm[which(bad.sample == to.rm$Well.Position),])
  }
  if(nrow(to.omit) != 0){write.table(to.omit, file = paste0(removed, 'CT.RPPH1_5SD_plate_', platename), quote=F, row.names=F, sep="\t", col.names=T)}
  return(data.deltaCT.PO.clean)
}
