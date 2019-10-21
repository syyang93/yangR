#' Assigns pipetting order based on well position
#' 
#' @param mydata_deltaCT a data frame that contains deltaCT information. 
#' 
#' @export
#' 
#' @return Returns results of the rank-order analysis
#' 
#' @examples
#' 	ranks <- getRanks(mydata_deltaCT)

assignPO <- function(data.deltaCT) {
  data.deltaCT.PO=data.frame(matrix(NA, ncol=6))
  colnames(data.deltaCT.PO)=c("Sample.Name","deltaCT","Well.Position","PO", "CT.Mt_ND1", "CT.RPPH1")
  samplelist=levels(as.factor(data.deltaCT$Sample))
  
  for (i in 1:length(samplelist)){
    test=subset(data.deltaCT, Sample.Name==samplelist[i])
    test$Well.Position.number=as.numeric(gsub("[A-Z]","",test$Well.Position))
    test$Well.Position.letter=gsub("[0-9]","",test$Well.Position)
    test.ordered=test[order(test$Well.Position.letter, test$Well.Position.number),]
    test.ordered$deltaCT=as.numeric(test.ordered$deltaCT)
    if (nrow(test.ordered)>1){
      test.ordered$PO=1:nrow(test.ordered)
      test.ordered = test.ordered[,c("Sample.Name","deltaCT","Well.Position","PO", "CT.Mt_ND1", "CT.RPPH1")]
      data.deltaCT.PO=rbind(data.deltaCT.PO, test.ordered)
    }
  }
  data.deltaCT.PO <- na.omit(data.deltaCT.PO) 
  return(data.deltaCT.PO)
}
