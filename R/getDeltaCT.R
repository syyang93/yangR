#' Reads qPCR data into a dataframe
#' 
#' @param mydata the data frame with the qPCR data
#' @param nuc.probe the name of the nuclear probe
#' @param mit.probe the name of the mitochondrial probe
#' 
#' @export
#' 
#' @return Returns a data frame with the column (deltaCT), which represents the Ct(nuc) - Ct(mito)
#' 
#' @examples
#' mydata2 <- getDeltaCT(mydata)

getDeltaCT <- function(mydata, nuc.probe = 'RPPH', mit.probe = 'ND1') {
  RPPH1 <- subset(mydata, Target.Name == nuc.probe)
  Mt_ND1 <- subset(mydata, Target.Name == mit.probe)
  RPPH1$CT.RPPH1 <- RPPH1$CT
  RPPH1$CT <- NULL
  RPPH1$Target.Name <- NULL
  Mt_ND1$CT.Mt_ND1 <- Mt_ND1$CT
  Mt_ND1$CT <- NULL
  Mt_ND1$Target.Name <- NULL
  mydata_deltaCT <- merge(Mt_ND1, RPPH1, by = c("Well.Position", 
                                                "Sample.Name"))
  mydata_deltaCT$CT.RPPH1 <- as.numeric(mydata_deltaCT$CT.RPPH1)
  mydata_deltaCT$CT.Mt_ND1 <- as.numeric(mydata_deltaCT$CT.Mt_ND1)
  mydata_deltaCT$deltaCT <- mydata_deltaCT$CT.RPPH1 - mydata_deltaCT$CT.Mt_ND1
  return(mydata_deltaCT)
}
