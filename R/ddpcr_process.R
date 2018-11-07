#' Shapes the ddPCR data into a better format for ggplot
#' 
#' @param testdata the original data frame after reading in the csv file
#' 
#' @export
#' 
#' @return Returns a data frame that's combined the mt-ND1 and RPPH probe info 
#' 
#' @examples
#' combined.data <- ddpcr_process(testdata)


ddpcr_process <- function(testdata) {
  require(plyr)
  require(dplyr)
  require(magrittr)  
  
  # drop samples with not enough droplets
  good_data <- subset(testdata, AcceptedDroplets > 8000) # dropped two samples
  
  # select columns to care about
  good_data <- dplyr::select(good_data, Well, Sample, Target, Concentration, CopiesPer20uLWell, AcceptedDroplets, Ratio, PoissonRatioMax, PoissonRatioMin)
  
  # assign MT ratio
  good_data$MT.ratio <- 1/good_data$Ratio
  
  # combine the data into a good dataframe
  rpph1 <- subset(good_data, Target == 'RPPH1')
  nd1 <- subset(good_data, Target == 'MT-ND1')
  rpph1$RPPH_CopiesPer20uLWell <- rpph1$CopiesPer20uLWell
  rpph1 <- dplyr::select(rpph1, Well, RPPH_CopiesPer20uLWell)
  nd1$ND_CopiesPer20uLWell <- nd1$CopiesPer20uLWell
  
  combined.data <- merge(nd1, rpph1, by = 'Well')
  return(combined.data)
}
