#' Function that will look at the RPPH1 copies in ddPCR data
#' 
#' @param good_data data with enough droplets. 
#' 
#' @export
#' 
#' @return A ggplot showing the amount of rpph1 in each sample
#' 
#' @examples
#' lookrpph(chemicaldata)

lookrpph <- function(good_data){
  rpph1 <- subset(good_data, Target == 'RPPH1')
  ggplot(rpph1, aes(Sample, CopiesPer20uLWell)) + geom_boxplot() + xlab('Sample') + ylab('Copies of RPPH1 per 20 uL Well') + ggtitle('Input sample vs. Copies of RPPH1 per 20 uL Well') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) # + geom_abline(slope = 125, intercept = 0, color = 'blue') 
}