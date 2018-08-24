#' Function that will look at the ND1 copies in ddPCR data
#' 
#' @param good_data data with enough droplets. 
#' 
#' @export
#' 
#' @return A ggplot showing the amount of mt-nd1 in each sample
#' 
#' @examples
#' looknd(chemicaldata)


looknd <- function(good_data){
  nd1 <- subset(good_data, Target == 'MT-ND1')
  ggplot(nd1, aes(Sample, CopiesPer20uLWell)) + geom_boxplot() + xlab('Sample') + ylab('Copies of MT-ND1 per 20 uL Well')+ geom_hline(yintercept = 120000, color = 'red') + ggtitle('Input sample vs. Copies of MT-ND1 per 20 uL Well') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) # + geom_abline(slope = 17000, intercept = 0, color = 'blue') 
}