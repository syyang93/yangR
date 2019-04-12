#' Function that makes a ggplot density plot with a rug on the bottom
#' 
#' @param to_plot Data frame containing whatever you want to plot
#' @param density_val A character string with the value you want the density plot for
#' @param col The color of the density line
#' @param outline If you want the histogram to have an outline
#' 
#' @export
#' 
#' @return Returns a ggplot
#' 
#' @examples
#' density_rug(with.pheno.noblue, 'deltaCT.Plateadjust.mean', outline = F)

density_rug <- function(to_plot, density_val){
  g <- ggplot(to_plot,aes_string(x = density_val))+
    geom_rug() +
    geom_density() + ggtitle(paste0('Density plot for ', density_val))
  return(g)
}


