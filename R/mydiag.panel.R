#' Function for making fancy correlation matrices, adapted from Rebecca
#' 
#' @export
#' 
#' @examples
#' pairs(~PC1 + PC2, data = df, lower.panel=twolines, diag.panel=mydiag.panel, upper.panel=panel.cor, label.pos=0.5, main="Correlation matrix of PCs and covariates", labels = c('PC1', 'PC2'))

mydiag.panel <- function( x,  labels, ...){
  ll <- par("usr")
  rect(ll[1], ll[3], ll[2], ll[4], col="darkolivegreen1")
}
