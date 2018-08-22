#' Function for making fancy correlation matrices, adapted from Rebecca
#' 
#' @export
#' 
#' @examples
#' pairs(~PC1 + PC2, data = df, lower.panel=twolines, diag.panel=mydiag.panel, upper.panel=panel.cor, label.pos=0.5, main="Correlation matrix of PCs and covariates", labels = c('PC1', 'PC2'))


twolines = function(x,y) {
  points(x,y,pch=19, cex = 0.3, col="black") 
  # abline --> line of best fit. 
  abline(lm(y~x),col="red")
  #      legend("bottomright", paste("R=",prettyNum(cor(x,y), digits=3)),bty="n" ,cex=1.5)
  # ^ that part shows the R values. 
}
