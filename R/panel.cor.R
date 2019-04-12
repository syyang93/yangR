#' Function for plotting cor values onto a correlation matrix
#' 
#'
#' 
#' @export
#' 
#' @return text for the pairwise correlation plot
#' 
#' @examples
#' pairs(~PC1 + PC2, data = df, lower.panel=twolines, diag.panel=mydiag.panel, upper.panel=panel.cor, label.pos=0.5, main="Correlation matrix of PCs and covariates", labels = c('PC1', 'PC2'))


panel.cor<-function(x,y,digits=2,prefix="",cex.cor,...)
{
  usr<-par("usr");on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r<-cor(x,y, use = 'complete.obs')
  txt<-format(c(r,0.123456789),digits=digits)[1]
  txt<-paste(prefix,txt,sep="")
  if(missing(cex.cor))cex.cor<-0.8/strwidth(txt)
  text(0.5,0.5,txt,cex=cex.cor*abs(r))
}