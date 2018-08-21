#' Function for plotting cor values onto a correlation matrix
#' 
#' @param lm_object The results of an lm that you want a p-value QQ-plot for
#' 
#' @export
#' 
#' @return A p-value QQ-plot
#' 
#' @examples
#' pairs(~TRISCHD + covariates, data = df, na.action = na.omit, upper.panel=panel.cor)

panel.cor<-function(x,y,digits=2,prefix="",cex.cor,...)
{
  usr<-par("usr");on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r<-abs(cor(x,y))
  txt<-format(c(r,0.123456789),digits=digits)[1]
  txt<-paste(prefix,txt,sep="")
  if(missing(cex.cor))cex.cor<-0.8/strwidth(txt)
  text(0.5,0.5,txt,cex=cex.cor*r)
}