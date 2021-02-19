#' Plots xvar against yvar, and title is the P-value
#' 
#' @param df data frame for ggplotting
#' @param character of xvariable name
#' @param character of yvariable name
#' 
#' @export
#' 
#' @return a ggplot object
#' 
#' @examples
#' plot_lm(df, 'xvariable', 'yvariable')

plot_lm = function(df, xvar, yvar){
  formula = as.formula(paste0(yvar, '~', xvar))
  pval = coef(summary(lm(formula, data = df)))[2,4]
  ggplot(df, aes_string(xvar, yvar)) + geom_point() + geom_smooth(method = 'lm') + ggtitle(paste0('Pval = ', formatpval(pval)))
}