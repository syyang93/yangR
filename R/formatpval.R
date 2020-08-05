#' Function that formats betas to look nice
#' 
#' @export
#' @param pval The value you would like to format
#' 
#' @examples
#' formatpval(pval)

formatpval = function(pval){
  ifelse(pval > 0.001, format(round(pval, 3), nsmall = 3), formatC(pval, format = "e", digits = 2))
}
