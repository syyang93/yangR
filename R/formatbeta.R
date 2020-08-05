#' Function that formats betas to look nice
#' 
#' @export
#' @param beta The value you would like to format
#' 
#' @examples
#' formatbeta(beta)

formatbeta = function(beta){
  formatC(round(beta, 3), 3, format = "f")
}