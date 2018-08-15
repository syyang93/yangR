#' Reads qPCR data into a dataframe
#' 
#' @param data_dir a path to the data directory where your files are stored
#' @param filename the name of the file to be loaded
#' 
#' @export
#' 
#' @return Returns a dataframe with all the qPCR data in it.
#' 
#' @examples
#' df <- read_qpcr_data('/Users/arkinglab/Documents/', 'qPCR_data.xls') 

read_qpcr_data <- function(data_dir, filename, skiplines = 45) {
  require(readxl)
  require(dplyr)
  data <- read_excel(paste(data_dir,filename,sep=""), sheet = "Results", skip = skiplines)
  data <- dplyr::select(data, 'Well Position', 'Sample Name', 'Target Name', 'CT')
  data <- data.frame(data)
  # data <- data[1:576,] <- you needed this for processing your old data...
}
