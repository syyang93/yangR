#' Adds key information to qPCR data
#' 
#' @param key_dir a path to the directory with the keys
#' @param key_name the name of the key to be loaded
#' @param mydata your qPCR data
#' @param all_key a key showing how the 96W plate location relates to the qPCR 384W replicate locations
#' 
#' @export
#' 
#' @return Returns the qPCR data with IDs in a column called 'pid'
#' 
#' @examples
#' 

add_key <- function(key_dir, key_name, mydata, all_key) {
  
  key_values <- read.table(paste0(key_dir, key_name), sep = '\t', header = T)
  colnames(key_values) <- c('X96.well.Storage.POSITION', 'pid')
  key_with_id <- merge(key_values, all_key, by = 'X96.well.Storage.POSITION')
  data_with_ids <- merge(mydata, key_with_id, by.x = 'Well.Position', by.y = 'Replicate.Location', all.x = T)
  data_with_ids2 <- dplyr::select(data_with_ids, -Sample.Name, - X96.well.Storage.POSITION)
  return(data_with_ids2)
}