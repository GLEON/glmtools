#'@title write Lake Analyzer *.wtr file from GLM simulation
#'@description 
#'Creates a *.wtr file for Lake Analyzer use from GLM simulation.  \cr
#'
#'
#'@param glm_temp a data.frame with DateTime and temperatures from GLM
#'@param lake_name a string for the name of the lake (used in *.lvl file naming)
#'@param folder_out a boolean for including ice thickness in surface height
#'@keywords methods
#'@author
#'Jordan S. Read
#'@seealso \link{write_lvl}, \link{get_temp}
#'@examples 
#'file <- system.file('extdata', 'output.nc', package = 'rGLM')
#'z_out <- c(0,1,2,3,4,5,6)
#'glm_temp <- get_temp(file,reference='surface',z_out=z_out)
#'folder_out <- system.file('extdata', package = 'rGLM')
#'write_wtr(glm_temp, lake_name = 'lake', folder_out = folder_out)
#'@export
write_wtr  <- function(glm_temp, lake_name = 'lake', folder_out){
  wtr_name <- paste(lake_name, '.wtr', sep = '')
  file_path  <-  file.path(folder_out, wtr_name)
  write.table(glm_temp, file=file_path, col.names=TRUE, quote=FALSE, row.names=FALSE, sep="\t")
}
