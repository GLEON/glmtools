#'@title write Lake Analyzer *.wnd file from GLM simulation
#'@description 
#'Creates a *.wnd file for Lake Analyzer use from GLM simulation.  \cr
#'
#'
#'@param glm_wind a data.frame with DateTime and temperatures from GLM
#'@param lake_name a string for the name of the lake (used in *.lvl file naming)
#'@param folder_out a string for the output folder for the *.lvl file
#'@keywords methods
#'@author
#'Jordan S. Read
#'@seealso \link{write_wtr}, \link{write_lvl}, \link{get_wind}
#'@examples 
#'file <- system.file('extdata', 'output.nc', package = 'glmtools')
#'folder_out <- system.file('extdata', package = 'glmtools')
#'glm_wind <- get_wind(file)
#'write_wnd(glm_wind,lake_name='lake',folder_out = folder_out)
#'@export
write_wnd  <- function(glm_wind, lake_name = 'lake', folder_out){
  wnd_name <- paste(lake_name, '.wnd', sep = '')
  file_path  <-  file.path(folder_out, wnd_name)
  write.table(glm_wind,file=file_path,col.names=TRUE, quote=FALSE, row.names=FALSE, sep="\t")
}