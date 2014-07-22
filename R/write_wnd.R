#'@title write Lake Analyzer *.wnd file from GLM simulation
#'@description 
#'Creates a *.wnd file for Lake Analyzer use from GLM simulation.  \cr
#'
#'
#'@param glm_wnd a data.frame with DateTime and temperatures from GLM
#'@param lake_name a string for the name of the lake (used in *.lvl file naming)
#'@param folder_out a boolean for including ice thickness in surface height
#'@keywords methods
#'@author
#'Jordan S. Read
#'@seealso \link{write_wtr}, \link{write_lvl}, \link{get_wind}
#'@examples 
#'file = '../test/output.nc'
#'glm_wind <- get_wind(file)
#'write_wnd(glm_wind,lake_name='lake',folder_out='../resources/')
#'@export
write_wnd  <- function(glm_wind,lake_name='lake',folder_out='../Supporting Files/'){
  file_out <- paste(c(folder_out,lake_name,'.wnd'),collapse="")
  write.table(glm_wind,file=file_out,col.names=TRUE, quote=FALSE, row.names=FALSE, sep="\t")
}