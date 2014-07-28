#'@title write Lake Analyzer *.bth file from GLM simulation
#'@description 
#'Creates a *.bth file for Lake Analyzer use from GLM simulation.  \cr
#'
#'
#'@param glm_nml a nml (a list) for GLM config
#'@param lake_name a string for the name of the lake (used in *.lvl file naming)
#'@param folder_out a boolean for including ice thickness in surface height
#'@keywords methods
#'@author
#'Jordan S. Read
#'@seealso \link{write_wtr}, \link{write_lvl}, \link{get_wind}
#'@examples 
#'nml_file <- system.file('extdata', 'glm.nml', package = 'glmtools') 
#'folder_out <- system.file('extdata', package = 'glmtools') 
#'glm_nml <- read_nml(file = nml_file)
#'write_bth(glm_nml, lake_name='lake', folder_out = folder_out)
#'@export
write_bth  <-	function(glm_nml,lake_name='lake',folder_out){	
  bth_name <- paste(lake_name, '.bth', sep = '')
  file_path	<-	file.path(folder_out, bth_name)
  glm_bth <- get_hypsography(glm_nml)
  write.table(glm_bth, file = file_path, col.names=TRUE, quote=FALSE, row.names=FALSE, sep=",")
}


