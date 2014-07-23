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
#'glm_nml <- system.file('extdata', 'glm.nml', package = 'rGLM') 
#'write_bth(glm_nml, lake_name='lake', folder_out='../resources/')
#'@export
write_bth  <-	function(glm_nml,lake_name='lake',folder_out){	
  #file.path("E:", "DATA", "example.csv")
  fileN	<-	paste(c(folder_out,lake_name,'.bth'),collapse="")
  glm_bth <- get_hypsography(glm_nml)
  write.table(glm_bth,file=fileN,col.names=TRUE, quote=FALSE, row.names=FALSE, sep=",")
}


