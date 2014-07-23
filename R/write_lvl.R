#'@title write Lake Analyzer *.lvl file from GLM simulation
#'@description 
#'Creates a *.lvl file from GLM simulation.  \cr
#'
#'
#'@param glm_surface a data.frame with DateTime and surface elevation from from GLM
#'@param glm_nml a nml (a list) for GLM config
#'@param lake_name a string for the name of the lake (used in *.lvl file naming)
#'@param folder_out a boolean for including ice thickness in surface height
#'@keywords methods
#'@author
#'Jordan S. Read
#'@seealso \link{get_nml_value}, \link{read_nml}, \link{get_surface_height}
#'@examples 
#'file <- system.file('extdata', 'output.nc', package = 'rGLM')
#'glm_surface <- get_surface_height(file, ice.rm = TRUE)
#'glm_nml <- read_nml('../resources/glm.nml')
#'write_lvl(glm_surface, glm_nml)
#'@export
write_lvl  <- function(glm_surface, glm_nml, lake_name='lake',folder_out='../resources/'){
  lvl_name <- paste(lake_name, '.lvl', sep = '')
  file_path  <-	file.path(folder_out, lvl_name)
  lvl <- get_lvl(glm_surface,glm_nml)
  write.table(lvl,file=file_path,col.names=TRUE, quote=FALSE, row.names=FALSE, sep="\t")
}

get_lvl  <-	function(glm_surface,glm_nml){
  
  max_dep	<-	max(get_nml_value(glm_nml,arg_name="H"))-min(get_nml_value(glm_nml,arg_name="H"))
  dif	<-	max_dep-glm_surface[, 2]
  dif[dif<0]	<-	0 #??
  lvl	<-	data.frame('DateTime'=glm_surface[, 1],'level'=dif)
  return(lvl)
}

