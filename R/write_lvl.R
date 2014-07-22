#'@title write Lake Analyzer *.lvl file from GLM simulation
#'@description 
#'Creates a *.lvl file from GLM simulation.  \cr
#'
#'
#'@param glm_surface a data.frame with DateTime and surface elevation from from GLM
#'@param glm_nml a nml (a list) for GLM config
#'@param lake_name a string for the name of the lake (used in *.lvl file naming)
#'@param folder_out a boolean for including ice thickness in surface height
#'@return a data.frame with DateTime and surface_height (in meters)
#'@keywords methods
#'@author
#'Jordan S. Read
#'@seealso \link{get_nml}, \link{read_nml}
#'@examples 
#'file = '../test/output.nc'
#'glm_surface <- get_surface_height(file, ice.rm = TRUE)
#'glm_nml <- read_nml('../resources/glm.nml')
#'write_lvl(glm_surface, glm_nml)
#'@export
write_lvl  <- function(glm_surface, glm_nml, lake_name='lake',folder_out='../Supporting Files/'){
  file_out <- paste(c(folder_out,lake_name,'.lvl'),collapse="")
  
  lvl <- get_lvl()
  write.table(lvl,file=file_out,col.names=TRUE, quote=FALSE, row.names=FALSE, sep="\t")
}

get_lvl  <-	function(glm_surface,glm_nml){
  
  max_dep	<-	max(get.nml(glm_nml,argName="H"))-min(get.nml(glm_nml,argName="H"))
  dif	<-	max_dep-glm_surface[, 2]
  dif[dif<0]	<-	0 #??
  lvl	<-	data.frame('DateTime'=glm_surface[1, ], 'level(positive_Z_down_in_meters)'=dif)
  return(lvl)
}

