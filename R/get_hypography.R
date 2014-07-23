#'@title retrieve hypsography information from glm_nml object or file
#'@description 
#'Retrieves hypsography information from glm_nml object or file  \cr
#'
#'
#'@param glm_nml a nml (a list) for GLM config
#'@param file a string with the path to the GLM glm.nml file
#'@return glm_bth a data.frame with \code{Depths} and \code{Areas}
#'@keywords methods
#'@author
#'Jordan S. Read
#'@seealso \link{write_lvl}, \link{read_nml}, \link{get_nml_value}
#'@examples 
#'glm_nml <- read_nml('../resources/glm.nml')
#'write_bth(glm_nml, lake_name='lake', folder_out='../resources/')
#'@export
get_hypsography <- function(glm_nml=NULL, file=NULL){
  # if both are passed, glm_nml is used and 'file' is ignored
  if (is.null(file) & is.null(glm_nml)){stop('glm.nml file path OR glm_nml must be specified')}
  
  if (is.null(glm_nml)){
    glm_nml <- read_nml(file)
  }
  
  max_elev	<-	get_nml_value(glm_nml,'crest_elev')
  heights	<-	get_nml_value(glm_nml,'H')
  bthA	<-	rev(get_nml_value(glm_nml,'A')*1000) # now m2
  bthZ	<-	rev(max_elev-heights)
  glm_bth	<-	data.frame(bthZ,bthA)
  names(glm_bth)	<-	c("Depths","Areas")
  return(glm_bth)
}