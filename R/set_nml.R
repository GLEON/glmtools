#'@title sets values in nml object
#'@description This function sets values in nml object for GLM config.
#'@param glm_nml a nml (a list) for GLM config
#'@param arg_name a string representing a valid field in glm_nml
#'@param arg_val value for the valid field in glm_nml specified by \code{arg_name}
#'@param arg_list a list made up of valid \code{arg_name}s and \code{arg_val}s
#'@return glm_nml a modified nml
#'@author
#'Jordan S. Read
#'@examples
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nml_file <- file.path(sim_folder, 'glm2.nml')
#'glm_nml <- read_nml(nml_file)
#'get_nml_value(glm_nml, arg_name = 'Kw')
#'glm_nml <- set_nml(glm_nml, arg_name = 'Kw', arg_val = 1.4)
#'glm_nml <- set_nml(glm_nml, arg_list = list('Kw' = 1.4))
#'print(glm_nml)
#'@seealso \link{get_nml_value}, \link{read_nml}
#'@export
set_nml <- function(glm_nml, arg_name, arg_val, arg_list = NULL){
  
  if (missing(arg_name) & missing(arg_val)){
    return(setnmlList(glm_nml,arg_list))
  }
  
  if (!is.character(arg_name)){stop('arg_name should be a character')}

  if (!is.null(arg_list) & arg_name %in% names(arg_list)){
    warning(c("duplicate names given to arg_name and arg_list.", 
              " arg_name and arg_val values will overwrite duplicate arg_list values."))
    glm_nml <- setnmlList(glm_nml,arg_list)
  }
  
  
  
  currVal	<-	get_nml_value(glm_nml, arg_name, warn=FALSE)
  typeError	<-	paste0("input ", arg_name ," must be of same data type as current value")
  if (is.logical(currVal) & !is.logical(arg_val)){
    stop(c(typeError,' (logical)'))
  } else if (is.character(currVal) & !is.character(arg_val)){
    stop(c(typeError,' (character)'))
  } else if (is.numeric(currVal) & !is.numeric(arg_val)){
    stop(c(typeError,' (numeric)'))
  }
  
  # get appropriate block to place val within ** assumes no duplicate param names in other blocks **
  blck	<-	get_block(glm_nml,arg_name)
  arg_name <- get_arg_name(arg_name)
  if(length(arg_val) > 1){
    arg_val <- paste0(arg_val, collapse = ",")
  }
  glm_nml[[blck]][[arg_name]]	<- arg_val
  return(glm_nml)
}