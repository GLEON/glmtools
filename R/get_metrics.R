#'@title get possible metrics for comparing GLM outputs to field
#'@description
#'returns a character array of metric names that can be supported by \code{compare_to_field}. 
#'This function is only designed to handle calls to physical metrics that return a single value.
#'An example of this behavior is thermocline depth (but not water density for all depth-resolved measurements).
#'@param with_nml include metrics that require information in the nml file
#'@return a character array of metrics that can be used
#'@keywords methods
#'@seealso \link{compare_to_field}, \link{resample_to_field}, \link{read_nml}
#'@author
#'Jordan S. Read
#'@examples 
#'get_metrics()
#'get_metrics(with_nml = TRUE)
#'@import rLakeAnalyzer
#'@export
get_metrics <- function(with_nml =FALSE){
  package_name <- 'rLakeAnalyzer'
  funs <- fun_from_package(package_name)
  
  arg_names <- c('wtr', 'depths')
  metrics <- match_inputs(funs, arg_names)
  
  if (with_nml){
    arg_names = c('wtr', 'depths', 'bthA', 'bthD')
    metrics <- c(metrics, match_inputs(funs, arg_names))
  }
  
  
  return(metrics)
}

fun_from_package <- function(package_name, private = FALSE){
  
  if (private){
    funs <- as.character(unclass(lsf.str(envir = asNamespace(package_name), all = T)))
  } else {
    funs <- ls(paste0('package:', package_name))
  }
  
  return(funs)
}

match_inputs <- function(funs, arg_names){
  
  
  fun_valid <- vector(length = length(funs))
  for (i in 1:length(funs)){
    fun_valid[i] = all(arg_names %in% names(formals(funs[i])))
  }
  return(funs[fun_valid])

}