#'@title sets values in lke object
#'@description This function sets values in lke object for Lake Analyzer config.
#'@param lke a nml (a list) for Lake Analyzer config
#'@param arg_name a string representing a valid field in the lke object
#'@param arg_val value for the valid field in lke specified by \code{arg_name}
#'@return lke a modified lke object
#'@author
#'Jordan S. Read
#'@examples
#'lke <- init_lke()
#'lke <- set_lke(lke, arg_name='totalDep', arg_val=19.4)
#'@seealso \link{init_lke}, \link{write_lke}
#'@export
set_lke	<-	function(lke,arg_name,arg_val){
  lke[arg_name]	<-	arg_val
  return(lke)
}
