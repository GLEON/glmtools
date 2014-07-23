#'@title initializes Lake Analyzer list for .lke file
#'@description 
#'This function initializes Lake Analyzer list for .lke file.  \cr
#'
#'
#'@return lke a list with Lake Analyzer parameter call defaults
#'@keywords methods
#'@author
#'Jordan S. Read
#'@seealso \link{write_lke}, \link{set_lke}
#'@examples 
#'lke <- init_lke()
#'lke <- set_lke(lke, arg_name='totalDep', arg_val=19.4)
#'@export
init_lke  <-	function(){
  lke	<-	list(LA_out = paste('metaB','SmetaB','SmetaT','SthermD','SLn','SW','SN2',sep=", "),
              outRes = 86400,
              totalDep = NA,
              wndHeight = 2,
              wndAve	= 86400,
              thermalAve	= 86400,
              outlierWin	= 21600,
              maxT	= 40,
              minT	= -12,
              maxU	= 98,
              minU	= 0,
              metaSlp	= 0.1,
              mixDif	= 0.5,
              plotFig = 'Y',
              writeRes= 'Y')
  return(lke)
}

