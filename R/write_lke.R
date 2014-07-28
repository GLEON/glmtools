
#'@title write Lake Analyzer *.lke file from GLM simulation
#'@description 
#'Creates a *.lke file for Lake Analyzer use from GLM simulation.  \cr
#'
#'
#'@param lke a nml (a list) for GLM config
#'@param lake_name a string for the name of the lake (used in *.lke file naming)
#'@param folder_out a boolean for including ice thickness in surface height
#'@keywords methods
#'@author
#'Jordan S. Read
#'@seealso \link{write_wtr}, \link{write_lvl}, \link{init_lke}, \link{set_lke}
#'@examples 
#'lke <- init_lke()
#'lke <- set_lke(lke, arg_name = 'totalDep', arg_val = 19.4)
#'folder_out <- system.file('extdata', package = 'rGLM') 
#'write_lke(lke, lake_name = 'lake', folder_out = folder_out)
#'@export
write_lke  <-	function(lke, lake_name = 'lake', folder_out){	
  lke_metadata	<-	init_lke_metadata()
  
  if (any(is.na(lke))){stop("no lke parameters can be NA")}
  lke_name <- paste(lake_name, '.lke', sep = '')
  file_path  <-	file.path(folder_out, lke_name)
  sink(file_path)
  cat(c("Configuration file for",lake_name,"\n","\n"))
  for (ln in 1:length(lke)){
    cat(as.character(lke[[ln]]))
    cat(c("\t","\t",lke_metadata[[names(lke[ln])]],"\n"))
  }
  sink()
}


init_lke_metadata  <-	function(){
  lke_metadata	<-	list(LA_out = "#outputs",
                       outRes = "#output resolution (s)",
                       totalDep = "#total depth (m)",
                       wndHeight = "#height from surface for wind measurement (m)",
                       wndAve	= "#wind averaging (s)",
                       thermalAve	= "#thermal layer averaging (s)",
                       outlierWin	= "#outlier window (s)",
                       maxT	= "#max water temp (°C)    inf if none",
                       minT	= "#min water temp (°C)    -inf if none",
                       maxU	= "#max wind speed (m/s)   inf if none",
                       minU	= "#min wind speed (m/s)   -inf if none",
                       metaSlp	= "#meta min slope (drho/dz per m)",
                       mixDif	= "#mixed temp differential (°C)",
                       plotFig = "#plot figure (Y/N)",
                       writeRes= "#write results to file (Y/N)")
  return(lke_metadata)
}

