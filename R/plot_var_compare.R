#' @title Plot matching heatmaps for modeled and observed temp
#' @param nc_file Netcdf model output file
#' @param field_file CSV or TSV field data file (see \link{resample_to_field} for format)
#' @param var_name a character vector of valid variable names (see \code{\link{sim_vars}})
#' @param fig_path F if plot to screen, string path if save plot as .png
#' @param resample sample the model output to the same time points as the observations?
#' @param \dots additional arguments passed to \code{\link{resample_to_field}}
#'
#' @seealso Internally uses \link{get_var} and \link{resample_to_field}
#'
#'
#'@examples
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'nml_file <- file.path(sim_folder, 'glm2.nml')
#'field_file <- file.path(sim_folder, 'field_data.tsv')
#'
#'run_glm(sim_folder)
#'
#'plot_var_compare(nc_file, field_file, 'temp', resample=FALSE) ##makes a plot!
#'
#'@importFrom akima interp
#'@export
plot_var_compare = function(nc_file, field_file, var_name, fig_path = FALSE, resample = TRUE, ...){
  
  heatmaps <- .is_heatmap(nc_file, var_name)
  if (!heatmaps){
    warning('plot_var_compare not implemented for 1D variables')
    return()
  }
  
  start_par = par(no.readonly = TRUE)
  #Create layout
  
  mod_temp = get_var(nc_file, var_name, reference='surface')
  mod_depths = get.offsets(mod_temp)
  
  
  data = resample_to_field(nc_file, field_file, var_name=var_name, ...)
  if(resample){
  	model_df <- resample_sim(mod_temp, t_out = unique(data$DateTime))
  }else{
  	model_df = mod_temp
  }
  
  #Pivot observed into table
  x = as.numeric(as.POSIXct(data$DateTime))
  y = data$Depth
  z = data[,paste0('Observed_', var_name)]
  x_out = sort(unique(x))
  y_out = sort(unique(mod_depths))
  
  #remove any NA values before the 2D interp
  x = x[!is.na(z)]
  y = y[!is.na(z)]
  z = z[!is.na(z)]
  
  #Added a scaling factor to Y. Interp won't interpolate if X and Y are on vastly different scales.
  # I don't use Y from here later, so it doesn't matter what the mangitude of the values is.
  interped = interp(x, y*1e6, z, x_out, y_out*1e6)

  gen_default_fig(filename=fig_path, num_divs=2)#, omi = c(0.1, 0.5, 0, 0))
  .stacked_layout(heatmaps, num_divs=2)
  obs_df <- data.frame(interped$z)
  names(obs_df) <- paste('var_', y_out, sep='')
  obs_df <- cbind(data.frame(DateTime=as.POSIXct(x_out, origin='1970-01-01')), obs_df)
  
  #Use model to define X-axis plotting extent for both graphs
  xaxis <- get_xaxis(model_df[,1])
  
  y.text = y_out[1]+diff(range(y_out))*0.05 # note, reference will ALWAYS be surface for compare to field data
  .plot_df_heatmap(obs_df, bar_title = .unit_label(nc_file,var_name), overlays=c(points(x=x,y=y),text(x_out[1],y=y.text,'Observed', pos=4, offset = 1)), xaxis=xaxis)
  
  .plot_df_heatmap(model_df, bar_title = .unit_label(nc_file,var_name), overlays=text(x_out[1],y=y.text,'Modeled', pos=4, offset = 1), xaxis=xaxis)
  
  par(start_par)#set PAR back to what it started at
  if(is.character(fig_path))
    dev.off()
}