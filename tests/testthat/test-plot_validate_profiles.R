context("validate plots")

test_that("plots work", {
  sim_folder <- run_example_sim(verbose = F)
  nc_file <- file.path(sim_folder, 'output/output.nc')
  field_file <- file.path(sim_folder, 'field_data.csv')

  expect_error(plot_validate_profiles(nc_file, field_file = NULL, fig_path = NULL))
  expect_error(plot_validate_profiles(nc_file, field_file = NA, fig_path = NULL))
  
  gg_profiles = plot_validate_profiles(nc_file, field_file, fig_path = file.path(tempdir(),'temp.png'))
  vdiffr::expect_doppelganger("gg_profiles plot", gg_profiles)
  
  nml_file <- system.file("extdata/glm3.nml", package = 'glmtools')
  gg_meteo = plot_meteo(met_file = nml_file)
  vdiffr::expect_doppelganger("gg_meteo plot", gg_meteo)
  
  gg_plot_var_compare = plot_var_compare(nc_file, field_file, var = 'temp', precision = 'hours')
  vdiffr::expect_doppelganger("gg_plot_var_compare plot", gg_plot_var_compare)
  
  gg_V = plot_var(nc_file, 'V')
  vdiffr::expect_doppelganger("gg_V plot", gg_V)
  
  gg_evap = plot_var(nc_file, 'evap')
  vdiffr::expect_doppelganger("gg_evap plot", gg_evap)
  
  gg_TotV = plot_var(nc_file, 'Tot_V')
  vdiffr::expect_doppelganger("gg_TotV plot", gg_TotV)
})
