context("plot_validate_profiles")

test_that("plot_default", {
  sim_folder <- run_example_sim(verbose = F)
  nc_file <- file.path(sim_folder, 'output/output.nc')
  field_file <- file.path(sim_folder, 'field_data.csv')
  old_par = par(no.readonly = TRUE)
  plot_validate_profiles(nc_file, field_file, fig_path = file.path(tempdir(),'temp.png'))
  expect_error(plot_validate_profiles(nc_file, field_file = NULL, fig_path = NULL))
  expect_error(plot_validate_profiles(nc_file, field_file = NA, fig_path = NULL))
  plot_validate_profiles(nc_file, field_file, fig_path = NULL)
  expect_equal(old_par,par(no.readonly = TRUE))
})


context("plot_meteo")
test_that("plot_meteo", {
  nml_file <- nml_template_path()
  # expect_error(plot_meteo(nml_file,fig_path = T))
  old_par = par(no.readonly = TRUE)
  plot_meteo(nml_file,fig_path = NULL)
  expect_equal(old_par, par(no.readonly = TRUE))
})

context("plot_var_compare")
test_that("plot_var_compare", {
  old_par = par(no.readonly = TRUE)
  plot_var_compare(nc_file, field_file, var = 'temp', precision = 'hours')
  expect_equal(old_par, par(no.readonly = TRUE))
})

context('plot_var')
test_that("plot_var", {
	
	old_par = par(no.readonly = TRUE)
	plot_var(nc_file, 'V')
	plot_var(nc_file, 'evap')
	plot_var(nc_file, 'Tot_V')
	plot_var(nc_file, 'u_mean')
	expect_equal(old_par, par(no.readonly = TRUE))
})
