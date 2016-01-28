context("plot_validate_profiles")

test_that("plot_default", {
  sim_folder <- run_example_sim(verbose = F)
  nc_file <<- file.path(sim_folder, 'output.nc')
  field_file <<- file.path(sim_folder, 'field_data.tsv')
  old_par = par(no.readonly = TRUE)
  expect_error(plot_validate_profiles(nc_file, field_file, fig_path = file.path(tempdir(),'temp.png')))
  expect_error(plot_validate_profiles(nc_file, field_file, fig_path = T))
  expect_error(plot_validate_profiles(nc_file, field_file = NULL, fig_path = F))
  expect_error(plot_validate_profiles(nc_file, field_file = NA, fig_path = F))
  plot_validate_profiles(nc_file, field_file, fig_path = F)
  expect_equal(old_par,par(no.readonly = TRUE))
})

context("plot_meteo")
test_that("plot_meteo", {
  nml_file <- nml_template_path()
  
  expect_error(plot_meteo(nml_file,fig_path = T))
  old_par = par(no.readonly = TRUE)
  plot_meteo(nml_file,fig_path = F)
  expect_equal(old_par, par(no.readonly = TRUE))
})

context("plot_temp_compare")
test_that("plot_temp_compare", {
  old_par = par(no.readonly = TRUE)
  plot_temp_compare(nc_file, field_file)
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
