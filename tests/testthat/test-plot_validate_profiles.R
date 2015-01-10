context("plot_validate_profiles")

test_that("plot_default", {
  sim_folder <- run_example_sim(verbose = F)
  nc_file <<- file.path(sim_folder, 'output.nc')
  field_file <<- file.path(sim_folder, 'field_data.tsv')
  
  expect_error(plot_validate_profiles(nc_file, field_file, fig_path = file.path(tempdir(),'temp.png')))
  expect_error(plot_validate_profiles(nc_file, field_file, fig_path = T))
  expect_error(plot_validate_profiles(nc_file, field_file = NULL, fig_path = F))
  expect_error(plot_validate_profiles(nc_file, field_file = NA, fig_path = F))
})