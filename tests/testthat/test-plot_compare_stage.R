test_that("plot_compare_stage works", {
  sim_folder <- run_example_sim(verbose = FALSE)
  nc_file <- file.path(sim_folder, 'output/output.nc')
  
  field_file <- file.path(sim_folder, 'LakeMendota_stage_USGS05428000.csv')
  
  res <- plot_compare_stage(nc_file, field_file)
  
  expect(inherits(res, "ggplot"))
})
