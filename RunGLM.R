#script to run GLM
library(devtools)
devtools::install_github("rqthomas/glmtools", force = TRUE)
devtools::install_github("rqthomas/GLM3r", force = TRUE)
library(glmtools)
library(GLM3r)
library(dplyr)
library(lubridate)
sim_folder <- "./sim"
GLM3r::run_glm(sim_folder = sim_folder)

#sim_folder <- "./configuration/default_aed"

nc_file <- file.path('sim/output/output.nc')
current_temp <- glmtools::get_var(nc_file, var_name = "temp")
glmtools::plot_var_nc(nc_file, var_name = "temp")
current_oxy <- glmtools::get_var(nc_file, var_name = "OXY_sat")
glmtools::plot_var_nc(nc_file, var_name = "OXY_sat")

#pull only one depth to check against temperature
#this may be a problem because of how temperature was collected in some years. 
glm_temp1m <- glmtools::get_var(
  file = nc_file,
  var_name = "temp",
  reference = "surface",
  z_out = 1)

glm_temp0p5m <- glmtools::get_var(
  file = nc_file,
  var_name = "temp",
  reference = "surface",
  z_out = 0.5)

glm_temp5.5m <- glmtools::get_var(
  file = nc_file,
  var_name = "temp",
  reference = "surface",
  z_out = 5.5)

glm_temp9.5m <- glmtools::get_var(
  file = nc_file,
  var_name = "temp",
  reference = "surface",
  z_out = 9.5)

