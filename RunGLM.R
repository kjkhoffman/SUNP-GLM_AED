#script to run GLM
library(devtools)
devtools::install_github("rqthomas/glmtools", force = TRUE)
#devtools::install_github("rqthomas/GLM3r", force = TRUE)
library(glmtools)
library(GLM3r)
library(dplyr)
library(lubridate)
sim_folder <- "./sim"
GLM3r::run_glm(sim_folder = sim_folder)

#sim_folder <- "./configuration/default_aed"

nc_file <- file.path('sim/output/output.nc')
glmtools::sim_vars(nc_file)

current_temp <- glmtools::get_var(nc_file, var_name = "temp")
glmtools::plot_var_nc(nc_file, var_name = "temp")
current_oxy <- glmtools::get_var(nc_file, var_name = "OXY_sat")
glmtools::plot_var_nc(nc_file, var_name = "OXY_sat")
current_carbon <- glmtools::get_var(nc_file, var_name = "CAR_dic")
glmtools::plot_var_nc(nc_file, var_name = "CAR_dic")
current_carbon <- glmtools::get_var(nc_file, var_name = "white_ice_thickness")
glmtools::plot_var_nc(nc_file, var_name = "white_ice_thickness")
current_carbon <- glmtools::get_var(nc_file, var_name = "blue_ice_thickness")
glmtools::plot_var_nc(nc_file, var_name = "blue_ice_thickness")
current_carbon <- glmtools::get_var(nc_file, var_name = "CAR_ch4")
glmtools::plot_var_nc(nc_file, var_name = "CAR_ch4")
current_carbon <- glmtools::get_var(nc_file, var_name = "lake_level")
glmtools::plot_var_nc(nc_file, var_name = "lake_level")
current_carbon <- glmtools::get_var(nc_file, var_name = "NIT_amm")
glmtools::plot_var_nc(nc_file, var_name = "NIT_amm")

current_carbon <- glmtools::get_var(nc_file, var_name = "NIT_nit")
glmtools::plot_var_nc(nc_file, var_name = "NIT_nit")
current_carbon <- glmtools::get_var(nc_file, var_name = "OGM_cdom")
glmtools::plot_var_nc(nc_file, var_name = "OGM_cdom")
current_carbon <- glmtools::get_var(nc_file, var_name = "PHY_cyano")
glmtools::plot_var_nc(nc_file, var_name = "PHY_cyano")
current_carbon <- glmtools::get_var(nc_file, var_name = "PHY_diatom")
glmtools::plot_var_nc(nc_file, var_name = "PHY_diatom")
current_carbon <- glmtools::get_var(nc_file, var_name = "PHY_tchla")
glmtools::plot_var_nc(nc_file, var_name = "PHY_tchla")
current_carbon <- glmtools::get_var(nc_file, var_name = "surface_temp")
glmtools::plot_var_nc(nc_file, var_name = "surface_temp")


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

