#script to run GLM
#library(devtools)
#devtools::install_github("rqthomas/glmtools", force = TRUE)
#devtools::install_github("rqthomas/GLM3r", force = TRUE)
library(glmtools)
library(GLM3r)
library(dplyr)
library(lubridate)
setwd(here::here())
sim_folder <- "./SUNP-GLM_AED/SNP_GLM_AED/sim"
GLM3r::run_glm(sim_folder = sim_folder)

#sim_folder <- "./configuration/default_aed"

nc_file <- file.path('./SUNP-GLM_AED/SNP_GLM_AED/sim/output/output.nc')
#glmtools::sim_vars(nc_file)
# evap <- function (file) 
# {
#   day_secs <- 86400
#   m_to_mm <- 1000
#   glm_evaporation <- glmtools::get_var(file, var_name = "evaporation")
#   glm_evaporation[, 2] <- glm_evaporation[, 2] * day_secs * 
#     m_to_mm
#   names(glm_evaporation) <- c("DateTime", "evaporation(mm/d)")
#   return(glm_evaporation)
# }

current_evap <- glmtools::get_var(nc_file, var_name = "evaporation")



get_evaporation(nc_file)
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
current_snow <- glmtools::get_var(nc_file, var_name = "snow_thickness")
glmtools::plot_var_nc(nc_file, var_name = "snow_thickness")
current_carbon <- glmtools::get_var(nc_file, var_name = "CAR_ch4")
glmtools::plot_var_nc(nc_file, var_name = "CAR_ch4")
current_carbon <- glmtools::get_var(nc_file, var_name = "lake_level")
glmtools::plot_var_nc(nc_file, var_name = "lake_level")
current_carbon <- glmtools::get_var(nc_file, var_name = "NIT_amm")
glmtools::plot_var_nc(nc_file, var_name = "NIT_amm")
water_balance <- glmtools::get_var(nc_file, var_name = "tot_inflow_vol")
glmtools::plot_var_nc(nc_file, var_name = "tot_inflow_vol")
water_balance <- glmtools::get_surface_height(nc_file)

water_balance <- glmtools::get_var(nc_file, var_name = "lake_volume")

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

#get water level
water_level<-get_surface_height(nc_file, ice.rm = TRUE, snow.rm = TRUE)
plot(water_level$DateTime,water_level$surface_height)


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

