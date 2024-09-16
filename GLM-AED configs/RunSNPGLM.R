# author: Robert Ladwig
# date: 10/07/2020
# title: GLM Workshop 

#### Workshop setup ####
cat("\f")
rm(list = ls())

# if you're using Rstudio:
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

setwd('./example')

# overview of files for this workshop
list.files()

# install these packages:
# install.packages("devtools")
# require(devtools)
# devtools::install_github("robertladwig/GLM3r", ref = "v3.1.1")
devtools::install_github("GLEON/GLM3r")
# devtools::install_github("hdugan/glmtools", ref = "ggplot_overhaul")
# install.packages("rLakeAnalyzer")
# install.packages("tidyverse")

# we will need these packages
library(glmtools)
library(GLM3r)
library(rLakeAnalyzer)
library(tidyverse)

# overview of glmtools functions
#   | Function       | Title           |
#   | ------------- |:-------------|
#   | `calibrate_sim` | Calibrates GLM-AED2 variables to improve fit between observed and simulated data |
#   | `compare_to_field` | compare metric for GLM vs field observations |
#   | `get_evaporation`  | get evaporation from GLM simulation |
#   | `get_hypsography` | retrieve hypsography information |
#   | `get_ice` | get ice depth from GLM simulation |
#   | `get_nml_value` | gets a nml value according to an arg_name |
#   | `get_surface_height` | get surface height from GLM simulation |
#   | `get_var` | get a variable from a GLM simulation |
#   | `get_wind` | get wind speed from GLM simulation |
#   | `model_diagnostics` | run diagnostics on model results |
#   | `plot_var_compare` | Plot matching heatmaps for modeled and observed variables |
#   | `plot_var_nc` | plot variables from a GLM simulation |
#   | `plot_var_df` | plot variables from a data.frame |
#   | `read_field_obs` | read in field data into a data.frame |
#   | `read_nml` | read in a GLM simulation `*.nml` file |
#   | `resample_sim` | get subset of time from a generic timeseries data.frame |
#   | `resample_to_field` | match GLM water temperatures with field observations |
#   | `set_nml` | sets values in nml object |
#   | `sim_metrics` | get possible metrics for comparing GLM outputs to field |
#   | `summarize_sim` | creates GLM simulation summary outputs |
#   | `validate_sim` | run diagnostics on model results vs observations |
#   | `write_nml` | write GLM `*.nml` for a GLM simulation |

# check out which R version we're currently using
glm_version()

#### reading the namelist file into R  ####
glm_template = 'glm3-wAED_WW.nml' 
sim_folder <- getwd()
out_file <- file.path(sim_folder, "output","output.nc")
field_data <- file.path(sim_folder,"bcs","field_temp_oxy.csv")
file.copy(glm_template, 'glm3.nml', overwrite = TRUE)
nml_file <- file.path(sim_folder, 'glm3.nml')

# read example configuration into memory
snp_nml <- read_nml(nml_file = file.path(sim_folder,'glm3.nml'))
snp_nml
class(snp_nml)
names(snp_nml)
snp_nml[[1]][1:4]
snp_nml$light

# read and change values inside the namelist file
kw_1 <- get_nml_value(snp_nml, 'Kw')
print(kw_1)

snp_nml <- set_nml(eg_nml, 'Kw', 1.4)
get_nml_value(snp_nml, 'Kw')

# write modified values into namelist file
write_nml(snp_nml, file = nml_file)

snp_nml <- set_nml(snp_nml, 'Kw', kw_1)
write_nml(snp_nml, file = nml_file)

#### Example 2: first visualisations ####
# run GLM
GLM3r::run_glm(sim_folder, verbose = T)

# visualize change of water table over time
water_height <- get_surface_height(file = out_file)
ggplot(water_height, aes(DateTime, surface_height)) +
  geom_line() +
  ggtitle('Surface water level') +
  xlab(label = '') + ylab(label = 'Water level (m)') +
  theme_minimal()


