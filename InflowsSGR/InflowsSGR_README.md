# SUNP-GLM_AED

Start-Up Code is a collection of other peoples code for GLM-AED at Sunapee. 
I will base calibrations for GLM-AED off of this. Using a combination of Whitney and Nicole's parameters. We then spent time going through the params with Quinn and Cayelan. Mary has helped a lot with this so far. Freya has helped me get all of the files set up and running. 

Everything needed to run GLM-AED for Sunapee is in the SUNP_GLM_AED folder. 
Inflows are made in this folder: InflowsSGR

The endpoint for the S3 buckets was changed in late April. Not all endpoints have been adjusted, so code may not run. 

So many assumptions, listing what I can here: 
1. Using TMWB model for inflow
2. 

Here is what is in the code for the following files (5May25)
tmwb_inflow.qmd - creates the inflow file using the tmwb model. This is a monthly model. 
InflowPrep.qmd - Adds nutrients to the inflow file. draws nutrients from a random distribution. 
InflowModel.qmd - the intention was to use this to create individual inflow models for each stream. This only has code to pull metdata right now
GetInflowData.qmd - this pulls Sugar River Outflow data from NHDES using an API. We don't actually use this data anywhere yet.
EcoHydrology_functions.R - these are the functions from the now deprecated ecohydrology package. 

