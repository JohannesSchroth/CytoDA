# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency

usethis::use_package('gridExtra')
usethis::use_package('grid')
usethis::use_package('ggplot2')
usethis::use_package('umap')
usethis::use_package('dplyr')
usethis::use_package('reshape')
usethis::use_package('RColorBrewer')
usethis::use_package('cytofkit')
usethis::use_package('FlowSOM')
usethis::use_package('Rtsne')
usethis::use_package('ggdendro')
usethis::use_package('plotly')
usethis::use_package('shiny')
usethis::use_package('shinycssloaders')
usethis::use_package('monocle')
usethis::use_package('shinydashboard')
usethis::use_package('DT')
usethis::use_package('flowCore')
usethis::use_package('openCyto')
usethis::use_package('ggsci')
usethis::use_package('rhandsontable')
usethis::use_package('shinyBS')
usethis::use_package('sortable')


## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = 'dim_reduction_pca')

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct('fs_to_ff')
golem::add_fct('transform_data')
golem::add_fct('edit_samplenames')
golem::add_fct('edit_colnames')
golem::add_fct('calculate_clusters')
golem::add_fct('calculate_pca')
golem::add_fct('calculate_tsne')
golem::add_fct('calculate_umap')
golem::add_fct('calculate_dm')
golem::add_fct('calculate_heatmap')


golem::add_utils( "helpers" )


## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("CytoDA")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

