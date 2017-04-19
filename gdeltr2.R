# Be sure you are using R-3.3.2 or later version.
# If not, download from https://cran.r-project.org/bin/macosx/

rm(list = ls())
cat("\014") 
########################################################################
# load required libraries (install if libraries not found)
if ("devtools" %in% rownames(installed.packages())==F) {
  devtools::install_github("hadley/devtools")}
if ("dplyr" %in% rownames(installed.packages())==F) {
  devtools::install_github("hadley/dplyr")}
if ("trelliscopejs" %in% rownames(installed.packages())==F) {
  devtools::install_github("hafen/trelliscopejs")}
if ("hrbrthemes" %in% rownames(installed.packages())==F) {
  devtools::install_github("hrbrmstr/hrbrthemes")}
if ("tidyverse" %in% rownames(installed.packages())==F) {
  install.packages("tidyverse", repos="http://cran.us.r-project.org")}
if ("stringr" %in% rownames(installed.packages())==F) {
  install.packages("stringr", repos="http://cran.us.r-project.org")}
if ("gdeltr2" %in% rownames(installed.packages())==F) {
  devtools::install_github("abresler/gdeltr2")}
library(gdeltr2)
load_needed_packages(c('dplyr', 'magrittr'))

# set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

########################################################################

gkg_summary_count_may_15_16_2014 <-
  get_data_gkg_days_summary(
    dates = c('2014-05-15', '2014-05-16'),
    is_count_file = T,
    return_message = T
  )

syria_events <- gkg_summary_count_may_15_16_2014[gkg_summary_count_may_15_16_2014$location == "Syria",]
kill_events <- gkg_summary_count_may_15_16_2014[gkg_summary_count_may_15_16_2014$typeEvent == "KILL",]
