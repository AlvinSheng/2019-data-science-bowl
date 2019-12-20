
library(rstudioapi)
library(drake)
library(tools)
library(dplyr)

source("wrangle.R")

# This sets the working directory to that of this file main.R 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data_folder <- "data-science-bowl-2019"
data_files <- list_files_with_exts(data_folder, "csv")



wrangle_plan <- drake_plan(
  
  dat = read_dat(data_files), 
  train = dat$train,
  test = dat$test
  
)

plan <- rbind(wrangle_plan)

config <- drake_config(plan)

vis_drake_graph(config)



make(plan)

history <- drake_history(analyze = TRUE)

cache <- drake_cache()










