
# you may need to install.packages("pkg_name") first

library(rstudioapi)
library(tools)
library(dplyr)
library(jsonlite)
library(data.table)
library(drake)
library(tidyverse)

# This sets the working directory to that of this file main.R 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()

# make sure the folder of data "data-science-bowl-2019" downloaded from Kaggle is in the same directory as this file

train <- read_csv('data-science-bowl-2019/train.csv')
test <- read_csv('data-science-bowl-2019/test.csv')

train_labels <- read_csv('data-science-bowl-2019/train_labels.csv')

specs <- read_csv('data-science-bowl-2019/specs.csv')
sample_submission <- read_csv('data-science-bowl-2019/sample_submission.csv')

# # Reading in all the files
# data_folder <- "data-science-bowl-2019"
# 
# data_files <- list_files_with_exts(data_folder, "csv")
# 
# data_names <- data_files %>% basename() %>% file_path_sans_ext()
# 
# dat <- lapply(data_files, read_csv)
# 
# names(dat) <- data_names
# 
# 
# 
# # Extracting the required datasets
# train <- dat$train
# test <- dat$test
# 
# sample_submission <- dat$sample_submission
# specs <- dat$specs
# train_labels <- dat$train_labels




#######

## convert event_data JSON column to data.table
train_event_data <- train$event_data %>%
  lapply(function(x) fromJSON(gsub('""', "\"", x))) %>%
  rbindlist( fill =TRUE)

train_event_data %>% head %>% DT::datatable()















