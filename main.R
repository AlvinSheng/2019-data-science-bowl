
library(rstudioapi)
library(tools)
library(dplyr)

# This sets the working directory to that of this file main.R 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Reading in all the files
data_folder <- "data-science-bowl-2019"

data_files <- list_files_with_exts(data_folder, "csv")

data_names <- data_files %>% basename() %>% file_path_sans_ext()

dat <- lapply(data_files, read.csv)

names(dat) <- data_names



# Extracting the required datasets
train <- dat$train
test <- dat$test

sample_submission <- dat$sample_submission
specs <- dat$specs
train_labels <- dat$train_labels
















