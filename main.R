
# you may need to install.packages("pkg_name") first

library(rstudioapi)
library(tools)
library(dplyr)
library(jsonlite)
library(data.table)
library(drake)
library(tidyverse)
library(shiny)
library(purrr)

library(conflicted)

conflict_prefer("filter", winner = "dplyr", losers = "stats")

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



### Data Wrangling ###

# keep only the installation_id's in train with assessment history

train_with_assess <- train %>% 
  filter(type == "Assessment") %>% 
  distinct(installation_id) %>%
  left_join(train, by = "installation_id")



# Downsampling train_with_assess 

set.seed(1004)

row_idx <- sample(1:dim(train_with_assess)[1], size = 1000, replace = FALSE)

train_sub <- train[row_idx,]



# make new column, with TRUE if "correct":true, FALSE if "correct":false, NA otherwise

event_data <- train_with_assess$event_data

correct <- ifelse(grepl("\"correct\"", event_data), ifelse(grepl("\"correct\":true", event_data), TRUE, FALSE), NA)

train_with_assess$correct <- correct



## Preparing test for prediction.

# Finding the rows of assessments that we need to predict for

pred_assess_idx <- rep(NA, dim(test)[1])

for (i in 1:dim(test)[1]) {
  
  if (test$type[i] == "Assessment") {
    if (i == dim(test)[1]) {
      pred_assess_idx[i] <- TRUE
    }
    else if (test$installation_id[i] != test$installation_id[i+1]) {
      pred_assess_idx[i] <- TRUE
    }
    else {
      pred_assess_idx[i] <- FALSE
    }
  }
  else {
    pred_assess_idx[i] <- FALSE
  }
  
}

test_no_labels <- test[pred_assess_idx,]



# Enhancing train_labels

# First step is to align train_labels and test_no_labels








