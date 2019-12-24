
library(rstudioapi)
library(tools)
library(dplyr)
library(readr)
library(jsonlite)
library(data.table)
library(shiny)
library(purrr)
# This sets the working directory to that of this file main.R 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Reading in all the files
train <- data.frame(read_csv("train.csv"))
test <- data.frame(read_csv("test.csv"))
train_labels <- data.frame(read_csv("train_labels.csv"))
specs <- data.frame(read_csv("specs.csv"))
sample_submission <- data.frame(read_csv("sample_submission.csv"))














