
library(rstudioapi)
library(tools)
library(dplyr)

# This sets the working directory to that of this file main.R 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Reading in all the files
train.data <- data.frame(read_csv("train.csv"))
test.data <- data.frame(read_csv("test.csv"))
train.label <- data.frame(read_csv("train_labels.csv"))
















