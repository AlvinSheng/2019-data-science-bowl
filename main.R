
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
library(scales)
library(lubridate)
library(conflicted)
library(MASS)
conflict_prefer("filter", winner = "dplyr", losers = "stats")

# This sets the working directory to that of this file main.R 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()

# set plotting theme baseline
theme_set(theme_minimal() +
            theme(axis.title.x = element_text(size = 15, hjust = 1),
                  axis.title.y = element_text(size = 15),
                  axis.text.x = element_text(size = 12),
                  axis.text.y = element_text(size = 12),
                  panel.grid.major = element_line(linetype = 2),
                  panel.grid.minor = element_line(linetype = 2),
                  plot.title = element_text(size = 18, colour = "grey25", face = "bold"), plot.subtitle = element_text(size = 16, colour = "grey44")))

col_pal <- c("#5EB296", "#4E9EBA", "#F29239", "#C2CE46", "#FF7A7F", "#4D4D4D")




# make sure the folder of data "data-science-bowl-2019" downloaded from Kaggle is in the same directory as this file


train <- data.frame(read_csv('train.csv'))
test <- data.frame(read_csv('test.csv'))

train_labels <- data.frame(read_csv('train_labels.csv'))

specs <- data.frame(read_csv('specs.csv'))
sample_submission <- data.frame(read_csv('sample_submission.csv'))



# train <- read_csv('data-science-bowl-2019/train.csv')
# test <- read_csv('data-science-bowl-2019/test.csv')

# train_labels <- read_csv('data-science-bowl-2019/train_labels.csv')

# specs <- read_csv('data-science-bowl-2019/specs.csv')
# sample_submission <- read_csv('data-science-bowl-2019/sample_submission.csv')





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


set.seed(1004)

row_idx <- sample(1:dim(train_with_assess)[1], size = 1000, replace = FALSE)

train_sub <- train[row_idx,]

# make new column, with TRUE if "correct":true, FALSE if "correct":false, NA otherwise

event_data <- train_with_assess$event_data

correct <- ifelse(grepl("\"correct\"", event_data), ifelse(grepl("\"correct\":true", event_data), TRUE, FALSE), NA)

train_with_assess$correct <- correct



# train <- train %>% 
#   mutate(timestamp = gsub("T", " ", timestamp) %>% gsub("Z", "", .) %>% ymd_hms(),
#          date_stamp = ymd(gsub( " .*$", "", timestamp)),
#          activity_wday = lubridate::wday(timestamp, label = T),
#          activity_hour = lubridate::hour(timestamp))
# # %>% 
#   select(-timestamp)
#sgames are much more frequent than other types of activities. 

train$activity_wday <- lubridate::wday(train$timestamp, label = T)

train$activity_hour <- lubridate::hour(train$timestamp)

train$activity_minutes <- lubridate::minute(train$timestamp)



### Prediction used the median of accuracy group value for each type of assessment kappa of 0.396 ###

# arrange test data to select the last assessment attempted
last_assessment <- test %>% 
  filter(type == "Assessment") %>% 
  arrange(installation_id, desc(timestamp)) %>% 
  distinct(installation_id, .keep_all = T) %>% 
  select(installation_id, title)


# create prediction based off median scores
pred_table <- train_labels %>% 
  group_by(title) %>% 
  summarise(accuracy_group = median(accuracy_group, na.rm = T)) %>% ungroup()

# create the submission dataframe
submission <- last_assessment %>% 
  left_join(pred_table, by = "title") %>% 
  select(-title)

# write the submission file
write.csv(submission, "submission.csv", row.names = F)



### correlations with some of the featrues and accuracy group ###

# create summary statistic features using pivot_wider()
summary_stats_pivot <- train %>% 
  group_by(installation_id, type) %>% 
  summarise(n_events = n(),
            n_sessions = n_distinct(game_session),
            total_time = sum(game_time, na.rm = T),
            med_time_spent = median(game_time, na.rm = T),
            avg_time_spent = mean(game_time, na.rm = T),
            sd_time_spent = sd(game_time, na.rm = T)) %>% ungroup() %>% 
  pivot_wider(names_from = type, values_from = c(n_events, n_sessions, total_time, med_time_spent, avg_time_spent, sd_time_spent)) %>% 
  dplyr::select(-total_time_Clip, -med_time_spent_Clip, -avg_time_spent_Clip, -sd_time_spent_Clip)



# join the summary statistics to train labels and calculate correlations
train_labels %>% 
  dplyr::select(installation_id, accuracy_group) %>% 
  left_join(summary_stats_pivot, by = "installation_id") %>% 
  dplyr::select(-installation_id) %>% 
  as.matrix() %>% 
  na.omit() %>% 
  cor() %>%
  data.frame() %>% 
  mutate(var_name = rownames(.)) %>% 
  dplyr::select(var_name, accuracy_group) %>% 
  arrange(accuracy_group)



train_labels.new <- left_join(train_labels,summary_stats_pivot, by = "installation_id")



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





# LDA

train_labels.test <-train_labels.new[1:1000,] 


train_labels.train<- train_labels.new[1001:length(train_labels.new$installation_id),] 

lda.fit <- lda(accuracy_group ~ n_events_Clip + n_events_Assessment+n_events_Activity+n_sessions_Game,data=train_labels.train)
lda.fit
plot(lda.fit)
lda.pred <- predict(lda.fit ,train_labels.test )
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class ,train_labels.test$accuracy_group)
mean(lda.class == train_labels.test$accuracy_group)


qda.fit <- qda(accuracy_group ~ n_events_Clip + n_events_Assessment+n_events_Activity+n_sessions_Game,data=train_labels.train)
qda.fit
qda.class <- predict(qda.fit ,train_labels.test)$class
table(qda.class ,train_labels.test$accuracy_group)
mean(qda.class == Direction.2005)