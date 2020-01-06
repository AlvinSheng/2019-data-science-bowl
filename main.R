
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
library(RVAideMemoire)
library(randomForest)
library(caret)

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





### Data Wrangling ###

# keep only the installation_id's in train with assessment history

train <- train %>% 
  filter(type == "Assessment") %>% 
  distinct(installation_id) %>%
  left_join(train, by = "installation_id")



# Downsampling train 

set.seed(1004)

row_idx <- sample(1:dim(train)[1], size = 1000, replace = FALSE)

train_sub <- train[row_idx,]



# make new column, with TRUE if "correct":true, FALSE if "correct":false, NA otherwise



event_data <- train$event_data

correct <- ifelse(grepl("\"correct\"", event_data), ifelse(grepl("\"correct\":true", event_data), TRUE, FALSE), NA)

train$correct <- correct

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

test$activity_wday <- lubridate::wday(test$timestamp, label = T)

test$activity_hour <- lubridate::hour(test$timestamp)

test$activity_minutes <- lubridate::minute(test$timestamp)


# ### Prediction used the median of accuracy group value for each type of assessment kappa of 0.396 ###
# 
# # arrange test data to select the last assessment attempted
# last_assessment <- test %>% 
#   filter(type == "Assessment") %>% 
#   arrange(installation_id, desc(timestamp)) %>% 
#   distinct(installation_id, .keep_all = T) %>% 
#   select(installation_id, title)
# 
# 
# # create prediction based off median scores
# pred_table <- train_labels %>% 
#   group_by(title) %>% 
#   summarise(accuracy_group = median(accuracy_group, na.rm = T)) %>% ungroup()
# 
# # create the submission dataframe
# submission <- last_assessment %>% 
#   left_join(pred_table, by = "title") %>% 
#   select(-title)
# 
# # write the submission file
# write.csv(submission, "submission.csv", row.names = F)



# Creating variable to conserve historical sequence in summary_stats_pivot





### correlations with some of the features and accuracy group ###

# create summary statistic features using pivot_wider()
summary_stats_pivot.train <- train %>% 
  group_by(installation_id, type) %>% 
  summarise(n_events = n(),
            n_sessions = n_distinct(game_session),
            total_time = sum(game_time, na.rm = T),
            med_time_spent = median(game_time, na.rm = T),
            avg_time_spent = mean(game_time, na.rm = T),
            sd_time_spent = sd(game_time, na.rm = T)) %>% ungroup() %>% 
  pivot_wider(names_from = type, values_from = c(n_events, n_sessions, total_time, med_time_spent, avg_time_spent, sd_time_spent)) %>% 
  dplyr::select(-total_time_Clip, -med_time_spent_Clip, -avg_time_spent_Clip, -sd_time_spent_Clip)

summary_stats_pivot.test <- test %>% 
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
  left_join(summary_stats_pivot.train, by = "installation_id") %>% 
  dplyr::select(-installation_id) %>% 
  as.matrix() %>% 
  na.omit() %>% 
  cor() %>%
  data.frame() %>% 
  mutate(var_name = rownames(.)) %>% 
  dplyr::select(var_name, accuracy_group) %>% 
  arrange(accuracy_group)



train_labels.new <- left_join(train_labels,summary_stats_pivot.train, by = "installation_id")

train_labels.new[is.na(train_labels.new)] <- 0


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





#splitting training data into test and trian

row_idx.labels <- sample(1:dim(train_labels.new)[1], size = 1000, replace = FALSE)

train_labels.test <-train_labels.new[row_idx.labels,] 


train_labels.train<- train_labels.new[-row_idx.labels ,] 


#trying linear discriminant analysis and quadratic discriminant analysis. probably not hte most effiecient way to go, so went with reandom forest. # LDA
lda.fit <- lda(accuracy_group ~ n_events_Clip + n_events_Assessment+n_events_Activity+n_sessions_Game,data=train_labels.train)

lda.fit

   
mva.lda <- MVA.cv(X=train_labels.new$accuracy, Y= as.factor(train_labels.new$accuracy_group),  model=c("LDA"))

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
mean(qda.class == train_labels.test$accuracy_group)


# attempting random forest techniques on classificaiton. seemingly form the results the new fewatures are not as important as the original predictors of num correct, num in correct and accuracy

set.seed(1)

bag.train.labels <- randomForest(as.factor(accuracy_group)~.-(installation_id+game_session+ title),data=train_labels.train, mtry=23, importance=TRUE)

print(bag.train.labels) 

varImpPlot(bag.train.labels, sort = T, n.var=10,main="Top 10 - Variable Importance")

var.imp <-  data.frame(importance(bag.train.labels,  
                                type=2))

var.imp$Variables <-  row.names(var.imp)  

print(var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),])

# Predicting response variable
train_labels.train$predicted.response <-  predict(bag.train.labels , train_labels.train)

# Create Confusion Matrix
print(  
  confusionMatrix(data = train_labels.train$predicted.response ,  
                  reference = as.factor(train_labels.train$accuracy_group)))

# Predicting response variable
train_labels.test$predicted.response <- predict(bag.train.labels ,train_labels.test)

# Create Confusion Matrix
print(  
  confusionMatrix(data=train_labels.test$predicted.response,  
                  reference=as.factor(train_labels.test$accuracy_group)))


#same classification tree, but we did not have the num-correct, num-incorrect, and accuracy of test data. Therefore, i will use the summary features we have above

bag.train.labels.summary <- randomForest(as.factor(accuracy_group)~.-(installation_id+game_session+ title+ accuracy+num_correct+num_incorrect+predicted.response),data=train_labels.train, mtry=20, importance=TRUE)

print(bag.train.labels.summary) 

varImpPlot(bag.train.labels.summary, sort = T, n.var=10,main="Top 10 - Variable Importance")

var.imp <-  data.frame(importance(bag.train.labels.summary,  
                                  type=2))

var.imp$Variables <-  row.names(var.imp)  

print(var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),])

#modified to only include the first four important varible

bag.train.labels.summary <- randomForest(as.factor(accuracy_group)~med_time_spent_Assessment+med_time_spent_Game+sd_time_spent_Assessment+med_time_spent_Activity,data=train_labels.train, mtry=4, importance=TRUE)

print(bag.train.labels.summary) 

# Predicting response variable
train_labels.train$predicted.response <-  predict(bag.train.labels.summary , train_labels.train)

# Create Confusion Matrix
print(  
  confusionMatrix(data = train_labels.train$predicted.response ,  
                  reference = as.factor(train_labels.train$accuracy_group)))

# Predicting response variable
train_labels.test$predicted.response <- predict(bag.train.labels.summary ,train_labels.test)

# Create Confusion Matrix
print(  
  confusionMatrix(data=train_labels.test$predicted.response,  
                  reference=as.factor(train_labels.test$accuracy_group)))




