
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



# convert event_data JSON column to data.table

train_event_data <- train_with_assess$event_data %>% head(22) %>%
  lapply(function(x) fromJSON(gsub('""', "\"", x))) %>%
  rbindlist( fill =TRUE)

#only if need to iinteractively change the data table.
# train_event_data %>% head %>% DT::datatable()
# map(train_event_data, c( "event_code")) %>% DT::datatable() 
# specs_arguments <- specs$args %>%
#   lapply(function(x) fromJSON(gsub('""', "\"", x))) %>%
#   rbindlist( fill =TRUE)



set.seed(1004)

row_idx <- sample(1:dim(train_with_assess)[1], size = 1000, replace = FALSE)

train_sub <- train[row_idx,]



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

train$date_stamp <-  ymd( train$timestamp)

train %>% 
  count(type) %>% 
  ggplot(aes(x= reorder(type,n), y= n)) +
  geom_col(colour = "grey", fill = col_pal[5], alpha = 0.8) +
  scale_y_continuous(labels = comma) +
  labs(x= "Type", y= "Count of Events") +
  coord_flip() +
  ggtitle("GAMES HAVE THE MOST INTERACTIONS", subtitle = "Clips and Assessments by far the least events")


train %>%
  group_by(title, type) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  group_by(type) %>% 
  slice(1:10) %>% 
  ggplot(aes(x= reorder(title,n), y= n)) +
  geom_col(fill = col_pal[5], colour = "grey", alpha = 0.8) +
  labs(x= "Title", y= "Count") +
  ggtitle("TOP TITLES IN EACH ACTIVITY", subtitle = "Assessments fairly equally attempted") +
  coord_flip() +
  facet_wrap(~ type, scales = "free") +
  theme(strip.text = element_text(size = 12, face = "bold"))

# not sure why it is said to be twin peaks for the full data though 
a <- train %>% count(installation_id, sort = T) %>% 
  ggplot(aes(x= n)) + geom_histogram(fill = col_pal[5], colour = "grey") +
  ggtitle("SOME USERS HAVE A HIGH NUMBER OF EVENTS", subtitle = "Count of events are heavily skewed by some users") +
  labs(x= "Count of Events", y= "Number")

b <- train %>% count(installation_id, sort = T) %>% 
  ggplot(aes(x= log1p(n))) + geom_histogram(fill = col_pal[5], colour = "grey") +
  ggtitle("TWIN PEAKS IN NUMBER OF EVENTS DISTRIBUTION", subtitle = "Count of events have been log transformed") +
  labs(x= "Log1p(Count of Events)", y= "Number")

gridExtra::grid.arrange(a, b)

a <- train %>% 
  ggplot(aes(x= log1p(game_time))) +
  geom_density(fill = col_pal[5], colour = "grey", alpha = 0.5) +
  ggtitle("TIME SPENT IS HEAVILY SKEWED", subtitle = "A large number of events have time = 0")


b <- train %>% 
  filter(type != "Clip") %>% 
  ggplot(aes(x= log1p(game_time), fill = type)) +
  geom_density(colour = "grey", alpha = 0.5) +
  scale_fill_manual(values = col_pal, name = "Type") +
  ggtitle("USERS SPENDING MORE TIME ON GAMES,\nLEAST ON ASSESSMENTS", subtitle = "'Clips' have been excluded - all have time = 0") +
  theme(legend.position = "top")

gridExtra::grid.arrange(a, b)

train %>%
  group_by(title, type) %>% 
  summarise(median_time_spent = median(game_time, na.rm = T)) %>% 
  arrange(desc(median_time_spent)) %>% head(20) %>% 
  ggplot(aes(x= reorder(title, median_time_spent), y= median_time_spent, fill = type)) +
  geom_col(colour = "grey", alpha = 0.8) +
  scale_fill_manual(values = col_pal) +
  scale_y_continuous(labels = comma) +
  labs(x= "Title", y= "Median Time (milliseconds)", fill = "Type") +
  ggtitle("MOST TIME SPENT ON ACTIVITY AND\nGAME TITLES", subtitle = "The top 20 highest median time spent on titles") +
  theme(legend.position = "top") +
  coord_flip()

train_labels_sub <- train_labels[row_idx,]

train_labels%>% 
  count(accuracy_group) %>% 
  ggplot(aes(x= accuracy_group, y= n)) +
  geom_col(fill = col_pal[5], colour = "grey", alpha = 0.8) +
  labs(x= "Accuracy Group", y= "Count") +
  scale_y_continuous(labels = comma) +
  ggtitle("MORE ATTEMPTS SOLVED ON THE FIRST ATTEMPT (ACCURACY GROUP 3)", subtitle = "Never solved was the second most frequent outcome") +
  coord_flip()

train_labels %>% 
  count(accuracy_group, title) %>% 
  ggplot(aes(x= title, y= n, fill = as.character(accuracy_group))) +
  geom_bar(stat = "identity", position = "fill", colour = "grey") +
  labs(x= "Title", y= "Count") +
  scale_fill_manual(values = col_pal, name = "Accuracy\nGroup") +
  scale_y_continuous(labels = percent) +
  ggtitle("'CHEST SORTER' THE HARDEST ASSESSMENT?", subtitle = "Over 60% of Chest Sorter attempts never solved, while others\nhave been solved closer to 60% of the time on the first attempt") +
  coord_flip() +
  theme(legend.position = "top")

train_labels %>% 
  ggplot(aes(y= accuracy, x= title, fill = title)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = col_pal, name = "Title") +
  scale_y_continuous(labels = percent, name = "Accuracy") +
  ggtitle("CHILDREN DEFINITELY PERFORM THE WORST ON\nCHEST SORTER", subtitle = "The distribution of accuracy confirms the easiest and hardest assessments") +
  theme(axis.title.y = element_blank()) +
  coord_flip()

train_labels %>% 
  mutate(attempts = num_correct + num_incorrect) %>% 
  ggplot(aes(x= attempts, y= accuracy)) + 
  geom_point(position = "jitter", alpha = 0.2, colour = col_pal[5]) +
  ggtitle("MORE ATTEMPTS = LESS ACCURACY?", subtitle = "Appears to be a negative relationship between\nthe number of attempts and accuracy") +
  labs(x= "Attempts", y= "Accuracy") +
  scale_y_continuous(labels = percent)

a <- train %>% 
  count(date_stamp) %>% 
  ggplot(aes(x= date_stamp, y= n)) +
  geom_line(colour = col_pal[5]) +
  scale_y_continuous(labels = comma, name = "Number of Events") +
  labs(x= "Event Date") +
  ggtitle("DAILY NUMBER OF EVENTS PEAKED LATE SEPTEMBER", subtitle = "A fairly steady increase from August to start\nof October, then fell away")

b <- train %>% 
  count(activity_hour) %>% 
  ggplot(aes(x= as.factor(activity_hour), y= n, group = 1)) +
  geom_line(colour = col_pal[5], size = 1) +
  geom_point(colour = col_pal[6]) +
  scale_y_continuous(labels = comma, name = "Number of Events") +
  labs(x= "Event Hour") +
  ggtitle("MOST EVENTS OCCUR BETWEEN 2-8PM", subtitle = "A significant dip through the morning hours")

c <- train %>% 
  count(activity_wday) %>% 
  ggplot(aes(x= as.factor(activity_wday), y= n)) +
  geom_col(fill = col_pal[5], colour = col_pal[6], alpha = 0.8) +
  scale_y_continuous(labels = comma, name = "Number of Events") +
  labs(x= "Event Day") +
  ggtitle("MOST EVENTS OCCUR FRIDAY AND THURSDAY", subtitle = "Wednesdays and Sundays the lowest number of events")

gridExtra::grid.arrange(a, b, c)

train_assessments <- train %>%
  filter(event_code %in% c(4100, 4110))

# create a success or failure variable using the text in event data
train_assessments <- train_assessments %>% 
  mutate(success_or_fail = ifelse(str_detect(event_data, 'correct"":false'), "fail", ifelse(str_detect(event_data, 'correct"":true'), "success", "other"))) %>% 
  select(-event_data)

train_assessments %>% 
  filter(type == "Assessment", success_or_fail != "other") %>% 
  ggplot(aes(x= activity_wday, fill = success_or_fail)) +
  geom_bar(stat = "count", position = "fill", colour = col_pal[6]) + 
  scale_fill_manual(values = col_pal[c(3,1)]) +
  scale_y_continuous(labels = percent, name = "Success Rate") +
  labs(x= "Assessment Day") +
  ggtitle("NOT MUCH DIFFERENCES IN SUCCESS RATES\nDEPENDING ON DAY OF WEEK", subtitle = "Mondays slightly less successful than other days") +
  theme(legend.position = "top", legend.title = element_blank())

train_assessments %>% 
  filter(type == "Assessment", success_or_fail != "other") %>% 
  ggplot(aes(x= activity_hour, fill = success_or_fail)) +
  geom_bar(stat = "count", position = "fill", colour = col_pal[6]) + 
  scale_fill_manual(values = col_pal[c(3,1)]) +
  scale_y_continuous(labels = percent, name = "Success Rate") +
  scale_x_continuous(breaks = seq(0,24,4), name = "Assessment Hour") +
  ggtitle("THERE ARE SOME DIFFERENCES IN SUCCESS RATES\nDEPENDING ON TIME OF ASSESSMENT", subtitle = "Between 9-10pm and 11pm-12am worst success") +
  theme(legend.position = "top", legend.title = element_blank())


### Prediction used the median of accuracy group value for each ype of assessmentm kappa of 0.396###

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
  select(-total_time_Clip, -med_time_spent_Clip, -avg_time_spent_Clip, -sd_time_spent_Clip)


# join the summary statistics to train labels and calculate correlations
train_labels %>% 
  select(installation_id, accuracy_group) %>% 
  left_join(summary_stats_pivot, by = "installation_id") %>% 
  select(-installation_id) %>% 
  as.matrix() %>% 
  na.omit() %>% 
  cor() %>%
  data.frame() %>% 
  mutate(var_name = rownames(.)) %>% 
  select(var_name, accuracy_group) %>% 
  arrange(accuracy_group)
