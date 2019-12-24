
# run main.R first



# Exploring train_labels.csv

# how many unique installation_id's does it have?
length(unique(train_labels$installation_id)) # 3614

# does that correspond to the number in train/test? 
length(unique(train$installation_id)) # 17000
length(unique(test$installation_id)) # 1000
# no

# are the installation_id's gathered from the train set or the test set or both?
all(train_labels$installation_id %in% train$installation_id) # TRUE
# so it's just a sampling of all the installation_id's in the train set



# Exploring specs.csv

# dimensions of file
dim(specs)

# how many unique event_id's are in this file?
length(unique(specs$event_id)) # 386

# does it cover all possible event_id's in train and test?
length(unique(train$event_id)) # 384
length(unique(test$event_id)) # 365
# Yes; it even covers event_id's not in the train or test set



# Exploring train

# What are event_code's and their relation with event_id? 
length(unique(train$event_code))
length(unique(test$event_code))

# table of event_code's
table(train$event_code)

# how many "\"correct\"" assessments are there?
sum(grepl("\"correct\"", train_with_assess$event_data)) # 621068

# how many of them are actually correct?
sum(grepl("\"correct\":true", train_with_assess$event_data)) # 389376

# how many of them are actually incorrect?
sum(grepl("\"correct\":false", train_with_assess$event_data)) # 231692

# Do "\"correct\"" only appear with assessments? No, they also appear in Game as well!



# summary of how many attempts every unique combination of game_session and installation_id had on assessments
attempt_summary <- train_with_assess %>% filter(type == "Assessment") %>% group_by(game_session, installation_id) %>% 
  summarise(num_attempt = sum((title != "Bird Measurer (Assessment)" & event_code == "4100") | (title == "Bird Measurer (Assessment)" & event_code == "4110")))

# How many unique combinations of game_session and installation_id made at least one attempt on the assessment?
sum(attempt_summary$num_attempt != 0) # 17690

# This matches the number of rows in train_labels!



# Exploring test

# dimensions
dim(test)

# there are 1000 unique installation_id's. How many assessments are in the data set?
sum(test$type == "Assessment") # 102627

# how do we find the rows of the assessments that we need to predict for?
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

# Is there a more efficient way to do the above?





