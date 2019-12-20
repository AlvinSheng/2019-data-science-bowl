
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





