
# Find summary statistics on specific titles rather than types

title_pivot_train <- train %>% 
  group_by(installation_id, title) %>% 
  summarise(n_events = n()) %>% ungroup() %>% 
  pivot_wider(names_from = title, values_from = c(n_events))

title_pivot_train[is.na(title_pivot_train)] <- 0

train_labels.new <- left_join(train_labels, title_pivot_train, by = "installation_id")



title_pivot_test <- test %>% 
  group_by(installation_id, title) %>% 
  summarise(n_events = n()) %>% ungroup() %>% 
  pivot_wider(names_from = title, values_from = c(n_events))

title_pivot_test[is.na(title_pivot_test)] <- 0

train_labels.new <- left_join(train_labels, title_pivot_test, by = "installation_id")



# Indicator of whether child has completed all assigned media before given assessment



# If child didn't finish assigned media, what media types they did



# If child did finish all the assigned media, did they do them in order
# sort by world, see what order they went in



# Regex for "(Correct)" and "(Incorrect)" in specs.csv



# Regex for "Drag", "Drop", 



# Count the number of each event code before each assessment






