
# Find summary statistics on specific titles rather than types

title_pivot_train <- train %>% 
  group_by(installation_id, title) %>% 
  summarise(n_events = n()) %>% ungroup() %>% 
  pivot_wider(names_from = title, values_from = c(n_events))

title_pivot_train[is.na(title_pivot_train)] <- 0

train_labels.new <- left_join(train_labels, title_pivot_train, by = "installation_id")


