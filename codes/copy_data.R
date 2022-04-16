getwd() %>% 
  str_replace("automobile-survival", "diesel-cars/data") %>% 
  list.files(full.names = TRUE) %>% 
  walk(~ {
    new_file <- gsub(".*data/", "data/", .)
    print(new_file)
    file.copy(., new_file)
  })