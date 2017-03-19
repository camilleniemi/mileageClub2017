# This function will create the unique IDs. 
# It is manual in the sense that every year you need to make sure
# the teachers are given a unique ID.

create_IDs <- function(file) {
  require(dplyr)
  
  d <- readxl::read_excel(file) %>%
    select(-`Student #`, -DOB) %>%
    mutate(teacher_unique = ifelse(Teacher %in% c("2-Shirbroun","2-Sparks"),
                                   substr(Teacher, 1, 4),
                                   substr(Teacher, 1, 3)))
  
}

this <- d %>% select(Teacher, teacher_unique) %>% unique
