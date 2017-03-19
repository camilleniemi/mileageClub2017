# This function will create the unique IDs. 
# It is manual in the sense that every year you need to make sure
# the teachers are given a unique ID.

file <- "Fellows Mileage Club 2_21_17.xlsx"

readxl::read_excel(file) %>%
  select(-`Student #`, -DOB) %>%
  rename(last_name = `Last name`,
         first_name = `First Name`,
         middle_name = `Middle Name`,
         nickname = Nickname) %>%
  mutate(teacher_unique = ifelse(Teacher %in% c("2-Shirbroun","2-Sparks"),
                                 substr(Teacher, 1, 4),
                                 substr(Teacher, 1, 3))) %>%
  filter(teacher_unique != "Hom") %>% 
  arrange(teacher_unique, 
          last_name,
          first_name) %>%
  mutate(ID = 100+(1:nrow(.))) %>%
  
  write.csv(file="id.csv", row.names = FALSE)