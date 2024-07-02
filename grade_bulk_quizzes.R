
library(tidyverse)
library(janitor)

# ------------------------------ User Parameters ------------------------------
# ***** NOTE *****
# Please add options for the strings below to make this work for your use file system

# Specify the location where coarser quiz summaries are stored
quiz_directory <- ""
# Specify the location where detailed quiz data are stored
question_directory <- ""
# Set due date for quizzes
deadline <- as.Date("2024/06/28")
# Set filepath where output grade CSV will be written
output_filepath <- ""

# ------------------------------ Quiz-level Summaries ------------------------------
# These files have a single dataframe for each quiz which includes coarse information
# This does not distinguish between students who answered a question wrong and those who did not attempt it

# Get list of files within target directory
quiz_files <- list.files(quiz_directory, full.names = TRUE)
# Retain only the files which end in '.csv'
quiz_files <- quiz_files[grepl(".csv", quiz_files)]

# Load all the files from .csv - initially this generates a list of data frames
all_quiz_results <- lapply(quiz_files, 
                           function(filepath){ 
                             return(read_csv(filepath, 
                                             col_types = cols()))
                           })
# Reformat to a single data frame with all the quiz dat
all_quiz_results <- bind_rows(all_quiz_results) %>%
  janitor::clean_names()



# ------------------------------ Question-level Summaries ------------------------------
# These files have a single dataframe for each question, with multiple per quiz
# Notably, this tracks which student attempted which question, so it allows computing completion grades

# Get list of files within target directory
question_files <- list.files(question_directory, full.names = TRUE)
# Retain only the files which end in '.csv'
question_files <- question_files[grepl(".csv", question_files)]

# Load all the files from .csv - initially this generates a list of data frames
all_question_results <- lapply(question_files, 
                               function(filepath){ 
                                 new_data <- read_csv(filepath, 
                                                 col_types = cols())
                                 new_data$filepath <- filepath
                                 new_data$filename <- basename(filepath) 
                                 new_data$filename <- substr(new_data$filename, 1, nchar(new_data$filename)-4)
                                 new_data$quiz_index <- as.numeric(str_split((new_data$filename)[[1]], "_")[[1]][[2]])
                                 new_data$question_index <- as.numeric(str_split((new_data$filename)[[1]], "_")[[1]][[4]])
                                 date_string_list <- str_split(substr(new_data$'Quiz Start Time', 1, 9), "/")
                                 new_data$month <- unlist(lapply(date_string_list,
                                                                 function(date_str_list){
                                                                   return(as.numeric(date_str_list[[1]]))
                                                                 }))
                                 new_data$day <- unlist(lapply(date_string_list,
                                                                 function(date_str_list){
                                                                   return(as.numeric(date_str_list[[2]]))
                                                                 }))
                                 new_data$year <- unlist(lapply(date_string_list,
                                                                 function(date_str_list){
                                                                   return(as.numeric(date_str_list[[3]]))
                                                                 }))
                                 new_data$date <- lubridate::ymd(paste(new_data$year,
                                                                       new_data$month,
                                                                       new_data$day,
                                                                       sep="/"))
                                 return(new_data)
                               })
# Reformat to a single data frame with all the quiz data
all_question_results <- bind_rows(all_question_results) %>%
  janitor::clean_names()


# ------------------------------ Generate Summary Data and Output to Disk ------------------------------

# Generate Summaries by Quiz
gradebook_summary <- all_question_results %>% 
  group_by(user_display_name, 
           quiz_index) %>% 
  summarize(total_attempts = n(), 
            correct_attempts = sum(correct),
            on_time_attempts = sum(date <= deadline),
            on_time_correct_attempts = sum(correct*(date <= deadline))) %>% 
  group_by(quiz_index) %>% 
  mutate(fraction_correct = on_time_correct_attempts / total_attempts, 
         fraction_attempted = on_time_attempts/max(total_attempts)) 
gradebook_summary %>% View()
# Add surname field
names <- gradebook_summary$user_display_name
surnames <- str_split(names, " ")
surnames <- unlist(lapply(surnames,
                          function(str_list){ return(str_list[[2]]) }))
gradebook_summary$surname <- surnames

# Write Output

write_csv(gradebook_summary %>% arrange(quiz_index, surnames), output_filepath, append = FALSE)