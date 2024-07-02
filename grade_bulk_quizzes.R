
library(tidyverse)
library(janitor)

# ------------------------------ Quiz-level Summaries ------------------------------
# These files have a single dataframe for each quiz which includes coarse information
# This does not distinguish between students who answered a question wrong and those who did not attempt it

# Specify the location where coarser quiz summaries are stored
quiz_directory <- ""
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
# Reformat to a single data frame with all the quiz data
all_quiz_results <- bind_rows(all_quiz_results) %>%
  janitor::clean_names()



# ------------------------------ Question-level Summaries ------------------------------
# These files have a single dataframe for each question, with multiple per quiz
# Notably, this tracks which student attempted which question, so it allows computing completion grades

# Specify the location where detailed quiz data are stored
question_directory <- ""
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
                                 print(new_data$filename)
                                 new_data$quiz_index <- as.numeric(str_split((new_data$filename)[[1]], "_")[[1]][[2]])
                                 new_data$question_index <- as.numeric(str_split((new_data$filename)[[1]], "_")[[1]][[4]])
                                 return(new_data)
                               })
# Reformat to a single data frame with all the quiz data
all_question_results <- bind_rows(all_question_results) %>%
  janitor::clean_names()


# Generate Summaries by Quiz
all_question_results %>% 
  group_by(user_display_name, 
           quiz_index) %>% 
  summarize(total_attempts = n(), 
            correct_attempts = sum(correct)) %>% 
  group_by(quiz_index) %>% 
  mutate(fraction_correct = correct_attempts / total_attempts, 
         fraction_attempted = total_attempts/max(total_attempts)) %>% 
  View()


