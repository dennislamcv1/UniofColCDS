library(tidyverse)
library(magrittr)
library(bigrquery)
library(dplyr)


con <- DBI::dbConnect(drv = bigquery(),
                      project = "learnclinicaldatascience")
discharge_summaries <- tbl(con, "course4_data.discharge_summaries") %>% 
collect()

## Question 2

distinct_sections <- discharge_summaries %>%
  separate_rows(TEXT, sep = "\n\n") %>%
  separate(col = TEXT, into = c("SECTION_TYPE", "SECTION_TEXT"), sep = ":", remove = TRUE, extra = "merge") %>%
  distinct(SECTION_TYPE)

# Number of distinct note sections
num_distinct_sections <- nrow(distinct_sections)

print(num_distinct_sections)


## Question 4
discharge_summaries %>% 
  separate_rows(TEXT, sep = "\n\n") %>% 
  separate(col = TEXT, into = c("SECTION_TYPE", "SECTION_TEXT"), sep = ":", remove = TRUE, extra = "merge") %>% 
  filter(str_detect(string = SECTION_TYPE, pattern = regex("medications", ignore_case = TRUE))) 

# Assuming discharge_summaries is your data frame

unique_medication_headers <- discharge_summaries %>%
  separate_rows(TEXT, sep = "\n\n") %>%
  separate(col = TEXT, into = c("SECTION_TYPE", "SECTION_TEXT"), sep = ":", remove = TRUE, extra = "merge") %>%
  filter(str_detect(string = SECTION_TYPE, pattern = regex("medications", ignore_case = TRUE))) %>%
  distinct(SECTION_TYPE)

# Number of unique section headers describing medications
num_unique_medication_headers <- nrow(unique_medication_headers)

print(num_unique_medication_headers)

# Question 6

# Assuming discharge_summaries is your data frame

num_notes_lisinopril <- discharge_summaries %>%
  separate_rows(TEXT, sep = "\n\n") %>%
  separate(col = TEXT, into = c("SECTION_TYPE", "SECTION_TEXT"), sep = ":", remove = TRUE, extra = "merge") %>%
  filter(str_detect(string = SECTION_TEXT, pattern = regex("Lisinopril", ignore_case = TRUE)))

# Number of notes describing the patient taking Lisinopril
num_notes_lisinopril <- nrow(num_notes_lisinopril)

print(num_notes_lisinopril)
