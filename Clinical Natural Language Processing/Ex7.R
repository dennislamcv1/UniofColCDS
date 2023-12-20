library(tidyverse)
library(magrittr)
library(bigrquery)

con <- DBI::dbConnect(drv = bigquery(),
                      project = "learnclinicaldatascience")
discharge_summaries <- tbl(con, "course4_data.discharge_summaries") %>% 
  collect()

extract_text_window <- function(dataframe, keyword, half_window_size) {
  dataframe %>% 
    group_by(NOTE_ID) %>% 
    mutate(WORDS = TEXT) %>% 
    separate_rows(WORDS, sep = "[ \n]+") %>% 
    mutate(INDEX = seq(from = 1, to = n(), by = 1.0),
           WINDOW_START = case_when(INDEX - half_window_size < 1 ~ 1,
                                    TRUE ~ INDEX - half_window_size),
           WINDOW_END = case_when(INDEX + half_window_size > max(INDEX) ~ max(INDEX),
                                  TRUE ~ INDEX + half_window_size),
           WINDOW = word(string = TEXT, start = WINDOW_START, end = WINDOW_END, sep = "[ \n]+")) %>% 
    ungroup() %>% 
    filter(str_detect(string = WORDS, pattern = regex(keyword, ignore_case = TRUE)))
}

discharge_summaries %>% 
  extract_text_window(keyword = "hypertension", half_window_size = 8) %>% 
  distinct(NOTE_ID)