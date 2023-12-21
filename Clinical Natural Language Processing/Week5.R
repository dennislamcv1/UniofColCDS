library(tidyverse)
library(magrittr)
library(bigrquery)
library(caret)

con <- DBI::dbConnect(drv = bigquery(),
                      project = "learnclinicaldatascience")
notes <- tbl(con, "course4_data.diabetes_notes") %>% 
  collect()

standards <- tbl(con, "course4_data.diabetes_goldstandard") %>% 
  collect() 

extract_text_window <- function(dataframe, keyword, half_window_size) {
  dataframe %>% 
    group_by(NOTE_ID) %>% 
    mutate(WORDS = SECTION_TEXT) %>% 
    separate_rows(WORDS, sep = "[ \n]+") %>% 
    mutate(INDEX = seq(from = 1, to = n(), by = 1.0),
           WINDOW_START = case_when(INDEX - half_window_size < 1 ~ 1,
                                    TRUE ~ INDEX - half_window_size),
           WINDOW_END = case_when(INDEX + half_window_size > max(INDEX) ~ max(INDEX),
                                  TRUE ~ INDEX + half_window_size),
           WINDOW = word(string = SECTION_TEXT, start = WINDOW_START, end = WINDOW_END, sep = "[ \n]+")) %>% 
    ungroup() %>% 
    filter(str_detect(string = WORDS, pattern = regex(keyword, ignore_case = TRUE)))
}

getStats <- function(df, ...){
  df %>%
    select_(.dots = lazyeval::lazy_dots(...)) %>%
    mutate_all(funs(factor(., levels = c(1,0)))) %>% 
    table() %>% 
    confusionMatrix()
}



######

any_complication <- notes %>% 
  separate_rows(TEXT, sep = "\n\n")%>%
  separate(col = TEXT, into = c("SECTION_TYPE", "SECTION_TEXT"), sep = ":|( ){2}", remove = TRUE, extra = "merge",fill = "left") %>% 
  mutate(SECTION_TYPE = case_when(is.na(SECTION_TYPE) ~ "EXAM",
                                  TRUE ~ SECTION_TYPE)) %>%
  filter(!(SECTION_TYPE %in% c('FAMILY HISTORY','FHX'))) %>%
  mutate(DIABETES = case_when(str_detect(SECTION_TEXT, regex("((diabet)[(ic)(es)])| DM", ignore_case = TRUE)) ~ 1,
                              TRUE ~ 0)) %>%
  filter(DIABETES==1) %>%
  
  extract_text_window(keyword = "((nephr)|(neur)|(retin))(opathy)", half_window_size = 10) %>% 
  mutate(EXCLUDE = case_when(str_detect(WINDOW, regex(pattern = "not? ", ignore_case = TRUE)) ~ 1,
                             TRUE ~ 0)) %>%
  mutate(any_complication = case_when(
    (str_detect(SECTION_TEXT, regex("((nephr)|(neur)|(retin))(opathy)", ignore_case = TRUE)) & (EXCLUDE ==0)) ~ 1,
    TRUE ~ 0))

#####
neuropathy <- notes %>% 
  separate_rows(TEXT, sep = "\n\n")%>%
  separate(col = TEXT, into = c("SECTION_TYPE", "SECTION_TEXT"), sep = ":|( ){2}", remove = TRUE, extra = "merge",fill = "left") %>% 
  mutate(SECTION_TYPE = case_when(is.na(SECTION_TYPE) ~ "EXAM",
                                  TRUE ~ SECTION_TYPE)) %>%
  filter(!(SECTION_TYPE %in% c('FAMILY HISTORY','FHX'))) %>%
  mutate(DIABETES = case_when(str_detect(SECTION_TEXT, regex("((diabet)[(ic)(es)])| DM", ignore_case = TRUE)) ~ 1,
                              TRUE ~ 0)) %>%
  filter(DIABETES==1) %>%
  
  extract_text_window(keyword = "(neuropathy)|(neuropathic)", half_window_size = 10) %>% 
  mutate(EXCLUDE = case_when(str_detect(WINDOW, regex(pattern = "not? ", ignore_case = TRUE)) ~ 1,
                             TRUE ~ 0)) %>%
  mutate(neuropathy = case_when(
    (str_detect(SECTION_TEXT, regex("(neuropathy)|(neuropathic)", ignore_case = TRUE)) & (EXCLUDE ==0)) ~ 1,
    TRUE ~ 0))
#####
nephropathy <- notes %>% 
  separate_rows(TEXT, sep = "\n\n")%>%
  separate(col = TEXT, into = c("SECTION_TYPE", "SECTION_TEXT"), sep = ":|( ){2}", remove = TRUE, extra = "merge",fill = "left") %>% 
  mutate(SECTION_TYPE = case_when(is.na(SECTION_TYPE) ~ "EXAM",
                                  TRUE ~ SECTION_TYPE)) %>%
  filter(!(SECTION_TYPE %in% c('FAMILY HISTORY','FHX'))) %>%
  mutate(DIABETES = case_when(str_detect(SECTION_TEXT, regex("((diabet)[(ic)(es)])| DM", ignore_case = TRUE)) ~ 1,
                              TRUE ~ 0)) %>%
  filter(DIABETES==1) %>%
  
  extract_text_window(keyword = "(nephropathy)|(nephropathic)", half_window_size = 10) %>% 
  mutate(EXCLUDE = case_when(str_detect(WINDOW, regex(pattern = "not? ", ignore_case = TRUE)) ~ 1,
                             TRUE ~ 0)) %>%
  mutate(nephropathy = case_when(
    (str_detect(SECTION_TEXT, regex("(nephropathy)|(nephropathic)", ignore_case = TRUE)) & (EXCLUDE ==0)) ~ 1,
    TRUE ~ 0))
#####

retinopathy <- notes %>% 
  separate_rows(TEXT, sep = "\n\n")%>%
  separate(col = TEXT, into = c("SECTION_TYPE", "SECTION_TEXT"), sep = ":|( ){2}", remove = TRUE, extra = "merge",fill = "left") %>% 
  mutate(SECTION_TYPE = case_when(is.na(SECTION_TYPE) ~ "EXAM",
                                  TRUE ~ SECTION_TYPE)) %>%
  filter(!(SECTION_TYPE %in% c('FAMILY HISTORY','FHX'))) %>%
  mutate(DIABETES = case_when(str_detect(SECTION_TEXT, regex("((diabet)[(ic)(es)])| DM", ignore_case = TRUE)) ~ 1,
                              TRUE ~ 0)) %>%
  filter(DIABETES==1) %>%
  
  extract_text_window(keyword = "(retinopathy)|(retinopathic)", half_window_size = 10) %>% 
  mutate(EXCLUDE = case_when(str_detect(WINDOW, regex(pattern = "not? ", ignore_case = TRUE)) ~ 1,
                             TRUE ~ 0)) %>%
  mutate(retinopathy = case_when(
    (str_detect(SECTION_TEXT, regex("(retinopathy)|(retinopathic)", ignore_case = TRUE)) & (EXCLUDE ==0)) ~ 1,
    TRUE ~ 0))

######

standards %>% 
  left_join(any_complication) %>% 
  mutate(any_complication = coalesce(any_complication, 0)) %>% 
  collect() %>% 
  getStats(any_complication, ANY_DIABETIC_COMPLICATION)


standards %>% 
  left_join(neuropathy) %>% 
  mutate(neuropathy = coalesce(neuropathy, 0)) %>% 
  collect() %>% 
  getStats(neuropathy, DIABETIC_NEUROPATHY)

standards %>% 
  left_join(nephropathy) %>% 
  mutate(nephropathy = coalesce(nephropathy, 0)) %>% 
  collect() %>% 
  getStats(nephropathy, DIABETIC_NEPHROPATHY)

standards %>% 
  left_join(retinopathy) %>% 
  mutate(retinopathy = coalesce(retinopathy, 0)) %>% 
  collect() %>% 
  getStats(retinopathy, DIABETIC_RETINOPATHY)

