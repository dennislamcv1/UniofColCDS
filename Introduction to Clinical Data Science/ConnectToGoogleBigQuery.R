## Run this script to connect to Google BigQuery the first time you log in or start a new project. 
## Make sure that popups are enabled. 
library(dplyr)
library(bigrquery)

con <- DBI::dbConnect(drv = bigquery(),
                      project = "learnclinicaldatascience")
patients <- tbl(con, "mimic3_demo.PATIENTS")

# When you run this line, the Console below will as if you want to:
######## Use a local file ('.httr-oauth'), to cache OAuth access credentials between R sessions?
######## 
######## 1: Yes
######## 2: No
## Enter 1 in the console to store a local file. 
## This will open a pop-up that asks you to login to or select your Google account. 
## Once you have logged in, Google will give you a long authorization code. 
## Copy that code and paste it into the console.
## Congratulations! You can now access Google BigQuery from any analysis done in this folder/project. 