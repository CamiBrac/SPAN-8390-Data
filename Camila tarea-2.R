
##  SPAN 8390 Data Cleaning in R:


# set working directory
setwd("~/Documents/R8390") #this is one way for R to know where to look for and create files
getwd()


# load packages
#nb: if you don't have a specific package, you'll need to install it: install.packages('packageName')
library(qualtRics)             #package that helps deal with Qualtrics output
library(tidyverse)             #family of packages including dplyr (data cleaning)
library(readr)                 #read in csv files
library(readxl)                #read in excel files
library(writexl)               #write .xlsx files
library(psych)                 #for describe() and describeBy() functions for numeric variables
library(here)                  #here package circumvents issues with set wd
library(janitor)

 # load data with qualtRics::read_survey.  
ajt <- read_survey(("Code Switching Acceptability_February 19, 2025_06.49.csv"))

#nb: another option is read_csv() and skip argument, but I won't use this
ajt <- read_csv("Code Switching Acceptability_February 19, 2025_06.49.csv")

 # inspect data using head(), str(), summary()
View(ajt)
head(ajt) #gives top 6 rows
summary(ajt)

 # reshape/clean data using dplyr functions like select, filter, rename, and mutate
ajt <-  read_survey("Code Switching Acceptability_February 19, 2025_06.49.csv") %>%
  filter(Finished == TRUE) %>%  #retains rows that meet condition
  select(`Duration (in seconds)`, Finished, ResponseId, contains("Q")) %>% #retain only variables of interest (all the questions will start with Q)
  drop_na(Q8) %>%       #removes any rows that have missing values on a column (probably just want to use this for a variable you expect everyone to have completed)
  rename(consent = Q4) %>%       #rename as 'consent' the original column 'Q4'
  rename(id = Q8) %>%       #rename as 'consent' the original column 'Q4'
  mutate(consent = case_when(consent == "Yes - begin the experiment!" ~ 1)) %>% # 1-to-1 recoding values
  arrange('Duration (in seconds)') %>%         #finally, let's sort df by variables that help us make sense of our data
  janitor::clean_names()

ajt <- ajt %>% 
  rename(eng_gram_nonCS_1 = q7_1) %>%
  rename(spa_deque_nonCS_2 = q7_2) %>%
  rename(eng_gram_CS_intra_3 = q7_3) %>%
  rename(spa_gram_CS_intra_4 = q7_4) %>%
  rename(spa_ungram_CS_clV_5 = q7_5) %>%
  rename(eng_gram_CS_conglex_6 = q7_6) %>%
  rename(eng_ungram_nonCS_whatthat_7 = q7_7) %>%
  rename(spa_ungram_nonCS_inf_8 = q7_8) %>%
  rename(eng_ungram_CS_SV_9 = q7_9) %>%
  rename(eng_ungram_CS_intra_ag_10 = q7_10) %>%
  rename(eng_gram_CS_dialogue_11 = q7_11) %>% 
  rename(email = q10)

ajt <-ajt %>% filter(str_detect(email, 'osu.edu')) %>% 
  select(6:18)

#nb: use case for case_when function
#The left-hand side of the tilde is any statement that creates a logical vector.
#If it is TRUE, then the value is replaced with the one on the right-hand side.
#If it is FALSE, the next logical statement is checked. 
#If all statements are false, it will get the .default value (which defaults to NA).

 # write an excel file and save it in your project folder
writexl::write_xlsx(ajt, here("ajt.xlsx"), col_names =TRUE) ###google how to save to directory folder without using here code


#good reference for recoding row values (1 to 1, many to one, complex)
#https://debruine.github.io/post/case_functions/

##here is a base R way to semi-manually remove rows/columns you don't want:
#survey1 <- survey[-c(1:2),-c(1:22)] 


 # your bilingual survey data 
 # load, save as an object you call 'survey'
 ##inspect/clean and write to a new .xlsx as in the example 

### your code here

survey <- read_csv("SurveyData.csv")
View(survey)
head(survey) #gives top 6 rows
summary(survey)

# reshape/clean data using dplyr functions like select, filter, rename, and mutate
survey <-  read_survey("SurveyData.csv") %>%
  filter(Finished == TRUE) %>%  #retains rows that meet condition
  select(`Duration (in seconds)`, Finished, ResponseId, contains("Q")) %>% #retain only variables of interest (all the questions will start with Q)
  janitor::clean_names() %>% 
  rename(string = response_id) %>% 
  select(-q3_5_text) %>% 
  select(-q9_5_text)



survey <- survey %>% mutate(id = case_when(
  string == 'R_1kp3SD9VdRcqiGY' ~ "German Alcaraz",
  string == 'R_1ZukRALeQo4ZXKF' ~ "Laura Trenta",
  string == 'R_6I4DAcdRXhZ5BLY' ~ "Lucas Rubin",
  string == 'R_3GThMVWpCgx1VSY' ~ "Mariana Aguilar",
  string == 'R_6RdBzlQDOjSuhiF' ~ "Victoria Cataloni",
  string == 'R_5j04olMIXX1807o' ~ "Esteban Benalcazar")
  )



 # merging datasets using a unique key (like participant name or ID number)
 # nb: good reference: https://dplyr.tidyverse.org/reference/mutate-joins.html
combined <- left_join(survey, ajt, by = "id")

# write to local folder
writexl::write_xlsx(combined, "combined.xlsx", col_names =TRUE)
