library(here)
library(readxl)
library(tidyverse)
library(ggplot2)


data <- readr::read_csv(here("data/mri.csv"))

data <- data%>% mutate(smoking_label = case_when((sex == 'Female' & (packyrs != 0)) ~ 'Female smoker',
                                     (sex == 'Female' & (packyrs == 0)) ~ 'Female non-smoker',
                                     (sex == 'Male' & (packyrs != 0)) ~ 'Male smoker',
                                     (sex == 'Male' & (packyrs == 0)) ~ 'Male non-smoker'))

data <- data %>% mutate(reveal = case_when((smoking_label == "Female smoker") ~ 1,
                                           (smoking_label == "Female non-smoker") ~ 2,
                                           (smoking_label == "Male smoker") ~ 3,
                                           (smoking_label == "Male non-smoker") ~ 4))

#data %>% sample_frac(.1) -> 
 # data
data <- na.omit(data)
write_csv(data, here::here("data/final_data_mri.csv"))
