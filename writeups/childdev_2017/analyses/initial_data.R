## READ IN DATA ####
source("../../dashboard/global.R", chdir = TRUE)



## LOAD PACKAGES ####
library(metafor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(purrr)
library(knitr)
library(broom)
library(pwr)
library(lme4)


## CLEAN DATA ####
all_data = all_data %>%
  filter(is.na(condition_type) | condition_type == "critical") %>%
  filter(dataset!="Pointing and vocabulary (longitudinal)") %>%
  filter(dataset!="Categorization Bias") %>%
  filter(dataset!="Statistical Word Segmentation") %>% #added AC 2017-10-19
  filter(infant_type == "typical") %>%
  mutate(weights_d = 1/(d_var_calc)^2) 





all_data = all_data %>%
  mutate(year = as.numeric(unlist(lapply(strsplit(unlist(study_ID),
                                                  "[^0-9]+"),  function(x) unlist(x)[2])))) %>%
  mutate(year = ifelse(grepl("submitted",study_ID), 2017, year)) %>%
  mutate(year = ifelse(dataset == "Phonotactic learning", 
                       as.numeric(unlist(lapply(strsplit(unlist(short_cite),"[^0-9]+"),  function(x) unlist(x)[2]))), year)) %>%
  mutate(dataset = as.factor(dataset),
         dataset = plyr::revalue(dataset, 
                                 c("Infant directed speech preference"="IDS preference",
                                   "Statistical sound category learning"="Statistical sound learning", 
                                   "Label advantage in concept learning"="Concept-label advantage",
                                   "Vowel discrimination (native)"="Native vowel discrim.",
                                   "Vowel discrimination (non-native)"="Non-native vowel discrim." ,
                                   "Pointing and vocabulary (concurrent)"="Pointing and vocabulary"
                                 )))

#Remove outliers

clean_data = all_data %>%
  group_by(dataset) %>%
  mutate(mean_es = median(d_calc)) %>%
  mutate(sd_es = sd(d_calc)) %>%
  ungroup() %>%
  mutate(no_outlier = ifelse(d_calc < mean_es+3*sd_es, ifelse(d_calc > mean_es-3*sd_es, TRUE, FALSE), FALSE))  %>%
  filter(no_outlier) 

#Comment out if you do not want to remove outliers
all_data = clean_data
remove(clean_data)
