# This file caches the data to be used in the synthesis paper. This is useful 
# because the data in the repository change frequently, and not all MAs in 
# metalab are reported in the paper. This file reads in the data that has been
# cached by the scripts/cache_datasets.R script, and caches a second version of 
# the data to be used for the paper in ../paper_data/. To run this script, from main
# dir: Rscript write-ups/synthesis_paper/paper_scripts/cache_paper_data.R
# Thus, when data has been updated in the spreadsheets, two scripts need to be 
# run in order to include it in the paper (cache_datasets.R and cache_paper_data.R).

library(tidyverse)
library(forcats)
library(feather)

# MAs in synthesis paper
MAS <- c("idspref", "labadv", "gaze_following", "inphondb-native", 
         "inphondb-nonnative", "phonotactics", "word_recognition",
         "mutex", "pointing_concurrent", "sounds", "inworddb", "symbolism")

# read in all_data
source("dashboard/global.R", chdir = TRUE)

########################  ALL_DATA ########################   
# remove inappropriate conditions and rename MAs
synthesis_paper_all_data <- all_data %>%
  filter(short_name %in% MAS) %>%
  filter(is.na(condition_type) | condition_type == "critical") %>% # remove control conditions from labadv
  filter(infant_type == "typical") %>%
  filter(coder != "Alvaro Iturralde") %>% # can update to new data later
  mutate(dataset = fct_recode(dataset,
                              "Pointing and vocabulary" =
                                "Pointing and vocabulary (concurrent)"))

# write all_data to feather
write_feather(synthesis_paper_all_data, "write-ups/synthesis_paper/paper_data/synthesis_paper_all_data.feather")

########################    DATSET METADATA ########################   
# need to recompute datasets summary data, based on filtered all_data
synthesis_paper_studies <- synthesis_paper_all_data %>%
  group_by(dataset) %>%
  summarise(num_experiments = n(),
            num_papers = length(unique(study_ID)))

synthesis_paper_subjects <- synthesis_paper_all_data %>%
  rowwise() %>%
  mutate(n_total = sum(c(n_1, n_2), na.rm = TRUE)) %>%
  group_by(dataset, study_ID, same_infant) %>% # we want all unique participants (i.e. exclude those who are in more than 1 condition (same same_infant value), but include conditions where is.na(same_infant))
  mutate(id = 1:n()) %>%
  filter(is.na(same_infant) | id == 1) %>%
  select(-id) %>%
  group_by(dataset) %>%
  summarise(num_subjects = sum(n_total))

synthesis_paper_datasets <- datasets %>%
  rename(dataset = name) %>%
  select(-num_experiments, -num_papers, -num_subjects, -moderators) %>%
  filter(short_name %in% MAS) %>% 
  mutate(dataset = fct_recode(dataset, "Pointing and vocabulary" =
                                "Pointing and vocabulary (concurrent)")) %>%
  left_join(synthesis_paper_studies) %>% # merge in filtered meta-data
  left_join(synthesis_paper_subjects) %>%
  rename(name = dataset)

# write datasets to feather
write_feather(synthesis_paper_datasets, "write-ups/synthesis_paper/paper_data/synthesis_paper_datasets.feather")
