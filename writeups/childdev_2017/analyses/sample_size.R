## LOAD DATA AND PACKAGES ####
#source("analyses/initial_data.R") # only do this if running script alone, otherwise leave commented


## GET DESCRIPTIVE INFORMATION #####
# Make functions - This function is a hack, should update to just use map with MA_descriptives
get_power = function(df){
  pwr.t.test(n = df$n_dataset, d = df$d, sig.level = 0.05)
}

# Organize data to get descriptors
data_rma = all_data %>%
  nest(-dataset, .key = information) %>%
  mutate(model = map(information, ~rma.mv(d_calc, d_var_calc, random = ~ study_ID, data=.))) %>%
  mutate(d = map(model, "b")) %>%
  mutate(se = map(model, "se"))  %>%
  select(dataset, d, se) %>%
  mutate(d = as.numeric(as.character(d))) %>%
  mutate(se = as.numeric(as.character(se)))

# Descriptors
MA_descriptives = all_data %>%
  mutate(n_total = n) %>% #ifelse(!is.na(n_2), n_1 + n_2, n_1)) %>% I think n does the same thing
  group_by(dataset) %>%
  summarise(age_dataset = median(mean_age_months),
            age_min = min(mean_age_months),
            age_max = max(mean_age_months),
            n_dataset = median(n_total),
            n_min = min(n_total),
            n_max = max(n_total),
            n_records = n(),
            n_papers = length(unique(short_cite))) %>%
  ungroup() %>%
  inner_join(data_rma)

# Power
MA_power = MA_descriptives %>%
  nest(-dataset, .key = descriptives) %>%
  mutate(power = map(descriptives, get_power)) %>%
  mutate(power = map(power, "power")) %>%
  select(dataset, power) %>%
  mutate(power = as.numeric(as.character(power)))

# Summary
MA_summary = inner_join(MA_descriptives, MA_power) 

MA_summary_table = MA_summary %>%
  mutate(age = paste(as.character(round(age_dataset, 0)), " (", as.character(round(age_min, 0)), "-", as.character(round(age_max, 0)), ")", sep = "")) %>%
  mutate(n = paste(as.character(n_dataset), " (", as.character(n_min), "-", as.character(round(n_max, 0)), ")", sep = "")) %>%
  mutate(ES = paste(as.character(round(d, 2)), " (", as.character(round(se, 2)), ")", sep = "")) %>%
  select(dataset, age, n, n_records, n_papers, ES, power)


### DATA AVAILABLITY ####
# #From report Reproducibility by ML and CB
# counts = all_data %>%
#     mutate(corr_within_two  = ifelse(participant_design == "within_two", as.numeric(corr), NA)) %>% # creates a new column where any time design is not within_two "corr" becomes an NA
#   summarise(test_statistic = sum(!is.na(t) | !is.na(F) | !is.na(r)),
#                       means = sum(!is.na(x_1)),
#                       SD = sum(!is.na(SD_1)),
#                       d = sum(!is.na(d)),
#                       #corr = sum(!is.na(corr)), 
#                       corr_within_two = sum(!is.na(corr_within_two)),
#                       mean_age = sum(!is.na(mean_age_1)),
#                       age_range = sum(!is.na(age_range_1)),
#                       gender = sum(!is.na(gender_1))) %>%
#   gather("coded_variable", "n_coded") %>%
#   #mutate(total = nrow(all_data))  %>%
#   mutate(total = ifelse(coded_variable == "corr_within_two",
#                         nrow(subset(all_data, participant_design == "within_two")),
#                                     nrow(all_data))) %>%
#   mutate(coded_variable = factor(coded_variable)) %>%
#   mutate(n_uncoded = total - n_coded) %>%
#   select(coded_variable, n_coded, n_uncoded, total)
