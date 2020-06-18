## LOAD DATA AND PACKAGES ####
#source("analyses/initial_data.R") # only do this if running script alone, otherwise leave commented


## COMPUTE POWER FOR ALL PAPERS ####

get_power_oldest = function(df){
  pwr.t.test(n = df$n_dataset, d = df$largest_d, sig.level = 0.05)
}

# Compute oldest paper
oldest = all_data %>%
  group_by(dataset, short_cite) %>%
  summarise(year = max(year),
            largest_d = max(d_calc)) %>%
  ungroup() %>%
  group_by(dataset) %>%
  arrange(year) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Combine summary with oldest paper
d_comparison = inner_join(oldest, MA_summary) %>%
  select(dataset, largest_d, d, n_dataset, power)

# Include power
d_comparison_power = d_comparison %>%
  nest(-dataset, .key = descriptives) %>%
  mutate(power = map(descriptives, get_power_oldest)) %>%
  mutate(old_power = map(power, "power")) %>%
  select(dataset, old_power) %>%
  mutate(old_power = as.numeric(as.character(old_power)))

# Save overall summary
d_comparison_summary = inner_join(d_comparison, d_comparison_power) %>%
  mutate(difference = old_power-as.numeric(power)) %>%
  select(-power)


## PLOT OF DIFFERENCE OF D VALUES ####
# Get data ready for figure
d_comparison_full = inner_join(d_comparison, MA_summary) %>%
  select(dataset, n_dataset, d, largest_d) %>%
  mutate(diff_d = abs(largest_d) - abs(d))

# Make plot
d_comparison_diff.plot = ggplot(d_comparison_full, aes(x = largest_d, y = d)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(colour="grey50", size = 3) +
  geom_point(aes(color = dataset), size = 2) +
  xlab("Largest Cohen's d for Oldest Paper") +
  ylab("Meta-analytic Cohen's d") +
  labs(color = "Meta-analysis") +
  #scale_color_brewer(type = 'div', palette = 'Paired') +
  theme_classic() +
  theme(axis.line.x = element_line(), axis.line.y = element_line(),
        legend.position = "top")



#### Power / ES over time ####



# Very simplistic, just look for a general effect of year.
power_year = rma.mv(d_calc, d_var_calc, mods = ~year, random = ~ short_cite | dataset, data = all_data)

# Make plot


# Alternative analysis taking into account method and age

full.model = rma.mv(d_calc, d_var_calc, mods = ~mean_age_1+method,
                    random = ~ short_cite | dataset, data = all_data)

predicted = predict(full.model)

all_data = all_data %>%
  bind_cols(as.data.frame(predicted$pred),
            as.data.frame(predicted$se)) %>%
  rename(predicted_d = `predicted$pred`, 
         predicted_se = `predicted$se`) 

power_estimate = pwr.t.test(n = all_data$n, d = all_data$predicted_d, sig.level = 0.05)$power

all_data = cbind(all_data, power_estimate)


power_year.plot = ggplot(all_data , aes(x = year, y = power_estimate, color = dataset)) +
  geom_smooth(method = "lm", lwd =1.5) +
  xlab("Publication year") +
  ylab("Estimated Power") +
  scale_color_brewer(type = 'div', palette = 'Paired') +
  theme_classic() +
  theme(axis.line.x = element_line(), axis.line.y = element_line(),
        legend.position = "top")



##### Power per study #####


power_study.plot = ggplot(all_data , aes(x = reorder(dataset, power_estimate, FUN=median), y = power_estimate, color = dataset)) +
  geom_hline(yintercept = .8) +
  geom_boxplot(lwd=1) +
  geom_jitter(size = .5, alpha = .35) +
  xlab("Meta-Analysis") +
  ylab("Estimated Power") +
  scale_color_brewer(type = 'div', palette = 'Paired') +
  theme_classic() +
  theme(axis.line.x = element_line(), axis.line.y = element_line(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "top")



