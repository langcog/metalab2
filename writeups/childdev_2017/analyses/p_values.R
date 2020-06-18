#### LOAD DATA AND PACKAGES ####
#source("analyses/initial_data.R") # only do this if running script alone, otherwise leave commented


#### Recompute p-values ####
all_data = all_data %>%
  mutate(t_calc = ifelse(is.na(t), (r_calc/(sqrt((1-(r_calc^2))/(n-2)))), t)) %>%
  mutate(dfs = ifelse(participant_design=="between", (n_1+n_2-2), (n_1-1))) %>%
  mutate(p_calc = 2*pt(abs(t_calc), df=dfs, lower = FALSE)) %>%
  mutate(sig = ifelse(p_calc < .05, TRUE, FALSE))


#plotting effect sizes against p-values to display their relationship

sig_plot <- ggplot(all_data, aes(p_calc, abs(d_calc)),group_by(sig)) +
  geom_hline(yintercept = 1, size = 8, colour = "lightgrey") +
  geom_vline(xintercept = .05) +
  geom_point(size = all_data$n/20, alpha = .5) +
  xlab("Recalculated p-value") +
  ylab("Absolute value of Cohen's d") +
  ylim(0, 2) +
  theme_classic() +
  theme(axis.line.x = element_line(), axis.line.y = element_line())


#### P-curve ####

p_data = all_data %>%
  filter(p_calc < .05) %>%
  group_by(dataset) %>%
  mutate(number = n()) %>%
  mutate(n_below = sum(p_calc<.05))%>%
  mutate(n_above = sum(p_calc>.05))%>%
  mutate(percent_below = n_below/number) %>%
  ungroup() %>%
  filter(number > 10) 

pcurve.plot <- ggplot(p_data, aes(p_calc)) +
  facet_wrap(~dataset, scales = "free") +
  geom_vline(xintercept = .05) +
  geom_histogram(breaks = seq(0, 0.05,by=0.01)) +
  geom_density(adjust = .5) +
  xlim(0, .05) +
  xlab("p-value (recalculated)")  +
  ylab("Counts")  +
  theme_classic()

