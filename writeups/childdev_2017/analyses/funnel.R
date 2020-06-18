#### LOAD DATA AND PACKAGES ####
#source("analyses/initial_data.R") # only do this if running script alone, otherwise leave commented



#### Adapted from the synthesis paper ####
CRIT_95 = 1.96 

funnel.es.data = all_data %>%
  mutate(dataset = as.factor(dataset)) %>%
  group_by(dataset) %>%
  #mutate(outlier = ifelse(d_calc > mean(d_calc)  + (3 * sd(d_calc)) | d_calc < mean(d_calc)  - (3 * sd(d_calc)), 1, 0),
  #      outlier = as.factor(outlier)) %>%
  #filter(outlier == 0) %>%
  mutate(se = sqrt(d_var_calc), 
         es = g_calc, 
         center = median(d_calc), 
         lower_lim = max(se) + .05 * max(se))


# separate df for 95 CI funnel shape
funnel95.data.wide <- funnel.es.data %>%
  select(center, lower_lim, dataset) %>%
  group_by(dataset) %>%
  summarise(x1 = (center-lower_lim * CRIT_95)[1], 
            x2 = center[1],
            x3 = center[1] + lower_lim[1] * CRIT_95,
            y1 = -lower_lim[1],
            y2 =  0, 
            y3 = -lower_lim[1]) 

funnel95.data.x = funnel95.data.wide  %>%
  select(dataset,  dplyr::contains("x")) %>%
  gather("coordx", "x", 2:4) %>%
  arrange(dataset, coordx) %>%
  select(-coordx)

funnel95.data.y = funnel95.data.wide  %>%
  select(dataset, dplyr::contains("y")) %>%
  gather("coordy", "y", 2:4) %>%
  arrange(dataset, coordy) %>%
  select(-coordy)

funnel95.data = bind_cols(funnel95.data.x, funnel95.data.y)

funnel_grid <- ggplot(funnel.es.data, aes(x = es, y = -se)) +
  facet_wrap(~dataset, scales = "free") +
  xlab("Effect Size Cohen's d")  +
  ylab("Standard Error")  +
  geom_polygon(aes(x = x, y = y), 
               data = funnel95.data,
               fill = "lightgrey") +
  geom_smooth(method = 'lm', se = F, colour = "darkgrey") +
  geom_vline(aes(xintercept=x2), 
             linetype = "dashed", color = "red", size = .5, data = funnel95.data.wide) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black",  size = .5) +
  scale_y_continuous(labels = function(x){abs(x)}) +
  geom_point(size = .5) +
  theme_classic()



#### Testing for funnel plot asymmetry ####

data_funnel = all_data %>%
  nest(-dataset, .key = information) %>%
  mutate(model = map(information, ~ranktest(rma.mv(d_calc, d_var_calc, random = ~ study_ID, data=.)))) %>%
  mutate(p = map(model, "pval")) %>%
  mutate(tau = map(model, "tau"))  %>%
  select(dataset, tau, p) %>%
  mutate(p = as.numeric(as.character(p))) %>%
  mutate(p = ifelse(p < .001, "< .001", as.character(round(p, 3)))) %>%
  mutate(tau = as.numeric(as.character(tau))) 