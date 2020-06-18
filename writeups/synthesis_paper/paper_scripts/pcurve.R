# This code is heavily adopted from Simonsohn, Simmons and Nelson (2014a, 2014b, 2015) supplementary materials
# Note: the original code handled several different test statistics; this code only handles F

######################## HELPER FUNCTIONS #################################
# pbound: bound p-values and pp-values by precision of measurement to avoid errors
pbound <- function(p) pmin(pmax(p, 2.2e-16), 1-2.2e-16)

# stouffer: stouffer test for a vector of pp-values
stouffer <- function(pp) sum(qnorm(pp), na.rm = TRUE) / sqrt(sum(!is.na(pp)))

# get_ncp: finds non-centrality parameter for f distribution that gives some level of power
get_ncp <- function(df1, df2, power, ALPHA) {    
  ncp_error = function(ncp_est, power, x, df1, df2) {pf(x, df1 = df1, df2 = df2, ncp = ncp_est) - (1 - power)} # function uniroot funcitons over
  xc = qf(p = 1-ALPHA, df1 = df1, df2 = df2) 
  root <- uniroot(ncp_error, c(0, 35), x = xc, df1 = df1, df2 = df2, power = power)$root
  root
}

# get_all_pc_data: computes f, df, p, pp, and ncp33 values. transform flag determines whether p-values are calculated from descriptive statistics (TRUE).
get_all_pc_data <- function(df, ALPHA, P_INCREMENT, transform){
  
  df %>% # convert rs and ts to fs, then get p-values
    mutate(f.value = ifelse(!is.na(r), (r ^ 2 * (n_1 -2))/ (1 - r ^ 2), F)) %>% # from Sirkin ("Statistics for the Social Sciences", pg. 505)
    mutate(f.value = ifelse(is.na(f.value) & !is.na(t), t**2, f.value)) %>%
    mutate(N_total = ifelse(is.na(n_2), n_1*2, n_1 + n_2),
           f.transform = ifelse(is.na(f.value), ((d_calc/2)^2) * N_total, NA)) %>% # convert missing Fs from d_calc
    mutate(f.value = ifelse(is.na(f.value) & transform, f.transform, f.value)) %>%
    filter(!is.na(f.value)) %>%
    mutate(df2 = ifelse(participant_design == "between", (n_1 + n_2)-2, n_1-1),
           df1 = 1,
           p = pbound(1 - pf(f.value, df1 = df1, df2 = df2)), # recompute ps from df and bind to level of precision desired
           p_round = (ceiling(p /P_INCREMENT) * P_INCREMENT)) %>%
    rowwise %>% # need to do this rowwise because get_ncp is not vectorized
    mutate(ncp33 = get_ncp(df1, df2, power = 1/3, ALPHA)) %>%
    ungroup() %>%
    mutate(ppr.full = as.numeric(ifelse(p < ALPHA, p/ALPHA, NA)),  #compute pp-values (prob of observing a significant p value at least as extreme if the null were true)if p<.05, ppr is 1/alpha*p-value, so 20*pvalue, otherwise missing. (essentially normalize probability by alpha) - assumes p values uniformly distributed?
           ppr.half = as.numeric(ifelse(p < ALPHA/2, p/(ALPHA/2), NA)),
           pp33.full = ifelse(p < ALPHA , 3 * (pf(f.value, df1, df2, ncp = ncp33) - 2/3), NA),
           prop25 = 3 * prop33(ALPHA/2, ncp33, df1, df2, ALPHA), # share of p-values expected to be p<.025 if 33% power
           pp33.half = ifelse(p < ALPHA/2, (1 / prop25) * (pf(f.value, df1, df2, ncp = ncp33) - (1 - prop25)), NA)) %>%
    mutate_each(funs(pbound), c(ppr.full, ppr.half, pp33.full, pp33.half)) %>%
    select(dataset,study_ID, d_var_calc, d_calc, p, p_round, participant_design, r_calc, df1, df2, f.value,f.transform, ncp33, ppr.full, ppr.half, pp33.full, pp33.half)
}



# prop33: computes % of p-values that are smaller than critical p, for the tests submitted to p-curve, if power is 33%
# creates a vector of the same length as the number of tests submitted to p-curve, significant and not,
#    and computes the proportion of p-values expected to be smaller than {pc} given the df
#    and outputs the entire vector, with NA values where needed
prop33 <- function(pc, ncp33, df1, df2, ALPHA) {
  prop = 1 - pf(qf(1 - pc, df1 = df1, df2 = df2), df1 = df1, df2 = df2, ncp = ncp33)
}

# power_fit_f : create pp-values for how well a given power_est fits, and do KSD test 
power_fit_f <- function(df, power_est, ALPHA){
  ksd_value = df %>%
    filter(p < ALPHA & f.value > 0) %>%
    rowwise() %>%
    mutate(ncp_est = ifelse(!is.na(df1), get_ncp(df1, df2, power_est, ALPHA), NA)) %>%
    ungroup() %>%
    mutate(p_larger = pf(f.value, df1, df2, ncp_est),  # prob f>fobs given ncp_est
           ppr = (p_larger-(1-power_est))/power_est) %>%  # condition on p<.05
    summarise(ksd = ks.test(ppr, punif)$statistic) %>% # Kolmogorov-Smirnov test on the resulting pprs
    unlist() 
  ksd_value
}

# es_fit_t: loss function for effect size estimation based on Kolmogorov-Smirnov test
es_fit_t <- function(df, d_est, ALPHA) { 
  ksd_value = df %>%
    select(f.value, df2) %>%
    ungroup() %>%
    filter(p < ALPHA, f.value > 0) %>%
    mutate(ncp_est = sqrt((df2 + 2)/4) * d_est,
           tc = qt(1-(ALPHA/2), df2),
           power_est = 1 - pt(tc, df2, ncp_est),
           p_larger = pt(sqrt(f.value), df = df2, ncp = ncp_est),
           ppr = (p_larger - (1 - power_est)) / power_est) %>%  
    summarise(ksd = ks.test(ppr, punif)$statistic) %>%
    unlist()
  ksd_value 
}

# one_es_sample: get one es sample
one_es_sample <- function(df) {
  function(k) {
    sampled.df = sample_n(df, nrow(df), replace = TRUE) 
    optimize(es_fit_t, c(-.3, 2), df = sampled.df, ALPHA = ALPHA)$minimum
  }
}

# boot_es: get many es samples and get sumary stats
boot_es <- function(df, nboot) {
  sample_values <- 1:nboot %>%
    map(one_es_sample(df)) %>%
    unlist()
  data.frame(d = mean(sample_values),
             ci_lb = ci_lower(sample_values),
             ci_ub = ci_upper(sample_values),
             row.names = NULL)
}

######################## PRIMARY FUNCTIONS #################################

# get_p_curve_df: data for pcurve plot
get_p_curve_df <- function(df, ALPHA, P_INCREMENT){
  
  # get observed and baseline pcurve data
  pc_data <- data.frame(p = seq(P_INCREMENT, ALPHA, P_INCREMENT)) %>%
    group_by(p) %>%
    mutate(observed = sum(df$p_round == p) / sum(df$p < ALPHA),  # observed line
           baseline =  P_INCREMENT/ALPHA)  # baseline
  
  # get expected p-curve for 33% power data (i.e., proportion of tests expected to get <01, <02...)
  gcdf = seq(P_INCREMENT, ALPHA, P_INCREMENT) %>%
      map(function(x) prop33(x, df$ncp33,  df$df1,  df$df2, ALPHA))
  
  # Now compute difference, and divide by 1/3 to get the share of significant p-values in each bin  
  expected33 = c()
  for (i in 1:length(gcdf)){ 
    if (i == 1) {
      expected33[i] = mean(gcdf[[i]])*3 # average of the vector p<.01
    } else {
      expected33[i] = mean(gcdf[[i]]-gcdf[[i-1]])*3  # difference between .02 and .01
    }
  }
  pc_data$expected33 = expected33
  
  pc_long <- pc_data %>% 
    gather(measure, value, observed, baseline, expected33)
  
  pc_long
}

# stouffer_test: stouffer method for evaluating skew.
stouffer_test <- function(df, ALPHA){
    df %>%
      select(ppr.full, pp33.full, ppr.half, pp33.half) %>%
      gather("pp.measure", "pp.value", 1:4) %>%
      group_by(pp.measure) %>%
      summarise(Z.pp = stouffer(pp.value)) %>%
      mutate(p.Z.pp = pnorm(Z.pp),
             sig = p.Z.pp < ALPHA)  
}

# get_pc_power: estimate best fitting power
get_pc_power <- function(df, ALPHA) {
  optimize(power_fit_f, c(.06, .999), df = df, ALPHA = ALPHA)$minimum 
}

# get_pc_es: estimate best fitting es with CI
get_pc_es <- function(d, ALPHA){
  NBOOT = 100
  d %>%
    do(boot_es(., NBOOT)) %>% 
    mutate(method = "p curve")
}