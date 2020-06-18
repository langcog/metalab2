### Helper functions for synthesis paper ###

# get MA effect size (random effect or mixed-effect [multilevel = T]) 
overall_es <- function(ma_data, multilevel){
  if (multilevel){
    model = metafor::rma.mv(d_calc, V = d_var_calc,
                            random = ~ 1 | short_cite, data = ma_data)
  } else {
    model = metafor::rma(d_calc, d_var_calc, data = ma_data)
  }
  data.frame(dataset = ma_data$short_name[1],
             overall.d = model$b,
             ci_lower = model$ci.lb,
             ci_upper = model$ci.ub)
}


# Fail safe N
get_fail_safe_N <- function(dataset, ma_data, targ) {
  fsn_string = fsn(d_calc, d_var_calc, data = ma_data, target = targ, type = "Orwin")$fsnum 
  data.frame(dataset = dataset, fsn_string = fsn_string)
}

## Eggers test for funnel assymetry
eggers_tests <- function(ma_data){
  model = rma(d_calc, d_var_calc, data = ma_data) # model (mixed-effect model not implemented for eggers test)
  egg.random = regtest(model) # Egger's test
  data.frame(dataset = ma_data$short_name[1],
             egg.random.z = egg.random$zval,
             egg.random.p = egg.random$pval)
}


## gets CIs on p-curves
get_all_CIS_multi <- function(df, alpha, increment) {
  ps <- seq(increment, alpha, increment)
  props = ps %>%
    map(function(p,d){sum(d == p)}, df$p_round) %>%
    unlist()
  cis = MultinomialCI::multinomialCI(props, alpha = alpha)
  data.frame(dataset = df$dataset[1],
             p = ps,
             ci.lower = cis[,1],
             ci.upper = cis[,2])
}
