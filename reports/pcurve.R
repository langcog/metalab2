###############################################################################
# get_ncp - functions that find non-centrality parameter for f,chi distributions that gives some level of power
# we don't use chi-square / z-scores, so I only add the getncp for F scores (t will be turned to F in this version of p-curve)
ncp_error = function(ncp_est, power, x, df1, df2) 
{  
  pf(x, df1 = df1, df2 = df2, ncp = ncp_est) - (1 - power)   
}

get_ncp <- function(df1, df2, power)   
{      
  xc = qf(p=.95, df1 = df1, df2 = df2) 
  root <- uniroot(ncp_error, c(0, 35), x = xc, df1 = df1, df2 = df2, power = power)$root
  
  return(root)  
}

###############################################################################
# prop33(pc) - Computes % of p-values that are smaller than pc, for the tests submitted to p-curve, if power is 33%
prop33 <- function(pc, ncp33, df1, df2)
{
  #pc: critical  p-value
  
  #Overview:
  #Creates a vector of the same length as the number of tests submitted to p-curve, significant and not,
  #    and computes the proportion of p-values expected to be smaller than {pc} given the d.f. 
  #    and outputs the entire vector, with NA values where needed
  
  #F-tests (& thus  t-tests)
  prop= (1 - pf(qf(1 - pc, df1=df1, df2=df2), df1=df1, df2=df2, ncp=ncp33))
  
  #output it
  return(prop)
}

###############################################################################
# pbound: bound p-values and pp-values by precision of measurement to avoid errors
pbound <- function(p) 
{
  pmin(pmax(p,2.2e-16),1-2.2e-16)
}


###############################################################################
# p_curve 
# main function that does the doing. 
p_curve <- function(df){

  # Recompute Ps and bound to level of precision desired
  df <- df %>%
    mutate(p = pbound(1 - pf(value, df1 = df1, df2 = df2)),
           p_round = ceiling(p * 100) / 100) %>%
    rowwise %>% # need to do this rowwise because get_ncp is not vectorized
    # NCP33 (noncentrality parameter giving each test in p-curve 33% power given the d.f. of the test)
    mutate(ncp33 = get_ncp(df1, df2, power=1/3))
  
  pc_data <- data.frame(p = seq(.01,.05,.01)) %>%
    group_by(p) %>%
    mutate(observed = sum(df$p_round == p) / sum(df$p < .05), 
           baseline = 1/5)

  # Green line (Expected p-curve for 33% power)
  # Proportion of tests expected to get <01, <02...
  gcdf1=prop33(.01, df$ncp33, df$df1, df$df2)         #vector with proportion of p-values p<.01, with 33% power
  gcdf2=prop33(.02, df$ncp33, df$df1, df$df2)         #              ""                   p<.02,      "
  gcdf3=prop33(.03, df$ncp33, df$df1, df$df2)         #              ""                   p<.03,      "
  gcdf4=prop33(.04, df$ncp33, df$df1, df$df2)         #              ""                   p<.04,      "
  #Note: for p<.05 we know it is 33% power
  
  #5.1.2 Now compute difference, and divide by 1/3 to get the share of significant p-values in each bin      
  pc_data$expected <- c(mean(gcdf1)*3,        #Average of the vector p<.01
                        mean(gcdf2-gcdf1)*3,  #Difference between .02 and .01
                        mean(gcdf3-gcdf2)*3,  #Difference between .03 and .02
                        mean(gcdf4-gcdf3)*3,  #Difference between .04 and .03
                        mean(1/3-gcdf4)*3)    #Difference between .05 and .04
  #Because we have one row per test submitted, the average is weighted average, giving each test equal weight
  pc_long <- pc_data %>% 
    gather(measure, value, observed, baseline, expected)
  
  return(pc_long)
}
