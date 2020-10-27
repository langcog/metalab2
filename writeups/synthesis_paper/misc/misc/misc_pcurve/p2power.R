
####################################################################################################################################
# (STEP 1) CREATE FUNCTIONS THAT FIND THE NONCENTRALITY PARAMETER FOR THE t,F, associated with a given power, for the observed d.f.
#
#SET OF FUNCTIONS 1. 
#COMPUTE GAP BETWEEN POWER AND DESIRED POWER FOR A GIVEN NCP 
# (minimize these in the next step to solve for the ncp that gives the desired power)
    ncp_error.f = function(delta, power, x, df1,df2) pf(x, df1 = df1, df2=df2, ncp = delta) - (1-power)   

#SET OF FUNCTIONS 2: MINIMIZE FUNCTIONS ABOVE
  #F-test
    getncp.f =function(df1,df2, power)   {      
      xc=qf(p=.95, df1=df1,df2=df2) # critical F-value
      return(uniroot(ncp_error.f, c(0, 37.62), x = xc, df1 = df1,df2=df2, power=power)$root)  }
    

####################################################################################################################################
# (STEP 2) CREATE PP-VALUES FOR EACH OF THE FOUR DISTRIBUTIONS FOR HOW WELL A GIVEN POWER_EST FITS 
  powerfit.f=function(f_obs, df1_obs, df2_obs, power_est)    {
            ncp_est=mapply(getncp.f,df1=df1_obs, df2=df2_obs,power=power_est)  #find ncp for each  that gives each test power.k
            p_larger=pf(f_obs,df1=df1_obs,df2=df2_obs, ncp=ncp_est)        #prob t>tobs given ncp_est
            ppr=(p_larger-(1-power_est))/power_est          #condition on p<.05
            return(ppr) 
    }

####################################################################################################################################
# (STEP 3) STACK-UP ALL THE PP-VALUES INTO A VECTOR
  powerfit.all=function(power_est){
    ppr.all=c()
     #for each kind of test, check if there are any significant values, if there are, add ppr to overall ppr
      if (length(f.value.sig)>0) ppr.all=c(ppr.all, powerfit.f(f_obs=f.value.sig, df1_obs=f.df1.sig, df2_obs=f.df2.sig, power_est=power_est))

      KSD=ks.test(ppr.all,punif)$statistic                #KS test on the resulting pprs
      return(KSD)
     }
  
##############################################
#EXAMPLE: F(2,120)=4.8
f.value.sig=4.8
f.df1.sig=2
f.df2.sig=120

optimize(powerfit.all,c(.06,.999))$minimum ## estimate best fitting value

