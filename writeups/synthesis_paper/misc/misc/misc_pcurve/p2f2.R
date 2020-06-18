
#R Code behind Figure 5A&B in -  CHOICE OVERLOAD
#Simonsohn, Simmons and Nelson,  "P-Curve and Effect Size: Correcting for Publication Bias Using Only Significant Results"
#
#Prepared by Uri Simonsohn, uws@wharton.upenn.edu
#
#Last updated: 2014 05 13
##########################################################


#Function 1; loss function for how well expected p-curve fits observed p-curve
loss=function(t_obs,df_obs,d_est) {
  ncp_est=sqrt((df_obs+2)/4)*d_est                          
  tc=qt(.975,df_obs)                     
  power_est=1-pt(tc,df_obs,ncp_est)        
  p_larger=pt(t_obs,df=df_obs,ncp=ncp_est)
  ppr=(p_larger-(1-power_est))/power_est  
  KSD=ks.test(ppr,punif)$statistic        
  return(KSD)          
  }


#Function 2: Simplified bootstrapping formula
  sub = function(df) return(df[sample(nrow(df),nrow(df),replace=TRUE),])  #take a dataframe and generates another of the same size - used for bootstrapping SE

#Function 3: variance of a cohen d estimate
  vd=function(d,df) 
  {
  n=(df+2)/2
  return(2/n +(d**2)/(2*(n-3.94)))
  }

  
#EFFECT SIZE ESTIMATION PER P-CURVE
#Choice is bad
  # THESE ARE THE REPORTED RESULTS USING IN THE META-ANALYSIS
    #tbad=c(6.0752,3.6016,2.6336,2.7725,2.5614,2.4787,2.3607,2.1793,2.1768,2.1597,2.0911,2.0552,2.0025) 
    #dfbad=c(247,65,58,19,38,58,41,84,63,67,113,163,67)
  
  # CORRECTING ERROR WITH JAMS STUDY (barely affects p-curves point estimate, makes estimates more precise, makes naive smaller)
  # the meta-analysis uses an effect size that suffers from selection bias for it counts only people who stopped
  # and stopping is affected by treatment. This was discovered while Uri taught paper in PhD class and seemed appropriate to correct
  # again, makes very little difference;
    tbad=c(4.5548,3.6016,2.6336,2.7725,2.5614,2.4787,2.3607,2.1793,2.1768,2.1597,2.0911,2.0552,2.0025)
    dfbad=c(753,65,58,19,38,58,41,84,63,67,113,163,67)

  #Choice is good
    tgood=c(4.0093,3.1529,3.0129,2.8302,2.4666,2.3193,2.1967)
    dfgood=c(16,43,52,87,88,30,41)

#Point estimates for p-curve
	b.pcb=optimize(loss,c(-.3,4),df_obs=dfbad,t_obs=tbad)$minimum    #Choice is bad
	g.pcb=optimize(loss,c(-.3,4),df_obs=dfgood,t_obs=tgood)$minimum  #Choice is good

 
######################################## 
#COMPUTING STANDARD ERROR FOR P-CURVE
######################################## 
#Make them datasets to draw from them easily
  good=data.frame(tgood,dfgood)
  bad=data.frame(tbad,dfbad)

#BOOTSTRAP
  dgood.sim=c()  
  dbad.sim=c()  
  options(warn=-1)  #R gives warning for its own noncentral distribution, not my functions
  set.seed(778899)  #arbitrary number to always get same results

for (i in 1:1000)   #Do 1000 bootstraps: note, when bootstrappingn p-values larger number of bootstraps in often a good idea, 
                    #but for SE() not necessary as we want the stable central value, SD, not the unstable 2.5th percentile.
{
  #Draw a sample of significant studies with replacement (otherwise would get the same studies every time)
  good.k=sub(good)      
  bad.k=sub(bad)
  
  #Estimate effects for bootstrapped sample
  #p-curve
    dgood.k=optimize(loss,c(-.3,2),t_obs=good.k$tgood, df_obs=good.k$dfgood)$minimum
    dbad.k=optimize(loss,c(-.3,2),t_obs=bad.k$tbad, df_obs=bad.k$dfbad)$minimum
  
  #add to saved values
    dgood.sim=c(dgood.sim,dgood.k)
    dbad.sim=c(dbad.sim,dbad.k)
  }

#verify on average I get the right estimate
  mean(dgood.sim)         #Average bootstrapped mean estiamted with p-curve
  mean(dbad.sim)          #Average bootstrapped mean estiamted with p-curve

#compute bootstrapped SE for p-curve
  g.pcse=sd(dgood.sim)
  b.pcse=sd(dbad.sim)

#EFFECT SIZE ESTIMATION PER META-ANALYSIS
#Choice is bad
#effect sizes
      dbad_all=c(0.77,0.88,0.68,1.21,0.81,0.64,0.72,0.47,0.54,0.52,0.39,0.32,0.48,0.54,0.57,0.22,0.08,0.09,0.06,0.02,0.05,0.08,0.04)
        #correct jams study effect size; see long comment in line 42-45
      dbad_all[1]=2*tbad[1]/sqrt(dfbad[1]+2)

#sample sizes
      Nbad_all=c(249,67,60,21,40,60,43,86,65,69,115,165,69,52,41,43,82,60,64,504,61,21,68)
        #correct jams study sample  size; see long comment in line 42-45
        Nbad_all[1]=754


#Choice is bad
#effect sizes
    dgood_all=c(-1.89,-0.94,-0.82,-0.6,-0.52,-0.82,-0.67,-0.26,-0.43,-0.28,-0.31,-0.36,-0.27,-0.17,-0.28,-0.33,-0.2,-0.48,-0.24,-0.16,-0.22,-0.08,-0.17,-0.11,-0.12,-0.09,-0.12,-0.05,-0.05,-0.04,-0.02)
    dgood_all=abs(dgood_all)
#sample sizes
    Ngood_all=c(18,45,54,89,90,32,43,120,46,81,60,45,66,160,52,34,80,16,52,112,57,280,60,80,56,89,40,174,72,57,40)

#Compute var(d)
#Choice is bad
    nb_all=Nbad_all/2
    varb_all=vd(dbad_all,nb_all*2-2) 
#Choice is good
    ng_all=Ngood_all/2
    varg_all=vd(dgood_all,ng_all*2-2) 
  
#keep only the significant ones
    dgood=dgood_all[1:7]
    varg=dgood_all[1:7]
    
    dbad=dbad_all[1:13]
    varb=varb_all[1:13]

#EFFECT SIZE
library(metafor)

#Choice is good
  #naive
    g.nb=rma(yi=dgood,vi=varg,method="FE")$b
    g.nse=rma(yi=dgood,vi=varg,method="FE")$se
  #earnest
    g.eb=rma(yi=dgood_all,vi=varg_all,method="FE")$b
    g.ese=rma(yi=dgood_all,vi=varg_all,method="FE")$se

#Choice is bad
  #naive
    b.nb= rma(yi=dbad,vi=varb,method="FE")$b
    b.nse=rma(yi=dbad,vi=varb,method="FE")$se

  #Earnest
    b.eb=rma(yi=dbad_all,vi=varb_all,method="FE")$b
    b.ese=rma(yi=dbad_all,vi=varb_all,method="FE")$se

  results.b=matrix(c(b.nb, b.eb, b.pcb, g.nb, g.eb, g.pcb),nrow=3,ncol=2)
  results.se=matrix(c(b.nse, b.ese, b.pcse, g.nse, g.ese, g.pcse),nrow=3,ncol=2)





