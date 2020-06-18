
#R Code behind Figure 5C&D in -  MANY LABS REPLICATION PROJECT
#Simonsohn, Simmons and Nelson,  "P-Curve and Effect Size: Correcting for Publication Bias Using Only Significant Results"
#
#Prepared by Uri Simonsohn, uws@wharton.upenn.edu
#
#Last updated: 2014 05 13
##########################################################



#This program uses data collected by the 36 labs participating in the Many Labs replication project
#see Klein et al (2014) https://osf.io/wx7ck/

#1) It loads the data for two of the studies: sunk cost fallacy and asian disease.

#2) It drops the two largest studies, with about 1000 subjects each, so that there will be some effect of selective
#reporting (otherwise those two studies drive the effect size estimate. p-curve still does very well, almost exactly 
#estimating effect size from significant studies alone, but, the naive estimate is not too biased either, so the
#example is not partiularly informative.

#3) Computes a meta-analytical effect size based on all studies (earnest estimate) and based only on the significant ones (naive)
#   Using the RMA procedure it collects the standard error of the naive and earnest estiamte
#4) Computes a p-curve based effect size

#5) Computes the standard error of the p-curve estimate via bootstrapping. IN particular, the same number of significnat
#   studies is drawn with replacement from teh set that exists, a new estimate is performed, and the task is repeated
#   1000 times. The standard deviation of the effect size estimate is used as the standard error for p-curve's estimate

#################################################################

#Load library used for the meta-analysis
  library(metafor)

#Create three functions that will be used in the program below

#Function 1: variance of a cohen d estimate
    vd=function(d,df) 
      {
      n=(df+2)/2
      return(2/n +(d**2)/(2*(n-3.94)))
      }

#Function 2: Loss function for effect size estimation based on Kolmogorov-Smirnov test
    loss=function(t_obs,df_obs,d_est) {
      ncp_est=sqrt((df_obs+2)/4)*d_est                          
      tc=qt(.975,df_obs)                     
      power_est=1-pt(tc,df_obs,ncp_est)        
      p_larger=pt(t_obs,df=df_obs,ncp=ncp_est)
      ppr=(p_larger-(1-power_est))/power_est  
      KSD=ks.test(ppr,punif)$statistic        
      return(KSD)          
    }


#Function 3: Simplified sampling function
  sub = function(df) return(df[sample(nrow(df),nrow(df),replace=TRUE),])  #take a dataframe and generates another of the same size


#######################################################################

# significant subset
      sunk.d.sig=subset(sunk.d.all, sunk.p.all<.05 & sunk.t.all>0)
      sunk.t.sig=subset(sunk.t.all, sunk.p.all<.05 & sunk.t.all>0)
      sunk.df.sig=subset(sunk.df.all, sunk.p.all<.05 & sunk.t.all>0)
    
# variance of the effect sizes using formula (see Function 1 above)
      sunk.vd.sig=vd(d=sunk.d.sig, df=sunk.df.sig)  #vd: variance of d
      sunk.vd.all=vd(d=sunk.d.all, df=sunk.df.all)

# estimate effect size with p-curve
      sunk.dhat.pc=optimize(loss,c(-.3,2), t_obs=sunk.t.sig, df_obs=sunk.df.sig)$minimum
      asian.dhat.pc=optimize(loss,c(-.3,2), t_obs=asian.t.sig, df_obs=asian.df.sig)$minimum
    
# bootstrap sd for p-curve
    sunk = data.frame(sunk.t.sig, sunk.df.sig,  sunk.d.sig, sunk.vd.sig) #puts together all significant result
    sunk.dhat.pc.sim=c() #store each simulated effect size estimate in  vectors. 

for (i in 1:1000){
  sunk.k=sub(sunk)#Draw a sample of significant studies with replacement (otherwise would get the same studies every time)#here i am  using the sub(), Function 3 above
  sunk.dhat.pc.k  = optimize(loss,c(-.2,2),t_obs=sunk.k$sunk.t.sig, df_obs=sunk.k$sunk.df.sig)$minimum
  sunk.dhat.pc.sim=c(sunk.dhat.pc.sim, sunk.dhat.pc.k) #add to saved values
}
  
sunk.se.pc = sd(sunk.dhat.pc.sim)

 
  