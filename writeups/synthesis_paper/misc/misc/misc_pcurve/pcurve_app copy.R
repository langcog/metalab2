

#----------------------> CHANGE THIS <-----------------------------------------
      R_temp="C:\\Uri\\Research\\p-curve app\\App4\\"      #This is the folder where the file with the tests results to be read should be, all output will be saved here
      filek='1234_'                                        #This is a number used to identify all resulting files, the server assigns it based on time, you can keep 1234_ or change it to the name of the file you have keeping in mind that name will be used for all resulting output
#----------------------> CHANGE THIS <-----------------------------------------


#EXPLANATION
      #R will try to read the tests results from the file "1234_raw.txt"; See (1.1) below. The file should have the format allowed by the server.
      #A sample file is available here: http://p-curve.com/app4/1234_raw.txt
      #
      #For example, say you download that file and want to run in. Say you saved it as "c:\stuff\1234_raw.txt" 
      #You would change above:
      #     R_temp="c:\\stuff\\"
      #     filek="1234_"
      #
      #You can then run all the code below, and the folder c:\stuff will contain all the files generated with results, and they will be called 1234_stouffer.txt 1234_cumulative.png etc

      #There are undoubtedly better ways to implement reading a file when running the code on your own 
      #but this structure makes a lot of sense for the server implementation and the idea is to have this file perfectly match
      #what's run on the server. Sorry.

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#p-curve app 4.03
#Last update: 2016 04 08
#If you see any errors, please email Uri Simonsohn, uws@wharton.upenn.edu
#

##########################################################################################################
#Introduction: The app works by executing R on the server, saving output as image and text files that are then read by .PHP to present on a browser.
#This program is written so that people can run exactly what's on the server on their own machines. 
##########################################################################################################

############################################
    #   OUTLINE
    #(0) CREATE A FEW FUNCTIONS 
    #(1) PROCESS USER INPUT AND CONVERT IT INTO USABLE R DATA
    #(2) COMPUTE PP-VALUES
    #(3) INFERENCE (STOUFFER TEST + BINOMIAL)
    #(4) POWER ESTIMATE (confidence interval is 4.3)
    #(5) MAIN FIGURE: OBSERVED P-CURVE AND EXPECTED UNDERL NULL AND 33% POWER
    #(6)  SAVE PP-VALUE CALCULATIONS TO TEXT FILES
    #(7) CUMULATIVE P-CURVES
############################################


  library(stringr)  #Library to process string variables (text of the entered tests)
  library(poibin)   #This library has the poisson binomial, the distribution of the sum of binomial with different underlying probabilities
                    #used to compute the binomial test given that each test has a (slightly) different probability of p<.025 depending on its own noncentral parameter
                    #See Hong (2013) - "On computing the distribution function for the Poisson binomial distribution" Computational Statistics and Data Analysis, V59, p.41-51 - http://dx.doi.org/10.1016/j.csda.2012.10.006 

  setwd(R_temp)        #Set as Working Directory the folder where temporary files are created
  options(scipen=999)  #Turn off scientific notation

##############################################
#(0) CREATE A FEW FUNCTIONS
##############################################
#Function 1 - functions that find non-centrality parameter for f,chi distributions that gives some level of power

#F-test 
#Note: starting with app 4.0, t() are converted to F() and Z to chisq() to avoid unnecessary repetition of code
#So we only need code to find ncp for F() and chisq()

  getncp.f =function(df1,df2, power)   {      
  error = function(ncp_est, power, x, df1,df2) pf(x, df1 = df1, df2=df2, ncp = ncp_est) - (1-power)   
  xc=qf(p=.95, df1=df1,df2=df2) 
  return(uniroot(error, c(0, 1000), x = xc, df1 = df1,df2=df2, power=power)$root)  }


#chisq-test
  getncp.c =function(df, power)   {      
  xc=qchisq(p=.95, df=df) 
  error = function(ncp_est, power, x, df)      pchisq(x, df = df, ncp = ncp_est) - (1-power)   
  return(uniroot(error, c(0, 1000), x = xc, df = df, power=power)$root)   }

#Combine both in single function
  getncp=function(family,df1,df2,power) {
    if (family=="f") ncp=getncp.f(df1=df1,df2=df2,power=power)
    if (family=="c") ncp=getncp.c(df=df1,power=power)
  return(ncp)  }
  
###############################################################################
#Function 2 - percent() : makes a number look like a percentage
percent <- function(x, digits = 0, format = "f", ...)   {
  paste(formatC(100 * x, format = format, digits = digits, ...), "%", sep = "")
}
###############################################################################


###############################################################################
#Function 3 - pbound: bound p-values and pp-values by precision of measurement to avoid errors
pbound=function(p) pmin(pmax(p,2.2e-16),1-2.2e-16)


#Function 4 - prop33(pc) - Computes % of p-values that are expected to be smaller than pc, 
                        #for the tests submitted to p-curve, if power is 33%
prop33=function(pc)
{
  #pc: critical  p-value
  
  #Overview:
  #Creates a vector of the same length as the number of tests submitted to p-curve, significant and not,
  #    and computes the proportion of p-values expected to be smaller than {pc} given the d.f. 
  #    and outputs the entire vector, with NA values where needed
  
  #F-tests (& thus  t-tests)
    prop=ifelse(family=="f" & p<.05,1-pf(qf(1-pc,df1=df1, df2=df2),df1=df1, df2=df2, ncp=ncp33),NA)
  #Chi2 (& thus Normal)
    prop=ifelse(family=="c" & p<.05,1-pchisq(qchisq(1-pc,df=df1),  df=df1, ncp=ncp33),prop)
  #output it
    prop
}

#Function 5 Stouffer test for a vector of pp-values
  stouffer=function(pp) sum(qnorm(pp),na.rm=TRUE)/sqrt(sum(!is.na(pp)))
    
  
  
###############################################################################
#(1) PROCESS USER INPUT AND CONVERT IT INTO USABLE R DATA
###############################################################################  
  
  
#(1.1) Load data
#From file;
  raw = scan(file=paste0(filek,"raw.txt"),what="")      #read file on server
  raw=tolower(raw)                                      #lower case
  ktot=length(raw)                                      #count studies

#Create vector that numbers studies 1 to N,includes n.s. studies
  k=seq(from=1,to=length(raw))

#1.2 Parse the entered text into usable statistical results
#1.3 Create test type indicator
  stat=substring(raw,1,1)          #stat:   t,f,z,c,r
  test=ifelse(stat=="r","t",stat)  #test:   t,f,z,c      (r-->t)  

#1.4 Create family to turn t-->F and z-->chi2
  family=test
  family=ifelse(test=="t","f",family)
  family=ifelse(test=="z","c",family)

#Note on terminology:
  #Stat:   t,f,c,z,r  is what the user entered, t,f,c,z,r
  #test:   t,f,c,z    is the test statistic, same as stat but with r-->t
  #family: f,c        converting t-->f and z-->c

#1.5 Find comma,parentheses,equal sign 
  par1 =str_locate(raw,"\\(")[,1]         #(  First  parenthesis
  par2 =str_locate(raw,"\\)")[,1]         #)  Second parenthesis
  comma=str_locate(raw,",")[,1]           #,  comma
  eq   =str_locate(raw,"=")[,1]           #=  equal

#1.6 DF for t-tests
  df=as.numeric(ifelse(test=="t",substring(raw,par1+1,par2 -1),NA))             #t(df) later assigned to df2 in  F test with df1=1

#1.7 DF1 for all tests 
#   recall, normal=sqrt(chi(1)) so df1=1 for Normal, same f(1,df)<-t(df)
  df1=as.numeric(ifelse(test=="f",substring(raw,par1+1,comma-1),NA))            #If F test, take value after comma, NA otherwise
  df1=as.numeric(ifelse(test=="z",1,df1))                                       #If Z replace missing value with a 1
  df1=as.numeric(ifelse(test=="t",1,df1))                                       #If t, replace missing value with a 1
  df1=as.numeric(ifelse(test=="c",substring(raw,par1+1,par2 -1),df1))           #If c, replace missing value with value in ()

#1.8 DF2 for F(df1,df2) tests
  df2=as.numeric(ifelse(test=="f",substring(raw,comma+1,par2-1),NA))            #F-test
  df2=as.numeric(ifelse(test=="t",df,df2))                                      #t(df) into the df2 F test

#1.9 Take value after equal sign, the value of the test-statistic, and put it in vector "equal"
  equal=abs(as.numeric(substring(raw,eq+1,)))  #if not a r(), take the value after the ="

#1.10  Go from "equal" (the value after = sign) to F or Chi2 value,
  value=ifelse((stat=="f" | stat=="c"),equal,NA)                      #For F and Chi2 test, equal=value
  value=ifelse(stat=="r", (equal/(sqrt((1-equal**2)/df2)))**2,value)  #For correlation, first turn value (r) to t, then square t. (using t=r/sqrt(1-r**2)/DF)
  value=ifelse(stat=="t", equal**2 ,value)                            #For t and Z, square it since t(df)**2=f(1,df) and z**2=chi(1)
  value=ifelse(stat=="z", equal**2 ,value)  
    
  
    
#1.11 Compute p-values
  p=ifelse(family=="f",1-pf(value,df1=df1,df2=df2),NA)
  p=ifelse(family=="c",1-pchisq(value,df=df1),p)
  p=pbound(p)  #Bound it to level of precision, see function 3 above 

#1.12 Count  studies
  #ktot is all studies
  ksig= sum(p<.05,na.rm=TRUE)     #significant studies
  khalf=sum(p<.025,na.rm=TRUE)    #half p-curve studies

###############################################################################  
#(2) COMPUTE PP-VALUES
##############################################################################  
  
#2.1 Right Skew, Full p-curve
  ppr=as.numeric(ifelse(p<.05,20*p,NA))            #If p<.05, ppr is 1/alpha*p-value, so 20*pvalue, otherwise missing. 
  ppr=pbound(ppr)                                  #apply pbound function to avoid 0


#2.2 Right Skew, half p-curve
  ppr.half=as.numeric(ifelse(p<.025,40*p,NA))    #If p<.05, ppr is 40*pvalue, otherwise missing. 
  ppr.half=pbound(ppr.half)

#2.3 Power of 33%
#2.3.1 NCP for  f,c distributions
# NCP33 (noncentrality parameter giving each test in p-curve 33% power given the d.f. of the test)
    ncp33=mapply(getncp,df1=df1,df2=df2,power=1/3,family=family)  #See function 1 above
    
#2.3.2 Full-p-curve
#Using the ncp33 compute pp33
  pp33=ifelse(family=="f" & p<.05,3*(pf(value, df1=df1, df2=df2, ncp=ncp33)-2/3),NA)
  pp33=ifelse(family=="c" & p<.05,3*(pchisq(value, df=df1, ncp=ncp33)-2/3),pp33)
  pp33=pbound(pp33)

#2.3.3 HALF-p-curve
  #Share of p-values expected to be p<.025 if 33% power (using Function 4 from above, prop33() )
      prop25=3*prop33(.025)
      prop25.sig=prop25[p<.05]
    

 #Compute pp-values for the half
    pp33.half=ifelse(family=="f" & p<.025, (1/prop25)*(    pf(value,df1=df1,df2=df2,ncp=ncp33)-(1-prop25)),NA)
    pp33.half=ifelse(family=="c" & p<.025, (1/prop25)*(pchisq(value,df=df1,         ncp=ncp33)-(1-prop25)),pp33.half)
    pp33.half=pbound(pp33.half)

  
###############################################################################  
#(3) INFERENCE - STOUFFER & BINOMIAL
##############################################################################  

#3.1 Convert pp-values to Z scores, using Stouffer function above
  Zppr =     stouffer(ppr)            #right skew  - this is a Z value from Stouffer's test
  Zpp33=     stouffer(pp33)           #33% - idem 
  Zppr.half= stouffer(ppr.half)       #right skew, half p-curve - idem 
  Zpp33.half=stouffer(pp33.half)      #33% skew, half p-curve - idem 

#3.2 Overall p-values from Stouffer test
  p.Zppr =pnorm(Zppr)	
  p.Zpp33=pnorm(Zpp33)
  p.Zppr.half =pnorm(Zppr.half)
  p.Zpp33.half=pnorm(Zpp33.half)
  
#3.3 Save results to file
  main.results=as.numeric(c(ktot, ksig, khalf, Zppr, p.Zppr, Zpp33, p.Zpp33, Zppr.half, p.Zppr.half, Zpp33.half, p.Zpp33.half))
  write(main.results, paste("STOUFFER_",filek,".txt", sep=""),sep="\n")

#3.4 BINOMIAL
  #Observed share of p<.025
    prop25.obs=sum(p<.025)/sum(p<.05)
#3.4.1 Flat null
  binom.r=1-pbinom(q=prop25.obs*ksig- 1, p=.5, size=ksig)     #The binomial in R computes the probability of x<=xo. We want prob(x>=x0) so we subtract one from x, and 1-prob()
#3.4.2 Power of 33% null
  binom.33=ppoibin(kk=prop25.obs*ksig,pp=prop25[p<.05])             

  #syntax for ppoibin():
  #   kk: is the proportion of succeses, a scalar, in this case, the share of p<.025
  #   pp: is the probabilty of success for each attempt, the number of attempts is determined
  #    by the length of the vector. For example ppoibin(kk=0,pp=c(.5,.5,.5)) is .125,
  #    if there are three attempts, each with probability .5, the odds of getting 0 succeses is .125
  #     ppoibin(kk=1,pp=c(1,.75)), in turn computes the probability of getting 1 success
  #     when one has a 100% of success, and the other 75%, and the solution is .25, since
  #     the first one succeeds for sure and the second would need to fail, with 25% chance.


#3.4.3  Save binomial results
  binomial=c(mean(prop25.sig), prop25.obs, binom.r, binom.33)
  write(binomial, paste("BINOMIAL_",filek,".txt", sep=""),sep="\n")

    
#3.5 Beutify results for printing in figure
  #3.5.1 Function that processes p-values and bounds when >.999 or <.0001
    cleanp=function(p)
    {
      p.clean=round(p,4)           #Round it
      p.clean=substr(p.clean,2,6)  #Drop the 0
      p.clean=paste0("=",p.clean)
      if (p < .0001) p.clean= "<.0001"
      if (p > .9999) p.clean= ">.9999"
      return(p.clean)
    }  
    
  #If there are zero p<.025, change Stouffer values for half-p-curve tests for "N/A" messages	
		if (khalf==0) {
			 Zppr.half ="N/A"
			 p.Zppr.half ="=N/A"
			 Zpp33.half ="N/A"
			 p.Zpp33.half ="=N/A"
		}

       
  #If there are more than 1 p<.025, round the Z and beutify the p-values
    if (khalf>0) {
			 Zppr.half =round(Zppr.half,2)
			 Zpp33.half =round(Zpp33.half,2)
			 p.Zppr.half=cleanp(p.Zppr.half)
			 p.Zpp33.half=cleanp(p.Zpp33.half)
			 }
    
  #Clean  results for full test
      Zppr=round(Zppr,2)
      Zpp33=round(Zpp33,2)
      p.Zppr=cleanp(p.Zppr)
      p.Zpp33=cleanp(p.Zpp33)
      binom.r=cleanp(binom.r)
      binom.33=cleanp(binom.33)
              
################################################
#(4) POWER ESTIMATE
################################################

#4.1 Function powerfit(power_est) - Returns the Stouffer Z of observing at least as right skewed a p-curve if  power=power_est
                                    #if Z=0, power_est is the best fit (p=50%). 
                                    #if Z<0 the best fit is <power_est, 
                                    #if Z>0 the best fit is >power_est
  powerfit=function(power_est) 
  {
  #4.1.1 Get the implied non-centrality parameters (ncps) that give power_est to each test submitted to p-curve
    ncp_est=mapply(getncp,df1=df1,df2=df2,power=power_est,family=family)
  #4.1.2 Compute implied pp-values from those ncps_est,  
    pp_est=ifelse(family=="f" & p<.05,(pf(value,df1=df1,df2=df2,ncp=ncp_est)-(1-power_est))/power_est,NA)
    pp_est=ifelse(family=="c" & p<.05,(pchisq(value,df=df1,ncp=ncp_est)-(1-power_est))/power_est,pp_est)
    pp_est=pbound(pp_est)
  #4.1.3 Aggregate pp-values for null that power=power_est via Stouffer
    return(stouffer(pp_est))   #This is a z score, so powerfit is expressed as the resulting Z score.
  }
  
  
#4.2 COMPUTE FIT FOR EACH POWER for 5.1%, AND THEN 6-99%, AND PLOT IT. With power=5% boundary condition lead to errors
#This becomes the diagnostic plot and gives us the best estimate, within 1%, of power.

#Create image file to contain results
  png(filename=paste(filek,"_fit.png",sep=""), width=1200, height=1000, res=200)  
# Fit will be evaluated at every possible value of power between 5.1% and 99% in steps of 1%, stored in fit()
  fit=c()                                          #Create empty vector
  fit=abs(powerfit(.051))                      #Start it eavaluting fit of 5.1% power
  for (i in 6:99)   fit=c(fit,abs(powerfit(i/100))) #Now do 6% to 99%
  
# Find the minimum
  #which ith power level considered leads to best estimate
    mini=match(min(fit,na.rm=TRUE),fit)       
  #convert that into the power level, the ith value considered is (5+ith)/100
    hat=(mini+4)/100                          
#Plot results
#create the x-axis
  x.power=seq(from=5,to=99)/100 
#Draw the line
  par(mar=c(5.1,8.1,4.1,2.1))  #Margins 
  plot(x.power,fit,xlab="Underlying Power", ylab="",ylim=c(-.15,max(fit)), main="")  
#Figure title
  mtext("Estimating underlying statistical power",side=3,line=1.75,cex=1.5,at=0.4)
  mtext("(Plot should be V shaped, or a smooth line to 99%; else don't trust estimate)",col='red',side=3,line=.5,cex=1,at=0.4)
#Make red dot at the estimate
  points(hat,min(fit,na.rm=TRUE),pch=19,col="red",cex=2)    
#Put a label with the estimate value
  sign="="
  if (hat<.06) sign="<"
  text(min(.5,max(.28,hat)),min(fit,na.rm=TRUE)-.15,paste0("Estimated Power ",sign," ",hat*100,"%"))
#Label the y-axis
  mtext(c("Good","Bad"),side=2,line=3,at=c(0,max(fit)),las=1,cex=1.25,col=c("blue","red"))
  mtext("Fit for observed p-curve",side=2,line=6.5,cex=1.5)
  mtext("(Stouffer test for null of power in x-axis)\n|Z-score|",side=2,line=4.5,col="gray")
  dev.off()
 
 
#4.3 Confidence interval for power estimate
#4.3.1 Function get.power_pct(pct) 
  get.power_pct =function(pct)   {   
      #Function that finds power that gives p-value=pct for the Stouffer test 
      #for example, get.power_pct(.5) returns the level of power that leads to p=.5  for the stouffer test.
      #half the time we would see p-curves more right skewed than the one we see, and half the time
      #less right-skewed, if the true power were that get.power_pct(.5). So it is the median estimate of power
      #similarliy, get.power_pct(.1) gives the 10th percentile estimate of power...
  #Obtain the normalized equivalent of pct, e.g., for 5% it is -1.64, for 95% it is 1.64
    z=qnorm(pct)  #convert to z because powerfit() outputs a z-score. 
  #Quantify gap between computed p-value and desired pct
    error = function(power_est, z)  powerfit(power_est) - z
  #Find the value of power that makes that gap zero, (root)
    return(uniroot(error, c(.0501, .99),z)$root)   }
  
#4.3.2 Boundary conditions (when the end of the ci=5% or 99% we cannot use root to find it, 
      #use boundary value instead)

  #Boundary conditions
    p.power.05=pnorm(powerfit(.051)) #Proability p-curve would be at least at right-skewed if power=.051
    p.power.99=pnorm(powerfit(.99))  #Proability p-curve would be at least at right-skewed if power=.99

#4.3.3 Find lower end of ci
#Low boundary condition? If cannot reject 5% power, don't look for lower levels, use 5% as the end 
  if (p.power.05<=.95) power.ci.lb=.05   
#High boundary condition? If we reject 99%, from below dont look for higher power, use 99% as the low end
  if (p.power.99>=.95) power.ci.lb=.99   
#If low bound is higher than 5.1% power and lower than 99% power, estimate it, find interior solution
  if (p.power.05>.95 && p.power.99<.95)  power.ci.lb=get.power_pct(.95)


#4.3.4 Higher end of CI
#If we reject 5% power from below, 5% is above the confidence interval, use 5% as the upper end of the confidence interval
  if (p.power.05<=.05) power.ci.ub=.05
#If we do not reject that 99% power, don't look higher, use 99% as the higher end 
  if (p.power.99>=.05) power.ci.ub=.99
#If the the upper bound is between 5% and 99%, find it
  if (p.power.05>.05 && p.power.99<.05) power.ci.ub=get.power_pct(.05)

    
#4.4 Save power fit estiate and ci 
  power_results=c(power.ci.lb,hat,power.ci.ub)
  write(power_results, paste("POWERHAT_",filek,".txt", sep=""),sep="\n")     
  
  #Note, I use hat as the estimate of power, with powerfit(.5) we could get a more precise best fitting 
  #level of power than the minimum in the figure above between .051 and .99, hat, but more precision than 1% in power is not informative.
   

###############################################################################  
#(5) MAIN FIGURE: OBSERVED P-CURVE AND EXPECTED UNDERL NULL AND 33% POWER
##############################################################################  

#5.1 Green line (Expected p-curve for 33% power)
#5.1.1 Proportion of tests expected to get <01, <02...
#Uses FUNCTION 4, prop33() - see top of page
  gcdf1=prop33(.01)         #vector with proportion of p-values p<.01, with 33% power
  gcdf2=prop33(.02)         #              ""                   p<.02,      "
  gcdf3=prop33(.03)         #              ""                   p<.03,      "
  gcdf4=prop33(.04)         #              ""                   p<.04,      "
#Note: for p<.05 we know it is 33% power

#5.1.2 Now compute difference, and divide by 1/3 to get the share of significant p-values in each bin      
  green1=mean(gcdf1)*3        #Average of the vector p<.01
  green2=mean(gcdf2-gcdf1)*3  #Difference between .02 and .01
  green3=mean(gcdf3-gcdf2)*3  #Difference between .03 and .02
  green4=mean(gcdf4-gcdf3)*3  #Difference between .04 and .03
  green5=mean(1/3-gcdf4)*3    #Difference between .05 and .04
  #Because we have one row per test submitted, the average is weighted average, giving each test equal weight
  green=100*c(green1,green2,green3,green4,green5)  #The 5 values plotted in the figure for 33% power line


#5.2 The blue line (observed p-curve)   
  #Put all significant p-values  into bins, .01 ....05
    ps=ceiling(p[p<.05]*100)/100
  #Count # of tests in each bin
    blue=c()
  #This loop creates a vector, blue, with 5 elements, with the proportions of p=.01,p=.02...p=.05
    for (i in c(.01,.02,.03,.04,.05)) blue=c(blue,sum(ps==i,na.rm=TRUE)/ksig*100)
  
  #5.3 Red line
    red=c(20,20,20,20,20)
  
  #5.4 Make the graph
  #Note: R_temp & filek are parameterS set at the beggining of the program for the location of files
    png(filename=paste(filek,".png",sep=""), width=2600, height=2400, res=400)  
    
  #5.4.1  Define x-axis as p-values (.01, .02..)
      x = c(.01,.02,.03,.04,.05)

  #5.4.2 Plot the observed p-curve
    par(mar=c(15,5.5,1.5,6))   #margins; very big bottom margin to fit table with results below
  
  #5.4.3 Plot starts
      plot(x,blue,   type='l', col='dodgerblue2',  main="",lwd=2, xlab="", ylab="", xaxt="n",yaxt="n", xlim=c(0.01,0.055), ylim=c(0,105), bty='L', las=1);  	

  #5.4.4 x-axis value labels
    x_=c(".01",".02",".03",".04",".05")
    axis(1,at=x,labels=x_)
  #5.4.5 y-axis value labels
    y_=c("0%","25%","50%","75%","100%")
    y=c(0,25,50,75,100)
    axis(2,at=y,labels=y_,las=1,cex.axis=.75)
  
  #5.4.6 Add y-axis label
    mtext("Percentage of test results",font=2,side=2,line=2.75,cex=1.25)
  #5.4.7 Add y-axis label
    mtext("p            ",font=4,side=1,line=2,cex=1.25)
    mtext(" -value",font=2,side=1,line=2,cex=1.25)
  
  #5.4.8 Add little point in actual frequencies
    points(x,blue,type="p",pch=20,bg="dodgerblue2",col="dodgerblue2")
  #5.4.9 Add value-labels
    text(x+.00075,blue+5,percent(round(blue)/100),col='black', cex=.75)
  #5.4.10Add red and green lines
    lines(x,red,   type='l', col='firebrick2',    lwd=1.5, lty=3)
  lines(x,green, type='l', col='springgreen4',  lwd=1.5, lty=5)
  
  #5.4.11 Legend
  #By default in the middle
    legendx=.035 ; 
  #Move left for p-curves that have more 80% of p-values =.04 or =.05 so that the legend does not touch blue line
    if (blue[04]>80 | blue[05]>80) legendx=.015
  
  #Print legend
    legend(legendx, 100, c('Observed p-curve','Null of 33% power', 'Null of zero effect'), 
         box.col="white",lty=c(1,5,3), cex=.75,lwd=c(1,1),col=c('dodgerblue2','springgreen4', 'firebrick2'));
  
  #5.5 BEGINS TABLE EMBEDDED IN FIGURE
  #5.5.1 
    a=-30             #Parameter for positioning all text, using when setting up first time to make it easy to adjust, every y-coordinate is based off "a"
    par(xpd=TRUE)     #Allows printing off margins
    col1="#40C4FF"    #Color for first and third header
    col2="#80D8FF"    #Color for 2nd header
    
  #5.5.2 Header 1 (binomial   |    stouffer)
      rect(0,a,.065,a-15,col=col1,border=NA)                  #Create the rectangle
      text(.0275,a-4,"Binomial Test",cex=.75, font=2)
        text(.0275,a-10,"Share of p<.025",cex=.65, font=3)
      text(.05,a-4,"Continuous Test",cex=.75, font=2)
        text(.05,a-10,"Aggregate pp-values (Stouffer Method)",cex=.65, font=3)
    
  #5.5.3 Header 2 (half     |      full)
        rect(.035,a-15,.065,a-23,col=col2,border=NA)
        text(.0425,a-19,"Full p-curve (p's<.05)",cex=.65, font=4)
        text(.0575,a-19,"Half p-curve (p's<.025)",cex=.65, font=4)
  
  #5.5.3 Right skew tests
    #Label
        text(0,a-30,"1) Studies contain evidential value",cex=.65,adj=0)
        text(0,a-35,"          (Right-skew)",cex=.575,adj=0,font=3)
    #5.5.4 REsults right-skew
        text(.0275,a-30,paste0("p",binom.r),cex=.65,font=3)
  
        res.rf=paste0("Z=",Zppr,", p",p.Zppr)
        text(.0425,a-30,res.rf,cex=.65,font=3)
    #Z half
        res.rh=paste0("Z=",Zppr.half,", p",p.Zppr.half)
        text(.0575,a-30,res.rh,cex=.65,font=3)
    
  #33% Power tests
    #Label
        text(0,a-42,"2) Evidential value, if any, is inadequate",cex=.65,adj=0)
        text(0,a-47,"          (Flatter than when power=33%)",cex=.575,adj=0,font=3)
    #Binomial
        text(.0275,a-42,paste0("p",binom.33),cex=.65,font=3)
    #Z full
        res.33f=paste0("Z=",Zpp33,", p",p.Zpp33)
        text(.0425,a-42,res.33f,cex=.65,font=3)
    #Z half
        res.33h=paste0("Z=",Zpp33.half,", p",p.Zpp33.half)
        text(.0575,a-42,res.33h,cex=.65,font=3)
    #IF no p<.025, print red text
        if (khalf==0) text(.0575,a-47,"(No p<.025 entered)",col="red",cex=.65,font=3)
        
  #Power Estimate
    #Header 3
        rect(0,a-50,.065,a-60,col=col1,border=NA)
        text(.045,a-55,"Statistical Power",cex=.75,font=2)
    #Label
        text(0,a-65,"Power of tests included in p-curve",cex=.65,adj=0)
        text(0,a-70,"          (Correcting for publication bias)",cex=.575,adj=0,font=3)
    #Estimate
        hat_clean=paste0("Estimate: ",percent(hat))
        text(.045,a-65,hat_clean,cex=.65)
    #CI
        ci=paste0("Confidence interval: (",percent(power.ci.lb),","
                                          ,percent(power.ci.ub),")")
        text(.045,a-70,ci,cex=.65)
  #Separating line  
        abline(h=a-75,lty=3,col=col1)
  #Number of tests in p-curve      
        text(0,a-80,adj=0,paste0("The observed p-curve includes ",ksig,
                                 " statistically significant (p<.05) results, of which ",khalf,
                                 " are p<.025."),cex=.65)
  #Number of n.s. results entered
        kns=ktot-ksig
        if (kns==0) ns_msg="There were no non-significant results entered." 	
	      if (kns==1) ns_msg="There was one additional result entered but excluded from p-curve because it was p >.05." 	
	      if (kns>1)  ns_msg=paste0("There were ",kns," additional results entered but excluded from p-curve because they were p>.05.")
        text(0,a-85,adj=0,ns_msg,cex=.65) 
        
        dev.off()  
  
  
  
###############################################################################  
#(6)  SAVE PP-VALUE CALCULATIONS TO TEXT FILES
##############################################################################  

#6.1 Table contains the original text enter, the corresponding p-value, the pp-values, and Z-scores for those pp-values
  table_calc=data.frame(raw, p, ppr, ppr.half, pp33, pp33.half, 
                      qnorm(ppr),  qnorm(ppr.half), qnorm(pp33), qnorm(pp33.half))
#6.2 Headers
  headers1=c("Entered statistic","p-value", "ppr", "ppr half", "pp33%","pp33 half",
           "Z-R","Z-R half","Z-33","z-33 half")
#6.3 Put headers onto table
  table_calc=setNames(table_calc,headers1)

#6.4 Save it
  write.table(table_calc,sep="	", row.names=FALSE, file=paste("Calculations_",filek,".txt", sep=""))

#6.5 Save results behind p-curve figure
  headers2=c("p-value","Observed (blue)","Power 33% (Green)", "Flat (Red)")
  table_figure=setNames(data.frame(x,blue,green,red),headers2)
#Save it to file
  write.table(table_figure, sep="	",row.names=FALSE, file=paste("FigNumbers_",filek,".txt", sep=""))
  

################################################
# 7 CUMULATIVE P-CURVES
################################################
  

#7.1 FUNCTION THAT RECOMPUTES OVERALL STOUFFER TEST WITHOUT (K) MOST EXTREME VALUES, ADJUSTING THE UNIFORM TO ONLY INCLUDE RANGE THAT REMAINS
  dropk=function(pp,k,droplow) 
  {
  #Syntax:
  #pp: set of pp-values to analyze sensitivity to most extremes
  #k:  # of most extreme values to exclude
  #dropsmall: 1 to drop smallest, 0 to drop largest
  
  pp=pp[!is.na(pp)]                             #Drop missing values 
  n=length(pp)                                  #See how many studies are left
  pp=sort(pp)                                   #Sort the pp-value from small to large
  if (k==0) ppk=pp                              #If k=0 do nothing for nothing is being dropped
  #If we are dropping low values
  if (droplow==1 & k>0) 
  {
    #Eliminate lowest k from the vector of pp-values
    ppk=(pp[(1+k):n])
    ppmin=min(pp[k],k/(n+1))   #Boundary used to define possible range of values after exclusion
    ppk=(ppk-ppmin)/(1-ppmin)  #Take the k+1 smallest pp-value up to the highest, subtract from each the boundary value, divide by the range, ~U(0,1) under the null
    #This is explained in Supplement 1 of Simonsohn, Simmons Nelson, JEPG 2016 "Better p-curves" paper. See https://osf.io/mbw5g/
      
    
  }
  
  #If we are dropping high values
  if (droplow==0 & k>0) 
  {
    #Eliminate lowest k from the vector of pp-values
    ppk=pp[1:(n-k)]
    ppmax=max(pp[n-k+1],(n-k)/(n+1))  #Find new boundary of range
    ppk=ppk/ppmax                      #Redefine range to make U(0,1)
  }
  #In case of a tie with two identical values we would have the ppk be 0 or 1, let's replace that with almost 0 and almost 1
  ppk=pmax(ppk,.00001)                       #Adds small constant to the smallest redefined p-value, avoids problem if dropped p-value is "equal" to next highest, then that pp-value becomes 0
  ppk=pmin(ppk,.99999)                       #Subtract small constant to the largest  redefined pp-value, same reason
  Z=sum(qnorm(ppk))/sqrt(n-k)                        
  return(pnorm(Z))
  } #End function dropk


#7.2 Apply function, in loop with increasing number of exclusions, to full p-curve
#Empty vectors for results
  droplow.r=droplow.33=drophigh.r=drophigh.33=c()

#Loop over full p-curves
  for (i in 0:(round(ksig/2)-1))
  {
  #Drop the lowest k studies in terms of respective overall test
  #Right skew
    droplow.r= c(droplow.r,   dropk(pp=ppr,k=i,droplow=1))
    drophigh.r=c(drophigh.r,  dropk(pp=ppr,k=i,droplow=0))
  #Power of 33%
    droplow.33=c(droplow.33,  dropk(pp=pp33,k=i,droplow=1))
    drophigh.33=c(drophigh.33, dropk(pp=pp33,k=i,droplow=0))
  } 

#Half p-curves

  if (khalf>0)
  {
    droplow.halfr=drophigh.halfr=c()
    for (i in 0:(round(khalf/2)-1))
    {
    #Drop the lowest k studies in terms of respective overall test
      droplow.halfr= c(droplow.halfr,   dropk(pp=ppr.half,k=i,droplow=1))
      drophigh.halfr=c(drophigh.halfr,  dropk(pp=ppr.half,k=i,droplow=0))
    } #End loop
  }#End if that runs calculations only if khalf>0

#7.3 FUNCTION THAT DOES THE PLOT OF RESULTS
  plotdrop=function(var,col)
  {
  k=length(var)
  #Plot the dots
  plot(0:(k-1),var,xlab="",ylab="",type="b",yaxt="n",xaxt="n",main="",cex.main=1.15,ylim=c(0,1),col=col)
  #Add marker in results with 0 drops
  points(0,var[1],pch=19,cex=1.6)
  #Red line at p=.05
  abline(h=.05,col="red")  
  #Y-axis value labels
  axis(2,c(.01,.05,.1,seq(from=.2,to=.9,by=.10)),las=1,cex.axis=.95)
  axis(1,c(0:(k-1)),las=1,cex.axis=.95)
  }


#7.4 RUN PLOT FUNCTION 6 TIMES
#Save what follows
  png(filename=paste(filek,"_cumulative.png",sep=""), width=4000, height=4000, res=400)

#Put all graphs together
  par(mfrow=c(3,2),mar=c(4,4,1,2),mgp=c(2.5,1,0),oma=c(5,14,5,1)) 
#Plot(1) - Right skew, drop low
  plotdrop(droplow.r,col="dodgerblue2")
  mtext(side=2,line=4,"P-value Overall Test",font=2,cex=.85)
  mtext(side=2,line=3,"(Stouffer's Method)",font=3,cex=.75)
#Rigt Skew label
  mtext("Right Skew",line=7,side = 2,cex=1.2,las=1,col="dodgerblue2")
  mtext("\n\n\n(full p-curve)",line=8,side = 2,cex=.8,las=1,col="dodgerblue2") 
#Low to high label
  mtext(bquote("Drop"~italic(k)~bold("lowest")~"original p-values"),line=1,side = 3,cex=1.5,las=1) 
#Plot(2) - Right skew, drop high
  plotdrop(drophigh.r,col="dodgerblue2")
  mtext(bquote("Drop"~italic(k)~bold("highest")~"original p-values"),line=1,side = 3,cex=1.5,las=1) 
#Plot (3) - Half Right skew, drop low
  #If there are P<.025 results do it.
  if (khalf>0)
    {
    plotdrop(droplow.halfr,col="blue3")
    mtext(side=2,line=4,"P-value Overall Test",font=2,cex=.85)
    mtext(side=2,line=3,"(Stouffer's Method)",font=3,cex=.75)
    }

  #otherwise, message saying no chart
    if (khalf==0)
    {
    plot(1, type="n", axes=F, xlab="", ylab="")
    mtext(side=1,line=-10,"No p<.025 results, so no half p-curve",font=2,cex=1.25)
    }

#Half p-curve label
  mtext("Right Skew",line=7,side = 2,cex=1.2,las=1,col="blue3")
  mtext("\n\n\n(half p-curve)",line=8,side = 2,cex=.8,las=1,col="blue3") 
  
#Plot (4) - Half right skew, drop high
  plot(1, type="n", axes=F, xlab="", ylab="")
  mtext(side=1,line=-10,"Graph not needed",font=2,cex=1.25)
  mtext(side=1,line=-8.5,"(Half p-curve excludes high p-values)",font=3,cex=.75)
  

#Plot (5) Power 33%, drop low
  plotdrop(drophigh.33,col="springgreen4")

#33%  Skew label
  mtext("33% Power",line=7,side = 2,cex=1.2,las=1,col="springgreen4")
  mtext("\n\n\n(full p-curve)",line=8,side = 2,cex=.8,las=1,col="springgreen4") 
#Labels bottom left of chart
  mtext(side=2,line=4,"P-value Overall Test",font=2,cex=.85)
  mtext(side=2,line=3,"(Stouffer's Method)",font=3,cex=.75)
  mtext(side=1,line=2.5,"K Tests Dropped From p-curve",cex=1.15,font=2)
#Plot (6)
  plotdrop(droplow.33,col="springgreen4")
#Labels bottom right of chart
  mtext(side=2,line=4,"P-value Overall Test",font=2,cex=.85)
  mtext(side=2,line=3,"(Stouffer's Method)",font=3,cex=.75)
  mtext(side=1,line=2.5,"K Tests Dropped From p-curve",cex=1.15,font=2)
#x-axis label
#Legend (winging it for location)
  op=par(usr=c(0,1,0,1),xpd=NA)   #allow goin outsie of plot 6 and recalibrate dimensions to be 0-1 in x and y
  legend(-.6,-.25,horiz=TRUE,pch=c(19,1),cex=1.4, legend=c("Including all p-values","Dropping p-values"))   
#so the legend is placed 60% of a chart to the left of 0 of #6, an 25% of a chart below it.
#save it
  dev.off()

