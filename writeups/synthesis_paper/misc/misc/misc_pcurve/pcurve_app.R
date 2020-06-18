

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

options(scipen = 999)  #Turn off scientific notation

##############################################
#(0) CREATE A FEW FUNCTIONS
##############################################
#Function 1 - functions that find non-centrality parameter for f,chi distributions that gives some level of power

#F-test
#Note: starting with app 4.0, t() are converted to F() and Z to chisq() to avoid unnecessary repetition of code

#Combine both in single function
getncp = function(family,df1,df2,power) {
  error = function(ncp_est, power, x, df1,df2) {pf(x, df1 = df1, df2 = df2, ncp = ncp_est) - (1 - power)}
  xc = qf(p = .95, df1 = df1,df2 = df2)
  ncp = uniroot(error, c(0, 1000), x = xc, df1 = df1,df2 = df2, power = power)$root
  return(ncp)
}


###############################################################################
#Function 3 - pbound: bound p-values and pp-values by precision of measurement to avoid errors
pbound = function(p) {pmin(pmax(p,2.2e-16),1 - 2.2e-16)}

###############################################################################

#Function 4 - prop33(pc) - Computes % of p-values that are expected to be smaller than pc,
#for the tests submitted to p-curve, if power is 33%
prop33 = function(pc) {
  #pc: critical  p-value
  
  #Overview:
  #Creates a vector of the same length as the number of tests submitted to p-curve, significant and not,
  #    and computes the proportion of p-values expected to be smaller than {pc} given the d.f.
  #    and outputs the entire vector, with NA values where needed
  
  #F-tests (& thus  t-tests)
  prop = ifelse(p < .05, 1 - pf(qf(1 - pc,df1 = df1, df2 = df2),df1 = df1, df2 = df2, ncp = ncp33), NA)

  prop
}

###############################################################################

#Function 5 Stouffer test for a vector of pp-values
stouffer = function(pp) {sum(qnorm(pp),na.rm = TRUE) / sqrt(sum(!is.na(pp)))}

###############################################################################
#(2) COMPUTE PP-VALUES
##############################################################################

# pp.values -prob of observing a significant p value at least as extreme if the null were true. 
ALPHA = .05
ppr.full = pbound(as.numeric(ifelse(p < ALPHA, p/ALPHA, NA)))   #If p<.05, ppr is 1/alpha*p-value, so 20*pvalue, otherwise missing. (essentially normalize probability by alpha) - assumes p values uniformly distributed?
ppr.half = pbound(as.numeric(ifelse(p < ALPHA/2, p/(ALPHA/2), NA)))    

# pps for power of 33%
pp33.full = pbound(ifelse(p < ALPHA , 3 * (pf(value, df1 = df1, df2 = df2, ncp = ncp33) - 2 / 3), NA))

prop25 = 3 * prop33(ALPHA/2) # share of p-values expected to be p<.025 if 33% power (using Function 4 from above, prop33() )
prop25.sig = prop25[p < ALPHA]
pp33.half = pbound(ifelse(p < ALPHA/2, (1 / prop25) * (pf(value,df1 = df1,df2 = df2,ncp = ncp33) - (1 - prop25)), NA))

###############################################################################
#(3) INFERENCE - STOUFFER 
##############################################################################

#3.1 Convert pp-values to Z scores, using Stouffer function above
Zppr.full =     stouffer(ppr.full)            #right skew  - this is a Z value from Stouffer's test
Zpp33.full =     stouffer(pp33.full)           #33% - idem
Zppr.half = stouffer(ppr.half)       #right skew, half p-curve - idem
Zpp33.half = stouffer(pp33.half)      #33% skew, half p-curve - idem

#3.2 Overall p-values from Stouffer test
p.Zppr.full = pnorm(Zppr.full)
p.Zpp33.full = pnorm(Zpp33.full)
p.Zppr.half = pnorm(Zppr.half)
p.Zpp33.half = pnorm(Zpp33.half)


################################################
#(4) POWER ESTIMATE
################################################

#4.1 Function powerfit(power_est) - Returns the Stouffer Z of observing at least as right skewed a p-curve if  power=power_est
#if Z=0, power_est is the best fit (p=50%).
#if Z<0 the best fit is <power_est,
#if Z>0 the best fit is >power_est
powerfit = function(power_est){
  #4.1.1 Get the implied non-centrality parameters (ncps) that give power_est to each test submitted to p-curve
  ncp_est = mapply(
    getncp,df1 = df1,df2 = df2,power = power_est,family = family
  )
  #4.1.2 Compute implied pp-values from those ncps_est,
  pp_est = ifelse(family == "f" &
                    p < .05,(pf(
                      value,df1 = df1,df2 = df2,ncp = ncp_est
                    ) - (1 - power_est)) / power_est,NA)
  pp_est = ifelse(family == "c" &
                    p < .05,(pchisq(value,df = df1,ncp = ncp_est) - (1 - power_est)) / power_est,pp_est)
  pp_est = pbound(pp_est)
  #4.1.3 Aggregate pp-values for null that power=power_est via Stouffer
  return(stouffer(pp_est))   #This is a z score, so powerfit is expressed as the resulting Z score.
}


#4.2 COMPUTE FIT FOR EACH POWER for 5.1%, AND THEN 6-99%, AND PLOT IT. With power=5% boundary condition lead to errors
#This becomes the diagnostic plot and gives us the best estimate, within 1%, of power.

#Create image file to contain results
png(
  filename = paste(filek,"_fit.png",sep = ""), width = 1200, height = 1000, res =
    200
)
# Fit will be evaluated at every possible value of power between 5.1% and 99% in steps of 1%, stored in fit()
fit = c()                                          #Create empty vector
fit = abs(powerfit(.051))                      #Start it eavaluting fit of 5.1% power
for (i in 6:99)
  fit = c(fit,abs(powerfit(i / 100))) #Now do 6% to 99%

# Find the minimum
#which ith power level considered leads to best estimate
mini = match(min(fit,na.rm = TRUE),fit)
#convert that into the power level, the ith value considered is (5+ith)/100
hat = (mini + 4) / 100
#Plot results
#create the x-axis
x.power = seq(from = 5,to = 99) / 100
#Draw the line
par(mar = c(5.1,8.1,4.1,2.1))  #Margins
plot(
  x.power,fit,xlab = "Underlying Power", ylab = "",ylim = c(-.15,max(fit)), main =
    ""
)
#Figure title
mtext(
  "Estimating underlying statistical power",side = 3,line = 1.75,cex = 1.5,at =
    0.4
)
mtext(
  "(Plot should be V shaped, or a smooth line to 99%; else don't trust estimate)",col =
    'red',side = 3,line = .5,cex = 1,at = 0.4
)
#Make red dot at the estimate
points(hat,min(fit,na.rm = TRUE),pch = 19,col = "red",cex = 2)
#Put a label with the estimate value
sign = "="
if (hat < .06)
  sign = "<"
text(
  min(.5,max(.28,hat)),min(fit,na.rm = TRUE) - .15,paste0("Estimated Power ",sign," ",hat *
                                                            100,"%")
)
#Label the y-axis
mtext(
  c("Good","Bad"),side = 2,line = 3,at = c(0,max(fit)),las = 1,cex = 1.25,col =
    c("blue","red")
)
mtext(
  "Fit for observed p-curve",side = 2,line = 6.5,cex = 1.5
)
mtext(
  "(Stouffer test for null of power in x-axis)\n|Z-score|",side = 2,line =
    4.5,col = "gray"
)
dev.off()


#4.3 Confidence interval for power estimate
#4.3.1 Function get.power_pct(pct)
get.power_pct = function(pct)   {
  #Function that finds power that gives p-value=pct for the Stouffer test
  #for example, get.power_pct(.5) returns the level of power that leads to p=.5  for the stouffer test.
  #half the time we would see p-curves more right skewed than the one we see, and half the time
  #less right-skewed, if the true power were that get.power_pct(.5). So it is the median estimate of power
  #similarliy, get.power_pct(.1) gives the 10th percentile estimate of power...
  #Obtain the normalized equivalent of pct, e.g., for 5% it is -1.64, for 95% it is 1.64
  z = qnorm(pct)  #convert to z because powerfit() outputs a z-score.
  #Quantify gap between computed p-value and desired pct
  error = function(power_est, z)
    powerfit(power_est) - z
  #Find the value of power that makes that gap zero, (root)
  return(uniroot(error, c(.0501, .99),z)$root)
}

#4.3.2 Boundary conditions (when the end of the ci=5% or 99% we cannot use root to find it,
#use boundary value instead)

#Boundary conditions
p.power.05 = pnorm(powerfit(.051)) #Proability p-curve would be at least at right-skewed if power=.051
p.power.99 = pnorm(powerfit(.99))  #Proability p-curve would be at least at right-skewed if power=.99

#4.3.3 Find lower end of ci
#Low boundary condition? If cannot reject 5% power, don't look for lower levels, use 5% as the end
if (p.power.05 <= .95)
  power.ci.lb = .05
#High boundary condition? If we reject 99%, from below dont look for higher power, use 99% as the low end
if (p.power.99 >= .95)
  power.ci.lb = .99
#If low bound is higher than 5.1% power and lower than 99% power, estimate it, find interior solution
if (p.power.05 > .95 &&
    p.power.99 < .95)
  power.ci.lb = get.power_pct(.95)


#4.3.4 Higher end of CI
#If we reject 5% power from below, 5% is above the confidence interval, use 5% as the upper end of the confidence interval
if (p.power.05 <= .05)
  power.ci.ub = .05
#If we do not reject that 99% power, don't look higher, use 99% as the higher end
if (p.power.99 >= .05)
  power.ci.ub = .99
#If the the upper bound is between 5% and 99%, find it
if (p.power.05 > .05 &&
    p.power.99 < .05)
  power.ci.ub = get.power_pct(.05)


#4.4 Save power fit estiate and ci
power_results = c(power.ci.lb,hat,power.ci.ub)
write(power_results, paste("POWERHAT_",filek,".txt", sep = ""),sep = "\n")

#Note, I use hat as the estimate of power, with powerfit(.5) we could get a more precise best fitting
#level of power than the minimum in the figure above between .051 and .99, hat, but more precision than 1% in power is not informative.


###############################################################################
#(5) MAIN FIGURE: OBSERVED P-CURVE AND EXPECTED UNDERL NULL AND 33% POWER
##############################################################################

#5.1 Green line (Expected p-curve for 33% power)
#5.1.1 Proportion of tests expected to get <01, <02...
#Uses FUNCTION 4, prop33() - see top of page
gcdf1 = prop33(.01)         #vector with proportion of p-values p<.01, with 33% power
gcdf2 = prop33(.02)         #              ""                   p<.02,      "
gcdf3 = prop33(.03)         #              ""                   p<.03,      "
gcdf4 = prop33(.04)         #              ""                   p<.04,      "
#Note: for p<.05 we know it is 33% power

#5.1.2 Now compute difference, and divide by 1/3 to get the share of significant p-values in each bin
green1 = mean(gcdf1) * 3        #Average of the vector p<.01
green2 = mean(gcdf2 - gcdf1) * 3  #Difference between .02 and .01
green3 = mean(gcdf3 - gcdf2) * 3  #Difference between .03 and .02
green4 = mean(gcdf4 - gcdf3) * 3  #Difference between .04 and .03
green5 = mean(1 / 3 - gcdf4) * 3    #Difference between .05 and .04
#Because we have one row per test submitted, the average is weighted average, giving each test equal weight
green = 100 * c(green1,green2,green3,green4,green5)  #The 5 values plotted in the figure for 33% power line


#5.2 The blue line (observed p-curve)
#Put all significant p-values  into bins, .01 ....05
ps = ceiling(p[p < .05] * 100) / 100
#Count # of tests in each bin
blue = c()
#This loop creates a vector, blue, with 5 elements, with the proportions of p=.01,p=.02...p=.05
for (i in c(.01,.02,.03,.04,.05))
  blue = c(blue,sum(ps == i,na.rm = TRUE) / ksig * 100)

#5.3 Red line
red = c(20,20,20,20,20)

#5.4 Make the graph
#Note: R_temp & filek are parameterS set at the beggining of the program for the location of files
png(
  filename = paste(filek,".png",sep = ""), width = 2600, height = 2400, res =
    400
)

#5.4.1  Define x-axis as p-values (.01, .02..)
x = c(.01,.02,.03,.04,.05)

#5.4.2 Plot the observed p-curve
par(mar = c(15,5.5,1.5,6))   #margins; very big bottom margin to fit table with results below

#5.4.3 Plot starts
plot(
  x,blue,   type = 'l', col = 'dodgerblue2',  main = "",lwd = 2, xlab = "", ylab =
    "", xaxt = "n",yaxt = "n", xlim = c(0.01,0.055), ylim = c(0,105), bty =
    'L', las = 1
);

#5.4.4 x-axis value labels
x_ = c(".01",".02",".03",".04",".05")
axis(1,at = x,labels = x_)
#5.4.5 y-axis value labels
y_ = c("0%","25%","50%","75%","100%")
y = c(0,25,50,75,100)
axis(
  2,at = y,labels = y_,las = 1,cex.axis = .75
)

#5.4.6 Add y-axis label
mtext(
  "Percentage of test results",font = 2,side = 2,line = 2.75,cex = 1.25
)
#5.4.7 Add y-axis label
mtext(
  "p            ",font = 4,side = 1,line = 2,cex = 1.25
)
mtext(
  " -value",font = 2,side = 1,line = 2,cex = 1.25
)

#5.4.8 Add little point in actual frequencies
points(
  x,blue,type = "p",pch = 20,bg = "dodgerblue2",col = "dodgerblue2"
)
#5.4.9 Add value-labels
text(x + .00075,blue + 5,percent(round(blue) / 100),col = 'black', cex =
       .75)
#5.4.10Add red and green lines
lines(
  x,red,   type = 'l', col = 'firebrick2',    lwd = 1.5, lty = 3
)
lines(
  x,green, type = 'l', col = 'springgreen4',  lwd = 1.5, lty = 5
)

#5.4.11 Legend
#By default in the middle
legendx = .035 ;
#Move left for p-curves that have more 80% of p-values =.04 or =.05 so that the legend does not touch blue line
if (blue[04] > 80 | blue[05] > 80)
  legendx = .015

#Print legend
legend(
  legendx, 100, c('Observed p-curve','Null of 33% power', 'Null of zero effect'),
  box.col = "white",lty = c(1,5,3), cex = .75,lwd = c(1,1),col =
    c('dodgerblue2','springgreen4', 'firebrick2')
);

#5.5 BEGINS TABLE EMBEDDED IN FIGURE
#5.5.1
a = -30             #Parameter for positioning all text, using when setting up first time to make it easy to adjust, every y-coordinate is based off "a"
par(xpd = TRUE)     #Allows printing off margins
col1 = "#40C4FF"    #Color for first and third header
col2 = "#80D8FF"    #Color for 2nd header

#5.5.2 Header 1 (binomial   |    stouffer)
rect(0,a,.065,a - 15,col = col1,border = NA)                  #Create the rectangle
text(.0275,a - 4,"Binomial Test",cex = .75, font = 2)
text(.0275,a - 10,"Share of p<.025",cex = .65, font = 3)
text(.05,a - 4,"Continuous Test",cex = .75, font = 2)
text(.05,a - 10,"Aggregate pp-values (Stouffer Method)",cex = .65, font =
       3)

#5.5.3 Header 2 (half     |      full)
rect(.035,a - 15,.065,a - 23,col = col2,border = NA)
text(.0425,a - 19,"Full p-curve (p's<.05)",cex = .65, font = 4)
text(.0575,a - 19,"Half p-curve (p's<.025)",cex = .65, font = 4)

#5.5.3 Right skew tests
#Label
text(0,a - 30,"1) Studies contain evidential value",cex = .65,adj =
       0)
text(0,a - 35,"          (Right-skew)",cex = .575,adj = 0,font =
       3)
#5.5.4 REsults right-skew
text(.0275,a - 30,paste0("p",binom.r),cex = .65,font = 3)

res.rf = paste0("Z=",Zppr,", p",p.Zppr)
text(.0425,a - 30,res.rf,cex = .65,font = 3)
#Z half
res.rh = paste0("Z=",Zppr.half,", p",p.Zppr.half)
text(.0575,a - 30,res.rh,cex = .65,font = 3)

#33% Power tests
#Label
text(0,a - 42,"2) Evidential value, if any, is inadequate",cex =
       .65,adj = 0)
text(
  0,a - 47,"          (Flatter than when power=33%)",cex = .575,adj = 0,font =
    3
)
#Binomial
text(.0275,a - 42,paste0("p",binom.33),cex = .65,font = 3)
#Z full
res.33f = paste0("Z=",Zpp33,", p",p.Zpp33)
text(.0425,a - 42,res.33f,cex = .65,font = 3)
#Z half
res.33h = paste0("Z=",Zpp33.half,", p",p.Zpp33.half)
text(.0575,a - 42,res.33h,cex = .65,font = 3)
#IF no p<.025, print red text
if (khalf == 0)
  text(
    .0575,a - 47,"(No p<.025 entered)",col = "red",cex = .65,font = 3
  )

#Power Estimate
#Header 3
rect(0,a - 50,.065,a - 60,col = col1,border = NA)
text(.045,a - 55,"Statistical Power",cex = .75,font = 2)
#Label
text(0,a - 65,"Power of tests included in p-curve",cex = .65,adj =
       0)
text(
  0,a - 70,"          (Correcting for publication bias)",cex = .575,adj =
    0,font = 3
)
#Estimate
hat_clean = paste0("Estimate: ",percent(hat))
text(.045,a - 65,hat_clean,cex = .65)
#CI
ci = paste0("Confidence interval: (",percent(power.ci.lb),","
            ,percent(power.ci.ub),")")
text(.045,a - 70,ci,cex = .65)
#Separating line
abline(h = a - 75,lty = 3,col = col1)
#Number of tests in p-curve
text(
  0,a - 80,adj = 0,paste0(
    "The observed p-curve includes ",ksig,
    " statistically significant (p<.05) results, of which ",khalf,
    " are p<.025."
  ),cex = .65
)
#Number of n.s. results entered
kns = ktot - ksig
if (kns == 0)
  ns_msg = "There were no non-significant results entered."
if (kns == 1)
  ns_msg = "There was one additional result entered but excluded from p-curve because it was p >.05."
if (kns > 1)
  ns_msg = paste0(
    "There were ",kns," additional results entered but excluded from p-curve because they were p>.05."
  )
text(0,a - 85,adj = 0,ns_msg,cex = .65)

dev.off()



