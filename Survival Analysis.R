                                            ## Survival Analysis                                   

#Limitations
##Kaplan-Meier Curves and Log-Rank tests;
##Kaplan Meier curves and logrank tests, are useful only when the predictor variable is categorical; they don't
#necessarily work for quantitative predictors such as gene expression, weight, or age.
#-------------------------------------------
## Cox Proportional Regression model, works for quantitative predictors as well as categorical predictors; it also extends
## survival analysis methods to assess simultaneously the effect of several risk factors on survival time;
                      ## Cox Model is represented by a hazard function denoted by h(t)
## Briefly, the hazard function can be interpreted as the risk of dying at time (t). It can be estimated as follows:
                        
                      ## h(t) = ho(t)*exp(b1x1+b2x2+b3x3.......+bpxp)
#----------------------------------------------  
#where -----> t represents the survival time;
## h(t) is the hazard function determined by the set of p co-variates (x1,x2,x3......xp);
## the co-efficients (b1,b2,b3,b4......bp) measure the impact of the co-variates.

#----------------------------------------------

                      ##Computing the Cox Model in R
## Loading the required 'R' packages Survival analysis ---> 1.Survival 2.Survminer
install.packages("survival")  ### for survival analysis;
install.packages("survminer") ### for visualizing the hazard function;
## Loading the requisite libraries;
library(survival)
library('survminer')

## Testing proportional hazard assumption;
## The proportional hazard assumption can be checked using statistical tests, and graphical diagnosis based on the scaled Schoenfeld residuals;
## In principle, the Schoenfeld residuals are independent of time. A plot showing a non-random pattern against time is a evidence of violation of the PH Assumption.
## The function cox.zph() [in the survival package] provides a convenient solution to test the proportional hazards assumption for each covariate included in a Cox refression model fit.
## For each covariate, the function cox.zph() correlates the corresponding set of scaled Schoenfeld residuals with time, to test for independence between residuals and time. Additionally, it performs a global test for the model as a whole
       
   res.cox <- coxph(Surv(time,status)~age+sex+wt.loss, data=lung) 
   res.cox

## To test for the proportional hazard (PH) assumption, we use;
   test.ph <- cox.zph(res.cox)
   test.ph

#From the output above, the test is not statistically significant for each of the covariates, and the global test is also not statistically significant. Therefore, we can assume the proportional hazards.
   ggcoxzph(test.ph)   
   #It's possible to do a graphical diagnostic using the function ggcoxzph() [in the survminer package], which produces, for each covariate, graphs of the scaled Schoenfeld residuals against the transformed time
   ## In the figure above, the solid line is a smoothing spline fit to the plot, with the dashed lines representing a +/- 2-standard-error band around the fit.
   ## From the graphical inspection, there is no pattern with time. The assumption of proportional hazards appears to be supported for the covariates sex (which is, recall, a two-level factor, accounting for the two bands in the graph), wt.loss and age.
   
## Testing influential observations
   #To test influential observations or outliers, we can visualize either:
   #The deviance residuals or the dfbeta values
   #The function ggcoxdiagnostics()[in survminer package] provides a convenient solution for checking the influential observations. The simplified format is as follow:

   #fit: an object of class coxph.object
   #type: the type of residuals to present on Y axis. Allowed values include one of c("martingale", "deviance", "score", "schoenfeld", "dfbeta", "dfbetas", "scaledsch", "partial").
   #linear.predictions: a logical value indicating whether to show linear predictions for observations (TRUE) or just indexed of observations (FALSE) on X axis.
   
   ggcoxdiagnostics(res.cox, type = "dfbeta",
                    linear.predictions = FALSE, ggtheme = theme_bw()) 
   ##The above index plots show that comparing the magnitudes of the largest dfbeta values to the regression coefficients suggests that none of the observations is terribly influential individually, 
   ##Even though some of the dfbeta values for age and wt.loss are large compared with the others.
   
   ggcoxdiagnostics(res.cox, type = "deviance",
                    linear.predictions = FALSE, ggtheme = theme_bw())
##  Testing Non-Linearity;
   #Plotting the Martingale residuals against continuous covariates is a common approach used to detect nonlinearity or, in other words, to assess the functional form of a covariate. For a given continuous covariate, patterns in the plot may suggest that the variable is not properly fit.
   #Martingale residuals may present any value in the range (-INF, +1):
   
   #A value of martinguale residuals near 1 represents individuals that "died too soon",
   #and large negative values correspond to individuals that "lived too long".

## To Access the Function form of Age;
   ggcoxfunctional(Surv(time, status) ~ age + log(age) + sqrt(age), data = lung)
   
   ##The function ggcoxfunctional() displays graphs of continuous covariates against martingale residuals of null cox proportional hazards model. This might help to properly choose the functional form of continuous variable in the Cox model. 
   ##Fitted lines with lowess function should be linear to satisfy the Cox proportional hazards model assumptions.
   
 ## Now computing the Cox Model in R;
   data("lung")
   View(lung)
  
  res.cox <- coxph(Surv(time, status) ~ sex, data = lung)
  res.cox 
  summary(res.cox)
#{1*} --- > Summary explanation at the end;
  ## To apply the univariate coxph function to multiple covariates at once, type this:
  
  covariates <- c("age", "sex",  "ph.karno", "ph.ecog", "wt.loss")
  univ_formulas <- sapply(covariates,
                          function(x) as.formula(paste('Surv(time, status)~', x)))  
  univ_models <- lapply( univ_formulas, function(x){coxph(x, data = lung)})
  
  univ_results <- lapply(univ_models,
                         function(x){ 
                           x <- summary(x)
                           p.value<-signif(x$wald["pvalue"], digits=2)
                           wald.test<-signif(x$wald["test"], digits=2)
                           beta<-signif(x$coef[1], digits=2);#coeficient beta
                           HR <-signif(x$coef[2], digits=2);#exp(beta)
                           HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                           HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                           HR <- paste0(HR, " (", 
                                        HR.confint.lower, "-", HR.confint.upper, ")")
                           res<-c(beta, HR, wald.test, p.value)
                           names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                         "p.value")
                           return(res)
                           #return(exp(cbind(coef(x),confint(x))))
                         })
  
  res <- t(as.data.frame(univ_results, check.names = FALSE))
  as.data.frame(res)
  
  #beta HR (95% CI for HR) wald.test p.value
  #age       0.019            1 (1-1)       4.1   0.042
  #sex       -0.53   0.59 (0.42-0.82)        10  0.0015
  #ph.karno -0.016      0.98 (0.97-1)       7.9   0.005
  #ph.ecog    0.48        1.6 (1.3-2)        18 2.7e-05
  #wt.loss  0.0013         1 (0.99-1)      0.05    0.83

#Multivariate Cox regression analysis

  res.cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data =  lung)
  summary(res.cox)

#Visualizing the estimated distribution of survival times
  #Having fit a Cox model to the data, 
  #it's possible to visualize the predicted survival proportion at any given point in time for a particular risk group. The function survfit() estimates the survival proportion, by default at the mean values of covariates.
  
  # Plot the baseline survival function
  ggsurvplot(survfit(res.cox), color = "#2E9FDF", ggtheme = theme_minimal(), data=lung)
  
  #Consider that, we want to assess the impact of the sex on the estimated survival probability. 
  #In this case, we construct a new data frame with two rows, one for each value of sex; the other covariates are fixed to their average values (if they are continuous variables) or to their lowest level (if they are discrete variables). For a dummy covariate, the average value is the proportion coded 1 in the data set. This data frame is passed to survfit() via the newdata argument:
  
  sex_df <- with(lung,
                 data.frame(sex = c(1, 2), 
                            age = rep(mean(age, na.rm = TRUE), 2),
                            ph.ecog = c(1, 1)
                 )
  )
  
  sex_df
  
  # Survival curves
  fit <- survfit(res.cox, newdata = sex_df)
  ggsurvplot(fit, conf.int = TRUE, legend.labs=c("Sex=1", "Sex=2"),
             ggtheme = theme_minimal(), data=sex_df)

  ?rep
  