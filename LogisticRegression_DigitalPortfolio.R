### Preliminaries

needed_packages <- c("foreign",  "Epi", "arm", "DescTools", "stargazer", "lmtest",  "car", "generalhoslem", "regclass","dplyr","sqldf")                      
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed, repos = "http://cran.us.r-project.org") 

library(Epi)#ROC Curve
library(DescTools)#Pseudo Rsquare statistics
library(stargazer)
library(foreign)#read SPSS file.
library(arm)#for invlogit calculating predicted probabilities
library(lmtest)#Simple calculation of Chi-square for model
library(car)#Needed to test for colinearity of predictors
library(generalhoslem)#Needed to test assumption of linearity
library("regclass")#For confusion matrix
library(dplyr)
library(sqldf)




#Read in file needed

student_performance<-read.table("sperformance-dataset.csv", header = TRUE, sep = ',')

#changing the rownames to lowercase to read the columns in much convineNIENT way
colnames(student_performance) <- tolower(colnames(student_performance))
#adding gender column for getting numeric values for sex variable

student_performance$higher.p<-as.factor(student_performance$higher.p)
student_performance$higher.p
#Check your proportions of the outcome variable for bias - are these representative?
table(student_performance$higher.p)
## Build first model with gender as predictor

#Make sure categorical data is used as factors

logmodel1 <- glm(higher.p ~ sex, data = student_performance, na.action = na.exclude, family = binomial(link=logit))

#Full summary of the model
summary(logmodel1)

#Chi-square plus significance
lmtest::lrtest(logmodel1)


#Chi-square and Pseudo R2 calculation - THESE ARE INCLUDED FOR INFORMATION ONLY
#lmtest:lrtest achieves the same thing
modelChi <- logmodel1$null.deviance - logmodel1$deviance
modelChi

pseudo.R2 <- modelChi / logmodel1$null.deviance
pseudo.R2

chidf <- logmodel1$df.null - logmodel1$df.residual
chidf

chisq.prob <- 1 - pchisq(modelChi, chidf)
chisq.prob

#Output the sensitivity, specificity, and ROC plot
Epi::ROC(form=student_performance$higher.p ~ student_performance$sex, plot="ROC")

#Pseudo Rsquared 
DescTools::PseudoR2(logmodel1, which="CoxSnell")
DescTools::PseudoR2(logmodel1, which="Nagelkerke")

#Summary of the model with co-efficients
stargazer(logmodel1, type="text")

#Exponentiate the co-efficients
exp(coefficients(logmodel1))
## odds ratios and 95% CI 
cbind(Estimate=round(coef(logmodel1),4),
      OR=round(exp(coef(logmodel1)),4))

#Probability of answering yes when female 
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*0)#YES this is the same as just having the 1st co-efficient
#Probability of answering yes when male 
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*1)
#r checkassumptionsmodel1}
#Check the assumption of linearity of independent variables and log odds using a Hosmer-Lemeshow test, if this is not statistically significant we are ok
#Won't give a p-value here because only one predictor
generalhoslem::logitgof(student_performance$higher.p, fitted(logmodel1))

#We would check for collinearity here but as we only have one predictor it doesn't make sense - check model 2 to see how to do this


## Now extend the model to include 
#Make sure categorical data is used as factors



s<-sqldf('select fedu, count(fedu) as fc from student_performance group by fedu')
s
###creating a new varable called fedu_log by grouping the ranking data of 5 to 3
##fedu=0 and 1 combined to less_educated father
#fedu=2 is educated father
#fedu=3 and 4 are combined to high_educated father
student_performance$fedu_log<-ifelse(student_performance$fedu == 0|student_performance$fedu==1,"less_educated",
                                    ifelse(student_performance$fedu == 2,"educated",
                                    ifelse(student_performance$fedu == 3|student_performance$fedu==4,"high_educated",NA)))
student_performance$fedu_log
sedu<-sqldf('select fedu_log, count(fedu_log) as fc from student_performance group by fedu_log')
sedu
logmodel2 <- glm(higher.p ~ sex+fedu_log, data = student_performance, na.action = na.exclude, family = binomial(link=logit))
#Summary of the model with co-efficients
stargazer(logmodel2, type="text")

#Full summary of the model
summary(logmodel2)

#Chi-square plus significance
lmtest::lrtest(logmodel2)

#Pseudo Rsquared 
DescTools::PseudoR2(logmodel2, which="CoxSnell")
DescTools::PseudoR2(logmodel2, which="Nagelkerke")

#Output the sensitivity, specificity, and ROC plot
Epi::ROC(form=student_performance$higher.p ~ student_performance$sex+student_performance$fedu_log, plot="ROC")

#{r predictorsmodel2}
#Exponentiate the co-efficients
exp(coefficients(logmodel2))
## odds ratios 
cbind(Estimate=round(coef(logmodel2),4),
      OR=round(exp(coef(logmodel2)),4))



#fedu_log 1 less_educated father, 2 educated father, 3 high_educated father
#1. Probability of answering yes when female and less educated father
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*0)
#2. Probability of answering yes when male and less educated father
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*1)

#3.Probability of answering yes when female when educated father
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*0 +coef(logmodel2)[3]*1+coef(logmodel2)[3]*0)
#4. Probability of answering yes when male when educated father
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*1 +coef(logmodel2)[3]*1+coef(logmodel2)[3]*0)

#5.Probability of answering yes when female when high educated father
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*0 +coef(logmodel2)[3]*0+coef(logmodel2)[3]*1)
#6.Probability of answering yes when male when high educated father
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*1 +coef(logmodel2)[3]*0+coef(logmodel2)[3]*1)

#Confusion matrix
regclass::confusion_matrix(logmodel2)


##{r checkassumptionsmodel2}

#Check the assumption of linearity of independent variables and log odds using a Hosmer-Lemeshow test, if this is not statistically significant we are ok

generalhoslem::logitgof(student_performance$higher.p, fitted(logmodel2))


#Collinearity
vifmodel<-car::vif(logmodel2)#You can ignore the warning messages, GVIF^(1/(2*Df)) is the value of interest
vifmodel
#Tolerance
1/vifmodel
####################################################end##############################
