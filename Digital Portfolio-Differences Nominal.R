
student_performance<-read.csv("sperformance-dataset.csv", header = TRUE, sep = ',')

needed_packages <- c("sjstats", "gmodels")                      
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed) 

library(gmodels) #For creating histograms with more detail than plot
library(sjstats)#chi-square effect size


##Comparing Nominal Variables
###Contingency table

#Use the Crosstable function
#CrossTable(predictor, outcome, fisher = TRUE, chisq = TRUE, expected = TRUE)
gmodels::CrossTable(student_performance$sex, student_performance$romantic.p, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

#more simplistic way of doing Chi-Square

#Create your contingency table
mytable<-xtabs(~romantic.p+sex, data=student_performance)

ctest<-stats::chisq.test(mytable, correct=TRUE)#chi square test
#correct=TRUE to get Yates correction needed for 2x2 table

ctest#will give you the details of the test statistic and p-value
ctest$expected#expected frequencies
ctest$observed#observed frequencies
ctest$p.valu

#Calculate effect size
sjstats::phi(mytable)
sjstats::cramer(mytable)

