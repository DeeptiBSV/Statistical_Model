#Anova Differences more than 2 group. will assess goout and pg2
##READ the file in to R and view the dataset

student_performance<-read.table("sperformance-dataset.csv", header = TRUE, sep = ',')


#changing the rownames to lowercase to read the columns in much convineNIENT way
colnames(student_performance) <- tolower(colnames(student_performance))

#Remember to install these packages if you haven't already done so

#First check that package required is installed, if not install it
# Specify your packages
needed_packages <- c("pastecs", "ggplot2", "semTools", "FSA","dplyr","psych")                                    
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed)                              



library(pastecs) #For creating descriptive statistic summaries
library(ggplot2) #For creating histograms with more detail than plot
library(semTools) #For skewness and kurtosis
library(dplyr)
library(psych)
dim(student_performance)
###  gp2 Generate Histogram -Grade 2 in Portuguese
#Create histogram
pgg2 <- ggplot(student_performance, aes(x=student_performance$pg2))
pgg2
pgg2 <- pgg2 + labs(x="Potugues Grade 2")
pgg2 <- pgg2 + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
pgg2 <- pgg2 + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
pgg2 <- pgg2 + stat_function(fun=dnorm, color="red",args=list(mean=mean(student_performance$pg2, na.rm=TRUE), sd=sd(student_performance$pg2, na.rm=TRUE)))
pgg2



###  pg2 Generate Q-Q Plot-Grade 2 in Portuguese

#Create a qqplot
qqnorm(student_performance$pg2)
qqline(student_performance$pg2, col=2) #show a line on the plot

### PG2 Generate Summary Statistics

#Generate regular summary statistics - lots of packages offer mechanisms to do this
pastecs::stat.desc(student_performance$pg2, basic=F)
round(mean(student_performance$pg2))
round(sd(student_performance$pg2))


#We can make our decision based on the standardised score for skew and kurtosis
#We divide the skew statistic by the standard error to get the standardised score
#This will tell us if we have a problem
tpskew<-semTools::skew(student_performance$pg2)
tpkurt<-semTools::kurtosis(student_performance$pg2)
tpskew[1]/tpskew[2]
tpkurt[1]/tpkurt[2]

#and by calculating the percentage of standardised scores for the variable itself that are outside our acceptable range
#this will tell us how big a problem we have
# Calculate the percentage of standardised scores that are greated than 1.96
# the perc function which is part of the FSA package which calculate the percentage that are within a range - you can look for greater than "gt", greater than or equal "geq", "gt", less than or equal "leq",  or less than "lt"),
# scale is a function that creates z scores
zpgg2<- abs(scale(student_performance$pg2))

FSA::perc(as.numeric(zpgg2), 1.96, "gt")
FSA::perc(as.numeric(zpgg2), 3.29, "gt")

##Now we can check the number count in each group in varaible goout.p
#creating a new variable for goout.p as goout_p to reduce the groups
#Group 1 and 2 combined as 1
#Group 3 as 2
#Group4 and 5 combined as 3

student_performance$goout_p<-ifelse(student_performance$goout.p == 1|student_performance$goout.p==2,1,
                            ifelse(student_performance$goout.p == 4|student_performance$goout.p==5, 3,
                            ifelse(student_performance$goout.p == 3, 2,NA)))

##pg2 has been assesed for Normality.
## Differences more than 2 groups - Parametric Tests
#### ANOVA


##{r Describe}
#Get descriptive stastitics by group - output as a matrix
psych::describeBy(student_performance$pg2, student_performance$goout_p, mat=TRUE)

#Conduct Bartlett's test for homogeneity of variance in library car - the null hypothesis is that variances in groups are equal so to assume homogeneity we would expect probability to not be statistically significant.
stats::bartlett.test(pg2~goout_p, data=student_performance)
#p value is > 0.05 so the result is not statistically significant so we can assume homogeneity


#Conduct ANOVA using the userfriendlyscience test oneway
#In this case we cannot use Tukey as the post-hoc test option since variances in the groups are equal
#If variances were not equal we would use Games-Howell
userfriendlyscience::oneway(as.factor(student_performance$goout_p),y=student_performance$pg2,posthoc='Games-Howell')

#use the aov function - same as one way but makes it easier to access values for reporting
res2<-stats::aov(pg2~ goout_p, data = student_performance)
#Get the F statistic into a variable to make reporting easier
fstat<-summary(res2)[[1]][["F value"]][[1]]
#Get the p value into a variable to make reporting easier
aovpvalue<-summary(res2)[[1]][["Pr(>F)"]][[1]]
#Calculate effect
aoveta<-sjstats::eta_sq(res2)[2]
aoveta

#In the report we are using the res2 variable to retrieve the degrees of freedom
#and the eta_sq function from the sjstats package to calculate the effect
#A statistically significant difference was found so we need to examine and report the post-hoc results also.

################################################END####################################################
