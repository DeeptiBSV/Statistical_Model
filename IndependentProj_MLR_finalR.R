
#Remember to install these packages if you haven't already done so

#First check that package required is installed, if not install it
# Specify your packages
needed_packages <- c("pastecs", "ggplot2", "semTools", "FSA","readxl","sqldf","statgazer")                                    
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed) 
library(pastecs) #For creating descriptive statistic summaries
library(ggplot2) #For creating histograms with more detail than plot
library(semTools) #For skewness and kurtosis
dim(student_performance)
library(sqldf)
library(dplyr)
library(stargazer)
spdf<-read_excel("data_academic_performance.xlsx")

#changing the rownames to lowercase to read the columns in much convinient way
colnames(spdf) <- tolower(colnames(spdf))
unique(spdf$g_sc)
unique(spdf$cc_s11)


# from these unique values we can see that there are no missing values
# we will first assess the Normality for cc_s11 and g_sc, then will evaluate its correlation
#We will allocate the histogram to a variable to allow use to manipulate it
gg <- ggplot(spdf, aes(x=spdf$g_sc))
gg
#Change the label of the x axis
gg <- gg + labs(x="student global score g_sc")
gg
#manage binwidth and colours
gg <- gg + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#adding a normal curve
#use stat_function to compute a normalised score for each value of g_sc 
#pass the mean and standard deviation
#use the na.rm parameter to say how missing values are handled
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(spdf$g_sc, na.rm=TRUE),
                                                          sd=sd(spdf$g_sc, na.rm=TRUE)))
#to display the graph request the contents of the variable be shown
gg

### g_sc Generate Q-Q Plot

#Create a qqplot
qqnorm(spdf$g_sc)
qqline(spdf$g_sc, col=2) #show a line on theplot
#stat.desc is a function from pastecs - make sure you include the basic switch=F to ensure you don't get scienfitic notation
pastecs::stat.desc(spdf$g_sc, basic=F)

#We can make our decision based on the standardised score for skew and kurtosis
#We divide the skew statistic by the standard error to get the standardised score
#This will tell us if we have a problem
tpskew<-semTools::skew(spdf$g_sc)
tpkurt<-semTools::kurtosis(spdf$g_sc)
tpskew[1]
tpskew[2]
tpskew[1]/tpskew[2]
tpkurt[1]/tpkurt[2]

#and by calculating the percentage of standardised scores for the variable itself that are outside our acceptable range
#this will tell us how big a problem we have
# Calculate the percentage of standardized scores that are greater than 1.96
# the perc function which is part of the FSA package which calculate the percentage that are within a range - you can look for greater than "gt", greater than or equal "geq", "gt", less than or equal "leq",  or less than "lt"),
# scale is a function that creates z scores
gsc<- abs(scale(spdf$g_sc))

FSA::perc(as.numeric(gsc), 1.96, "gt")
FSA::perc(as.numeric(gsc), 3.29, "gt")
##central tendency
round(mean(spdf$g_sc),2)
round(sd(spdf$g_sc),2)
length(spdf$g_sc)-sum(is.na(spdf$g_sc))


###  cc_s11 Generate Histogram -cc_s11

gg <- ggplot(spdf, aes(x=spdf$cc_s11))
gg
#Change the label of the x axis
gg <- gg + labs(x="student critical competency at highschool")
gg
#manage binwidth and colours
gg <- gg + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#adding a normal curve
#use stat_function to compute a normalised score for each value of cc_s11
#pass the mean and standard deviation
#use the na.rm parameter to say how missing values are handled
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(spdf$cc_s11, na.rm=TRUE),
                                                          sd=sd(spdf$cc_s11, na.rm=TRUE)))
#to display the graph request the contents of the variable be shown
gg

### cc_s11 Generate Q-Q Plot

#Create a qqplot
qqnorm(spdf$cc_s11)
qqline(spdf$cc_s11, col=2) #show a line on theplot
#stat.desc is a function from pastecs - make sure you include the basic switch=F to ensure you don't get scienfitic notation
pastecs::stat.desc(spdf$cc_s11, basic=F)

#We can make our decision based on the standardised score for skew and kurtosis
#We divide the skew statistic by the standard error to get the standardised score
#This will tell us if we have a problem
tpskew<-semTools::skew(spdf$cc_s11)
tpkurt<-semTools::kurtosis(spdf$cc_s11)
tpskew[1]
tpskew[2]
tpskew[1]/tpskew[2]
tpkurt[1]/tpkurt[2]

#and by calculating the percentage of standardised scores for the variable itself that are outside our acceptable range
#this will tell us how big a problem we have
# Calculate the percentage of standardized scores that are greater than 1.96
# the perc function which is part of the FSA package which calculate the percentage that are within a range - you can look for greater than "gt", greater than or equal "geq", "gt", less than or equal "leq",  or less than "lt"),
# scale is a function that creates z scores
cs11<- abs(scale(spdf$cc_s11))

FSA::perc(as.numeric(cs11), 1.96, "gt")
FSA::perc(as.numeric(cs11), 3.29, "gt")
##central tendency cc_s11
round(mean(spdf$cc_s11),2)
round(sd(spdf$cc_s11),2)
length(spdf$cc_s11)-sum(is.na(spdf$cc_s11))
#missing for g_sc
sum(is.na(spdf$g_sc))
#missing for cc_s11
sum(is.na(spdf$cc_s11))
unique(spdf$g_sc)
unique(spdf$cc_s11)
length(spdf$cc_s11)
length(spdf$g_sc)
scatter1 <- ggplot(spdf, aes(x=spdf$cc_s11, y=spdf$g_sc))

#Add a regression line
scatter1 + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "ccs11 score high school", y = "g_sc Global score ")  
stats::cor.test(spdf$g_sc, spdf$cc_s11, method='pearson')  

spdfmodel1<-lm(spdf$g_sc~spdf$cc_s11)
anova(spdfmodel1)
summary(spdfmodel1)
lm.beta::lm.beta(spdfmodel1)
stargazer(spdfmodel1, type="text") #Tidy output of all the required stats


#Get descriptive stastitics by group - output as a matrix
psych::describeBy(spdf$g_sc, spdf$gender, mat=TRUE)


#Conduct Levene's test for homogeneity of variance in library car - the null hypothesis is that variances in groups are equal so to assume homogeneity we woudl expect probaility to not be statistically significant.
car::leveneTest(g_sc ~ gender, data=spdf)
#Pr(>F) is  probability - in this case it is  statistically significant so we cannot assume homogeneity


#Conduct the t-test from package stats
#In this case we can use the var.equal = TRUE option to specify equal variances and a pooled variance estimate
stats::t.test(g_sc~gender,var.equal=FALSE,data=spdf)
#statistically significant difference was found

spdfres <- stats::t.test(g_sc~gender,var.equal=FALSE,data=spdf)

#Eta squared calculation
spdfeffes=round((spdfres$statistic*spdfres$statistic)/((spdfres$statistic*spdfres$statistic)+(spdfres$parameter)),3)
spdfeffes



### Multuplie Linear Regression - g_sc is predicted by cc_s11 including dummy variable for gender to investigate a differential effect by gender - variable sex

#Note: R automatically recodes categorical to be dummy variable 0 = reference (Female), 1 category of interest (Male)
spdfmodel2<-lm(spdf$g_sc~spdf$cc_s11+spdf$gender)
anova(spdfmodel2)
summary(spdfmodel2)
stargazer(spdfmodel2, type="text") #Tidy output of all the required stats
lm.beta::lm.beta(spdfmodel2)
stargazer(spdfmodel1, spdfmodel2, type="text") #Quick model comparison

##Model Assumptions Model1
## Assess how model meets key assumptions of linear regression

#Influential Outliers - Cook's distance
cooksdm1<-sort(cooks.distance(spdfmodel1))

# plot Cook's distance
plot(cooksdm1, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksdm1, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksdm1)+1, y=cooksdm1, labels=ifelse(cooksdm1>4*mean(cooksdm1, na.rm=T),names(cooksdm1),""), col="red")  # add labels
##Influential observations
influential1 <- as.numeric(names(cooksdm1)[(cooksdm1 > 4*mean(cooksdm1, na.rm=T))])  # influential row numbers
stem(influential1)
head(spdf[influential1, ])

car::outlierTest(spdfmodel1) # Bonferonni p-value for most extreme obs - Are there any cases where the outcome variable has an unusual variable for its predictor values?

car::leveragePlots(spdfmodel1) # leverage plots

#Assess homocedasticity 
plot(spdfmodel1,1)
plot(spdfmodel1, 3)
#The first plot is the chart of residuals vs fitted values, in the second plot the standardised residuals are on the Y axis. If there is absolutely no heteroscedastity, you should see a completely random, equal distribution of points throughout the range of X axis and a flat red line. We reall want to see that there is no pattern in the residuals and that they are equally spread around the y = 0 line - the dashed line.
#n our case, as you can notice the red line is slightly distorted in the middle on plot 1 but is not a big problem. Looking at the second plot we can see that while it is a problem it is not huge. Only a concern if there are definite patterns.

#Create histogram and  density plot of the residuals
plot(density(resid(spdfmodel1))) 

#Create a QQ plotqqPlot(model, main="QQ Plot") #qq plot for studentized resid 
car::qqPlot(spdfmodel1, main="QQ Plot") #qq plot for studentized resid

#Calculate Collinearity-Not needed as we have one 1 predictor

#Check assumptions for MODEL 2

#Influential Outliers - Cook's distance
cooksdm2<-sort(cooks.distance(spdfmodel2))
# plot Cook's distance
plot(cooksdm2, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksdm2, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksdm2)+1, y=cooksdm2, labels=ifelse(cooksdm2>4*mean(cooksdm2, na.rm=T),names(cooksdm2),""), col="red")  # add labels

#Influential observations
influential2 <- as.numeric(names(cooksdm2)[(cooksdm1 > 4*mean(cooksdm2, na.rm=T))])  # influential row numbers
stem(influential2)
head(spdf[influential2, ])


car::outlierTest(spdfmodel2) # Bonferonni p-value for most extreme obs - Are there any cases where the outcome variable has an unusual variable for its predictor values?

car::leveragePlots(spdfmodel2) # leverage plots

#Assess homocedasticity 
plot(spdfmodel2,1)
plot(spdfmodel2, 3)
#This is a much better result than model 1

#A density plot of the residuals
plot(density(resid(spdfmodel2))) 

#Create a QQ plot qPlot(model, main="QQ Plot") #qq plot for studentized resid 
car::qqPlot(spdfmodel2, main="QQ Plot Model 2") #qq plot for studentized resid

#Collinearity
vifspdfmodel2<-car::vif(spdfmodel2)
vifspdfmodel2
#Tolerance
1/vifspdfmodel2


###Model 5 mat_s11

scatter2 <- ggplot(spdf, aes(x=spdf$mat_s11, y=spdf$g_sc))

#Add a regression line
scatter2 + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "mats11 score high school", y = "g_sc Global score ")  
stats::cor.test(spdf$g_sc, spdf$mat_s11, method='pearson') 

spdfmodel5<-lm(spdf$g_sc~spdf$cc_s11+spdf$gender+spdf$mat_s11)
anova(spdfmodel5)
summary(spdfmodel5)
stargazer(spdfmodel5, type="text") #Tidy output of all the required stats
lm.beta::lm.beta(spdfmodel5)
stargazer(spdfmodel1, spdfmodel2, type="text") #Quick model comparison

#Model5 Assumtions
#Influential Outliers - Cook's distance
cooksdm5<-sort(cooks.distance(spdfmodel5))
# plot Cook's distance
plot(cooksdm5, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksdm5, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksdm5)+1, y=cooksdm5, labels=ifelse(cooksdm5>4*mean(cooksdm5, na.rm=T),names(cooksdm5),""), col="red")  # add labels

#Influential observations
influential3 <- as.numeric(names(cooksdm5)[(cooksdm5 > 4*mean(cooksdm5, na.rm=T))])  # influential row numbers
stem(influential3)
head(spdf[influential3, ])


car::outlierTest(spdfmodel5) # Bonferonni p-value for most extreme obs - Are there any cases where the outcome variable has an unusual variable for its predictor values?

car::leveragePlots(spdfmodel5) # leverage plots

#Assess homocedasticity 
plot(spdfmodel5,1)
plot(spdfmodel5, 3)
#This is a much better result than model 1

#A density plot of the residuals
plot(density(resid(spdfmodel5))) 

#Create a QQ plot qPlot(model, main="QQ Plot") #qq plot for studentized resid 
car::qqPlot(spdfmodel5, main="QQ Plot Model 3") #qq plot for studentized resid

#Collinearity
vifspdfmodel5<-car::vif(spdfmodel5)
vifspdfmodel5
#Tolerance
1/vifspdfmodel5

##Model Comparison
stargazer(spdfmodel1, spdfmodel2, spdfmodel5,type="text") #Quick model comparison

summary(spdfmodel5)
lm.beta::lm.beta(spdfmodel1)
lm.beta::lm.beta(spdfmodel2)
lm.beta::lm.beta(spdfmodel5)




