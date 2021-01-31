#READ the file in to R and view the dataset

student_performance<-read.table("sperformance-dataset.csv", header = TRUE, sep = ',')

#changing the rownames to lowercase to read the columns in much convinient way
colnames(student_performance) <- tolower(colnames(student_performance))
#View(student_perform)
#Remember to install these packages if you haven't already done so

#First check that package required is installed, if not install it
# Specify your packages
needed_packages <- c("pastecs", "ggplot2", "semTools", "FSA")                                    
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed)                              


model1
library(pastecs) #For creating descriptive statistic summaries
library(ggplot2) #For creating histograms with more detail than plot
library(semTools) #For skewness and kurtosis
dim(student_performance)
library(sqldf)
library(dplyr)
library(stargazer)
pgna<-sqldf('select pg3, count(pg3) as pg3count from student_performance where pg3==0')
pgna



#We will allocate the histogram to a variable to allow use to manipulate it
gg <- ggplot(student_perform, aes(x=student_performance$pg3))
gg
#Change the label of the x axis
gg <- gg + labs(x="student final Grade for Portuguese")
gg
#manage binwidth and colours
gg <- gg + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#adding a normal curve
#use stat_function to compute a normalised score for each value of PG3 final grade in Portugues
#pass the mean and standard deviation
#use the na.rm parameter to say how missing values are handled
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(student_performance$pg3, na.rm=TRUE),
                                                          sd=sd(student_performance$pg3, na.rm=TRUE)))
#to display the graph request the contents of the variable be shown
gg

### pg3 Generate Q-Q Plot

#Create a qqplot
qqnorm(student_performance$pg3)
qqline(student_performance$pg3, col=2) #show a line on theplot
#stat.desc is a function from pastecs - make sure you include the basic switch=F to ensure you don't get scienfitic notation
pastecs::stat.desc(student_performance$pg3, basic=F)

#We can make our decision based on the standardised score for skew and kurtosis
#We divide the skew statistic by the standard error to get the standardised score
#This will tell us if we have a problem
tpskew<-semTools::skew(student_performance$pg3)
tpkurt<-semTools::kurtosis(student_performance$pg3)
tpskew[1]
tpskew[2]
tpskew[1]/tpskew[2]
tpkurt[1]/tpkurt[2]

#and by calculating the percentage of standardised scores for the variable itself that are outside our acceptable range
#this will tell us how big a problem we have
# Calculate the percentage of standardized scores that are greater than 1.96
# the perc function which is part of the FSA package which calculate the percentage that are within a range - you can look for greater than "gt", greater than or equal "geq", "gt", less than or equal "leq",  or less than "lt"),
# scale is a function that creates z scores
zpg3<- abs(scale(student_performance$pg3))

FSA::perc(as.numeric(zpg3), 1.96, "gt")
FSA::perc(as.numeric(zpg3), 3.29, "gt")

#ks.test(student_performance$pg3, "pnorm", mean=mean(student_performance$pg3), sd=sd(student_performance$pg3))
#shapiro.test(student_performance$pg3)

###  pg2 Generate Histogram -Grade 2 in Portuguese

#Create histogram
pgg2 <- ggplot(student_perform, aes(x=student_performance$pg2))
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

##Report assess of Normality

##pg3 data was assessed for normality. Visual inspection of the histogram and QQ-Plot (see Figure 1 and Figure 2) identified some issues with skewness and kurtosis. The standardised score for kurtosis (`r round(tpkurt[1]/tpkurt[2],2)`) can be considered acceptable using the criteria proposed by West, Finch and Curran (1996), but the standardised score for skewness (`r round(tpskew[1]/tpskew[2],2)`)  was outside the acceptable range.  However over 99% of standardised scores for total optimism and associated sensation fall within the bounds of +/- 3.29, using the guidance of Field, Miles and Field (2013) the data can be considered to approximate a normal distribution (m=`r round(mean(student_performance$pg3, na.rm=TRUE),2)`, sd=`r round(sd(student_performance$pg3, na.rm=TRUE),2)`, n=`r

### CORRELATION
#### Scatterplot

#Simple scatterplot of final grade and Grade 2 for Portuguese
#aes(x,y)
student_performance$pG2
library(ggplot2)
scatter <- ggplot(student_performance, aes(x=student_performance$pg2, y=student_performance$pg3))

#Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "pg2 grade 2 of Portuguese", y = "pg3 Final grade Portuguese ") 

#Pearson Correlation
stats::cor.test(student_performance$pg3, student_performance$pg2, method='pearson')
unique(student_performance$pg3)
student_performance$pg3<-na_if(student_performance$pg3,0)
sum(is.na(student_performance))

##Multiple Linear Regression:
### Simple Linear Regression -pg3 predicted by pg2
### Note: Doing simple linear regression is equal to Pearson correlation
model1<-lm(student_performance$pg3~student_performance$pg2)
anova(model1)
summary(model1)
lm.beta::lm.beta(model1)
stargazer(model1, type="text") #Tidy output of all the required stats

##Independent t-test
##### Describe pg3 by group (Male v Female - Sex)

#Get descriptive stastitics by group - output as a matrix
psych::describeBy(student_performance$pg3, student_performance$sex, mat=TRUE)


#Conduct Levene's test for homogeneity of variance in library car - the null hypothesis is that variances in groups are equal so to assume homogeneity we woudl expect probaility to not be statistically significant.
car::leveneTest(pg3 ~ sex, data=student_performance)
#Pr(<F) is  probability - in this case it is  statistically significant so we cannot assume homogeneity


#Conduct the t-test from package stats
#In this case we can use the var.equal = TRUE option to specify equal variances and a pooled variance estimate
stats::t.test(pg3~sex,var.equal=FALSE,data=student_performance)
#statistically significant difference was found

res <- stats::t.test(pg3~sex,var.equal=FALSE,data=student_performance)


#Eta squared calculation
effes=round((res$statistic*res$statistic)/((res$statistic*res$statistic)+(res$parameter)),3)
effes

#no N1=324, N2=58



### Multuplie Linear Regression - pg3 is predicted by pg2 including dummy variable for sex to investigate a differential effect by gender - variable sex

#Note: R automatically recodes categorical to be dummy variable 0 = reference (Female), 1 category of interest (Male)
model2<-lm(student_performance$pg3~student_performance$pg2+student_performance$sex)
anova(model2)
summary(model2)
stargazer(model2, type="text") #Tidy output of all the required stats
lm.beta::lm.beta(model2)
stargazer(model1, model2, type="text") #Quick model comparison

##Model 3 with interaction term which is a new variable generated by multiplying "pg2" and "gender"
##add new variable gender as dummy code for sex where Male==1 and Female==0
student_performance$gender=ifelse(student_performance$sex == "M", 1, ifelse(student_performance$sex == "F", 0, NA))
student_performance$gender
student_performance$interactionp<-student_performance$pg2*as.numeric(student_performance$gender)
student_performance$interactionp
model3<-lm(student_performance$pg3~student_performance$pg2+student_performance$sex+
             student_performance$interactionp)
anova(model3)
summary(model3)
lm.beta::lm.beta(model3)
stargazer::stargazer(model3, type="text") #Tidy output of all the required stats
stargazer::stargazer(model2, model3, type="text") #Quick model comparison

##Model Assumptions Model1
## Assess how model meets key assumptions of linear regression

#Influential Outliers - Cook's distance
cooksd<-sort(cooks.distance(model1))

# plot Cook's distance
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels


car::outlierTest(model1) # Bonferonni p-value for most extreme obs - Are there any cases where the outcome variable has an unusual variable for its predictor values?

car::leveragePlots(model1) # leverage plots

#Assess homocedasticity 
plot(model1,1)
plot(model1, 3)
#The first plot is the chart of residuals vs fitted values, in the second plot the standardised residuals are on the Y axis. If there is absolutely no heteroscedastity, you should see a completely random, equal distribution of points throughout the range of X axis and a flat red line. We reall want to see that there is no pattern in the residuals and that they are equally spread around the y = 0 line - the dashed line.
#n our case, as you can notice the red line is slightly distorted in the middle on plot 1 but is not a big problem. Looking at the second plot we can see that while it is a problem it is not huge. Only a concern if there are definite patterns.

#Create histogram and  density plot of the residuals
plot(density(resid(model1))) 

#Create a QQ plotqqPlot(model, main="QQ Plot") #qq plot for studentized resid 
car::qqPlot(model1, main="QQ Plot") #qq plot for studentized resid

#Calculate Collinearity-Not needed as we have one 1 predictor


#Check assumptions for MODEL 2

#Influential Outliers - Cook's distance
cooksd<-sort(cooks.distance(model2))
# plot Cook's distance
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels




car::outlierTest(model2) # Bonferonni p-value for most extreme obs - Are there any cases where the outcome variable has an unusual variable for its predictor values?

car::leveragePlots(model2) # leverage plots

#Assess homocedasticity 
plot(model2,1)
plot(model2, 3)
#This is a much better result than model 1

#A density plot of the residuals
plot(density(resid(model2))) 

#Create a QQ plot qPlot(model, main="QQ Plot") #qq plot for studentized resid 
car::qqPlot(model2, main="QQ Plot Model 2") #qq plot for studentized resid

#Collinearity
vifmodel2<-car::vif(model2)
vifmodel2
#Tolerance
1/vifmodel2

#Check assumptions for model 3

#Influential Outliers - Cook's distance
cooksd<-sort(cooks.distance(model3))
# plot Cook's distance
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels


car::outlierTest(model3) # Bonferonni p-value for most extreme obs - Are there any cases where the outcome variable has an unusual variable for its predictor values?

car::leveragePlots(model3) # leverage plots

#Assess homocedasticity 
plot(model3,1)
plot(model3, 3)
#This is a much better result than model 1

#A density plot of the residuals
plot(density(resid(model3))) 

#Create a QQ plot qPlot(model, main="QQ Plot") #qq plot for studentized resid 
car::qqPlot(model3, main="QQ Plot Model 2") #qq plot for studentized resid

#Collinearity
vifmodel3<-car::vif(model3)
vifmodel3
#Tolerance
1/vifmodel3


