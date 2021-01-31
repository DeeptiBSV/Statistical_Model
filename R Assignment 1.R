
#READ the file in to R and view the dataset

student_perform<-read.table("sperformance-dataset.csv", header = TRUE, sep = ',')

#changing the rownames to lowercase to read the columns in much convineNIENT way
colnames(student_perform) <- tolower(colnames(student_perform))


#First check that package required is installed, if not install it
# Specify your packages
needed_packages <- c("pastecs", "ggplot2", "semTools", "FSA")                                    
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed)                       

library(pastecs) #For creating descriptive statistic summaries
library(ggplot2) #For creating histograms with more detail than plot
library(semTools) #For skewness and kurtosis
dim(student_performance)

#We will allocate the histogram to a variable to allow use to manipulate it
gg <- ggplot(student_perform, aes(x=student_perform$pg3))
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
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(student_perform$pg3, na.rm=TRUE),
sd=sd(student_perform$pg3, na.rm=TRUE)))
#to display the graph request the contents of the variable be shown
gg

### pg3 Generate Q-Q Plot
#Create a qqplot
qqnorm(student_perform$pg3)
qqline(student_perform$pg3, col=2) #show a line on theplot
#stat.desc is a function from pastecs - make sure you include the basic switch=F to ensure you don't get scienfitic notation
pastecs::stat.desc(student_perform$pg3, basic=F)
#central Tendency for pg3
round(mean(student_perform$pg3))
round(sd(student_perform$pg3))

#We can make our decision based on the standardised score for skew and kurtosis
#We divide the skew statistic by the standard error to get the standardised score
#This will tell us if we have a problem
tpskew<-semTools::skew(student_perform$pg3)
tpkurt<-semTools::kurtosis(student_perform$pg3)
tpskew[1]
tpskew[2]
tpskew[1]/tpskew[2]
tpkurt[1]/tpkurt[2]

#and by calculating the percentage of standardised scores for the variable itself that are outside our acceptable range
#this will tell us how big a problem we have
# Calculate the percentage of standardized scores that are greater than 1.96
# the perc function which is part of the FSA package which calculate the percentage that are within a range - you can look for greater than "gt", greater than or equal "geq", "gt", less than or equal "leq",  or less than "lt"),
# scale is a function that creates z scores
zpg3<- abs(scale(student_perform$pg3))

FSA::perc(as.numeric(zpg3), 1.96, "gt")
FSA::perc(as.numeric(zpg3), 3.29, "gt")


###  pg2 Generate Histogram -Grade 2 in Portuguese
#Create histogram
pgg2 <- ggplot(student_perform, aes(x=student_perform$pg2))
pgg2
pgg2 <- pgg2 + labs(x="Potugues Grade 2")
pgg2 <- pgg2 + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
pgg2 <- pgg2 + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
pgg2 <- pgg2 + stat_function(fun=dnorm, color="red",args=list(mean=mean(student_perform$pg2, na.rm=TRUE), sd=sd(student_perform$pg2, na.rm=TRUE)))
pgg2

###  pg2 Generate Q-Q Plot-Grade 2 in Portuguese

#Create a qqplot
qqnorm(student_perform$pg2)
qqline(student_perform$pg2, col=2) #show a line on the plot




### PG2 Generate Summary Statistics

#Generate regular summary statistics - lots of packages offer mechanisms to do this
pastecs::stat.desc(student_perform$pg2, basic=F)
#central tendency for pg2
round(mean(student_perform$pg2))
round(sd(student_perform$pg2))


#We can make our decision based on the standardised score for skew and kurtosis
#We divide the skew statistic by the standard error to get the standardised score
#This will tell us if we have a problem
tpskew<-semTools::skew(student_perform$pg2)
tpkurt<-semTools::kurtosis(student_perform$pg2)
tpskew[1]/tpskew[2]
tpkurt[1]/tpkurt[2]

#and by calculating the percentage of standardised scores for the variable itself that are outside our acceptable range
#this will tell us how big a problem we have
# Calculate the percentage of standardised scores that are greated than 1.96
# the perc function which is part of the FSA package which calculate the percentage that are within a range - you can look for greater than "gt", greater than or equal "geq", "gt", less than or equal "leq",  or less than "lt"),
# scale is a function that creates z scores
zpgg2<- abs(scale(student_perform$pg2))

FSA::perc(as.numeric(zpgg2), 1.96, "gt")
FSA::perc(as.numeric(zpgg2), 3.29, "gt")

##Report assess of Normality

##pg3 data was assessed for normality. Visual inspection of the histogram and QQ-Plot (see Figure 1 and Figure 2) identified some issues with skewness and kurtosis. The standardised score for kurtosis (`r round(tpkurt[1]/tpkurt[2],2)`) can be considered acceptable using the criteria proposed by West, Finch and Curran (1996), but the standardised score for skewness (`r round(tpskew[1]/tpskew[2],2)`)  was outside the acceptable range.  However over 99% of standardised scores for total optimism and associated sensation fall within the bounds of +/- 3.29, using the guidance of Field, Miles and Field (2013) the data can be considered to approximate a normal distribution (m=`r round(mean(student_performance$pg3, na.rm=TRUE),2)`, sd=`r round(sd(student_performance$pg3, na.rm=TRUE),2)`, n=`r

### CORRELATION
#### Scatterplot

#Simple scatterplot of final grade and Grade 2 for Portuguese
#aes(x,y)
library(ggplot2)
scatter <- ggplot(student_perform, aes(student_perform$pg2, student_perform$pg3))

#Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "pg2 grade 2 of Portuguese", y = "pg3 Final grade Portuguese ") 

#Pearson Correlation
stats::cor.test(student_perform$pg3, student_perform$pg2, method='pearson')

