
#READ the file in to R and view the dataset

student_performance<-read.table("sperformance-dataset.csv", header = TRUE, sep = ',')

#changing the rownames to lowercase to read the columns in much convineNIENT way
colnames(student_performance) <- tolower(colnames(student_performance))

#Remember to install these packages if you haven't already done so

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
gg <- ggplot(student_performance, aes(x=student_performance$pg3))
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
# Calculate the percentage of standardised scores that are greated than 1.96
# the perc function which is part of the FSA package which calculate the percentage that are within a range - you can look for greater than "gt", greater than or equal "geq", "gt", less than or equal "leq",  or less than "lt"),
# scale is a function that creates z scores
zpg3<- abs(scale(student_performance$pg3))

FSA::perc(as.numeric(zpg3), 1.96, "gt")
FSA::perc(as.numeric(zpg3), 3.29, "gt")


###we have conducted the normality test now will go independent t-test
### Differences - Parametric Tests
#### Independent t-test
##### Describe pg3 by group (yes v no - internet)
#Get descriptive stastitics by group - output as a matrix
psych::describeBy(student_performance$pg3, student_performance$internet, mat=TRUE)

#Conduct Levene's test for homogeneity of variance in library car - the null hypothesis is that variances in groups are equal so to assume homogeneity we woudl expect probaility to not be statistically significant.
car::leveneTest(pg3 ~ internet, data=student_performance)
#Pr(>F) is  probability - in this case it is not statistically significant so we cannot assume homogeneity


#Conduct the t-test from package stats
#In this case we can use the var.equal = TRUE option to specify equal variances and a pooled variance estimate
stats::t.test(pg3~internet,var.equal=TRUE,data=student_performance)
#No statistically significant difference was found

res <- stats::t.test(pg3~internet,var.equal=TRUE,data=student_performance)


#Eta squared calculation
effes=round((res$statistic*res$statistic)/((res$statistic*res$statistic)+(res$parameter)),3)
effes



### Reporting the results with eta squared effect
##An independent-samples t-test was conducted to compare the mean scores of pg3 for students  
#who use internet and those who do not. 
#No significant difference in the scores for pg3 was found 
#(M=13, SD= 2.84 for students who use internet , M= 12, SD= 3.41 for students who do not use internet), (t(382)= -1.74, p = 0.08). A very small effect size was also indicated by the eta squared value (0.008). 


