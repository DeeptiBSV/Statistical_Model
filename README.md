This project is to evaluate Student Performance using various statiscal Model.
Detailed description of each model is available in below website:
https://sites.google.com/view/deeptibs/home
# Statistical_Model
Student Performance Dataset-Linear, Logistic and Dimension Reduction
Dataset Student-Performance dataset- Description
This data approach student achievement in secondary education of two Portuguese schools. The data attributes include student grades, demographic, social and school related features) and it was collected by using school reports and questionnaires. Two datasets are provided regarding the performance in two distinct subjects: Mathematics (mat) and Portuguese language (por). In [Cortez and Silva, 2008], the two datasets were modeled under binary/five-level classification and regression tasks. Important note: the target attribute G3 has a strong correlation with attributes G2 and G1. This occurs because G3 is the final year grade (issued at the 3rd period), while G1 and G2 correspond to the 1st and 2nd period grades. It is more difficult to predict G3 without G2 and G1, but such prediction is much more useful .

Size of the sample n=382

For continuous variables like pg3 (final grade) and pg2 will look at mean and median of the sample for representativeness of population

For categorical/Nominal variable like gender, higher.p, romantic.p, we will look for median and IQR of the sample for representativeness of population

For ranking data variable like goout.p(going out with friends), fedu (father's education) we will look for median and IQR

concepts of interest from the dataset

How data is distributed for continuous variable

Correlation between variables

Differences between two groups

Differences involving more than two group

Multiple Linear Regression

Binomial Logistic Regression

Variables which will analyzed  for statistics

pg3-Ratio or Interval data

pg2-Ratio or Interval data

internet-categorical/nominal data

goout-Ordinal data

gender-nominal

romantic.p- nominal

fedu- Ordinal/Ranking data

higher.p- nominal

Research questions for the variables from student-Performance dataset:
Research Question 1

Is there s relationship between final grade achieved in Portuguese and grade 2 in Portuguese

Research question 2

Is there a significant difference in mean pg3 scores for students who use internet and students who do not used internet 

Research question 3

Does going out impacts the grade 2 of the Portuguese students

Research question 4

Is there a difference between boys and girls in terms of whether they have been in romantic relationship?

Research Question 5

Is Multiple Linear regression model is significant predictor of final grade of Portuguese students.

Research Question 6

Is there is a significant difference in pursuing higher education between male and female students  of Portuguese and father's education qualifications.

Research Question 7 (seperate dataset)

Can we apply dimensionality reduction for the variables of interest using FA/PCA analysis.

Student Performance Dataset- sample dataset
Research Question - 1: To find the linear correlation between two variables

"Is there a relationship between Final Grade achieved in Portuguese and Grade 2 in Portuguese

Concept: Final overall grade achieved in Portuguese

Variable Name-pg3

Variable type - scale

Dependent/Outcome variable

Concept: Grade 2 achieved in Portuguese

Variable Name-pg2

Variable type - scale

Independent

Research Question - 2: Differences two groups 2-tailed tests

Is there a significant difference in mean pg3 scores for students who use internet and students who do not use internet 

Concept: Final overall grade achieved in Portuguese

Variable Name-pg3

Variable type - scale

Dependent/Outcome variable

Concept: internet 

Variable Name-internet

Variable type - categorical

Independent

Research Question - 3:Differences more than 2 groups (parametric data)

Does going out impacts the grade 2of the Portuguese students

Concept: Final overall grade achieved in Portuguese

Variable Name-pg2

Variable type - scale

Dependent/Outcome variable

Concept: internet 

Variable Name-goout

Variable type - Nominal

Independent

Research Question - 4: Differences Nominal Variables

"Is there a difference between boys and girls in terms of whether they have been in romantic relationship?"

Concept: Romantic relationship

Variable Name-romantic.p

Variable type - Nominal

Dependent

Concept: gender differences

Variable Name-sex

Variable type - Nominal

Independent

Research Question - 5: Multiple Linear Regression

"Is Multiple Linear regression model is significant predictor of final grade of Portuguese students?"

Concept: Final Grade of Portuguese students

Variable Name-pg3

Variable type - continuous

Dependent

Concept: Grade 2 of Portuguese students

Variable Name-pg2

Variable type - continuous

predictor 1

Concept: Gender

Variable Name-sex

Variable type - Nominal

predictor 2-dummy variable(for differential effect)

Concept: Interaction term

Attained by multiplying pg2 and sex

Research Question - 6: Binomial Logistic Regression

"Is there is a significant difference in pursuing higher education between male and female students  of Portuguese and father's education qualifications.

Concept: Interest for Higher Education by Portuguese students

Variable Name-higher.p

Variable type - Nominal

Dependent

Concept: Gender

Variable Name-sex

Variable type - Nominal

predictor 1(differential effect)

Concept: Father's Education

Variable Name-fedu

Variable type - Ordinal

predictor 2

Research Question - 7: Dimension Reduction(Other dataset Student Personality Analysis dataset)

Can we use dimension reduction for the variables of interest using FA/PCA

Concept: A1 to C10 (30 variables) are considered
