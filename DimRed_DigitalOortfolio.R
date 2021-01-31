##Dimensionreduction using Student personality dataset

  ## Preliminaries
needed_packages <- c("psych",  "REdaS", "Hmisc", "corrplot", "ggcorrplot", "factoextra",  "nFactors")                      
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed, repos = "http://cran.us.r-project.org") 
library(psych)
library(REdaS)
library(Hmisc)
library(corrplot)
library(ggcorrplot)
library(factoextra)#Used for principal component analysis to get a different view of eigenvalues
library(nFactors)
library(dplyr)


#We are using a .dat file from Field, Miles and Field Discovering Statistics with R, a survey on fear of statistics - raq.dat
#load data
spd1<-read.csv("studentpIusepersonality.csv", header = TRUE, sep=",")


spd<-spd1[,seq(55,87)]


##Step 1: Screen the correlation matrix

#create a correlation matrix (these are just some methods)
spdMatrix<-cor(spd)
round(spdMatrix, 2)
Hmisc::rcorr(as.matrix(spd))

###Using ggcorrplot

#Using ggcorrplot. Note these are examples you need to choose a style for yourself, you do not need to create multiple correlation matrices
sp.mat <- ggcorrplot::cor_pmat(spd)
ggcorrplot::ggcorrplot(spdMatrix, title = "Correlation matrix for SPD data")
#Showing Xs for non-significant correlations
ggcorrplot::ggcorrplot(spdMatrix, title = "Correlation matrix for SPD data", p.mat = sp.mat, sig.level = .05)
#Showing lower diagonal
ggcorrplot::ggcorrplot(spdMatrix, title = "Correlation matrix for SPD data", p.mat = sp.mat, sig.level = .05, type="lower")


#Overlay plot with a white grid to space things out.
#t1.cex is the text size, pch is controlling what is shown for non-significant correlations
#ggcorrplot(spdMatrix, sig.level=0.05, lab_size = 4.5, p.mat = NULL,
          # insig = c("pch", "blank"), pch = 1, pch.col = "black", pch.cex =1,
          # tl.cex = 10) +
  #theme(axis.text.x = element_text(margin=margin(-2,0,0,0)),
       # axis.text.y = element_text(margin=margin(0,-2,0,0)),
       # panel.grid.minor = element_line(size=10)) + 
  #geom_tile(fill="white") +
  #geom_tile(height=0.8, width=0.8)


#Showing the co-coefficients (this will be messy given the number of variables)
ggcorrplot::ggcorrplot(spdMatrix, lab=TRUE, title = "Correlation matrix for SPD data",  type="lower")

###Using corrplot

#Visualization of correlations using circles
#corrplot parameters method = c("circle", "square", "ellipse", "number", "shade",
#"color", "pie")
#type = c("full", "lower", "upper"),
corrplot::corrplot(spdMatrix, method="circle")
corrplot::corrplot(spdMatrix, method="circle", type="upper")
#Visualization using numbers
corrplot::corrplot(spdMatrix, method="number")

#Visualization of significance levels at 0.05
spdres1 <- corrplot::cor.mtest(spdMatrix, conf.level = .95)
corrplot::corrplot(spdMatrix, p.mat = spdres1$p, type="lower", sig.level = .05)

#Showing p-value for non-significant results
#corrplot(spdMatrix, p.mat = spdres1$p, type="lower",insig = "p-value")


##Step 2: Check if data is suitable - look at the relevant Statistics
###Bartlett's test

psych::cortest.bartlett(spd)
psych::cortest.bartlett(spdMatrix, n=nrow(spd))

###KMO
#KMO (execute one of these):
REdaS::KMOS(spd)
psych::KMO(spd)



###Determinant

#Determinant (execute one of these):
det(spdMatrix)
det(cor(spd))


##Step 3: Do the Dimension Reduction  (PRINCIPAL COMPONENTS ANALYSIS)


#pcModel<-principal(dataframe/R-matrix, nfactors = number of factors, rotate = "method of rotation", scores = TRUE)

#On raw data using principal components analysis
#For PCA we know how many factors if is possible to find
#principal will work out our loadings of each variable onto each component, the proportion each component explained and the cumulative proportion of variance explained 
sppc1 <-  principal(spd, nfactors = 23, rotate = "none")
sppc1 <-  principal(spd, nfactors = length(spd), rotate = "none")
sppc1#output all details of the PCA


##Step 4: Decide which components to retain (PRINCIPAL COMPONENTS ANALYSIS)

#Create the scree plot
plot(sppc1$values, type = "b") 
#Print the variance explained by each component
sppc1$Vaccounted 
#Print the Eigenvalues
sppc1$values


#Another way to look at eigen values plus variance explained (need to use princomp function of PCA to get right class for use with factoextra functions)
sppcf=princomp(spd)
factoextra::get_eigenvalue(sppcf)
factoextra::fviz_eig(sppcf, addlabels = TRUE, ylim = c(0, 50))#Visualize the Eigenvalues
factoextra::fviz_pca_var(sppcf, col.var = "black")
factoextra::fviz_pca_var(sppcf, col.var = "cos2",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                         repel = TRUE # Avoid text overlapping
                        )

#Print the loadings above the level of 0.3
psych::print.psych(sppc1, cut = 0.3, sort = TRUE)
#create a diagram showing the components and how the manifest variables load
fa.diagram(sppc1) 
#Show the loadings of variables on to components
fa.sort(sppc1$loading)
#Output the communalities of variables across components (will be one for PCA since all the variance is used)
sppc1$communality 
#Visualize contribution of variables to each component
spvar <- factoextra::get_pca_var(sppcf)
corrplot::corrplot(spvar$contrib, is.corr=FALSE) 

# Contributions of variables to PC1
factoextra::fviz_contrib(sppcf, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
factoextra::fviz_contrib(sppcf, choice = "var", axes = 2, top = 10)


##Step 5: Apply rotation
#Apply rotation to try to refine the component structure
sppc2 <-  principal(spd, nfactors = 4, rotate = "varimax")#Extracting 4 factors
#output the components
psych::print.psych(sppc2, cut = 0.3, sort = TRUE)
#output the communalities
sppc2$communality
#NOTE: you can do all the other things done for the model created in pc1


##Step 3: Do the dimension reduction and Step 4: Decide which factors/components to retain (FACTOR ANALYSIS)

#Factor Analysis - the default here is principal axis factoring fm=pa
#If we know our data going in is normally distributed we use maximum likelihood
spfacsol <- psych::fa(spdMatrix, nfactors=4, obs=NA, n.iter=1, rotate="varimax", fm="pa")

#Create your scree plot
plot(spfacsol$values, type = "b") #scree plot

#Print the Variance accounted for by each factor/component
spfacsol$Vaccounted
#Output the Eigenvalues
spfacsol$values 

#Print the components with loadings
psych::print.psych(spfacsol,cut=0.3, sort=TRUE)

#Print sorted list of loadings
fa.sort(spfacsol$loading)

#create a diagram showing the factors and how the manifest variables load
fa.diagram(spfacsol)

#Note: you can apply rotation as you did for PCA

##Step 5: Apply rotation

#Apply rotation to try to refine the component structure
spfacsolrot <-  principal(spdMatrix, rotate = "varimax")
#output the components
psych::print.psych(spfacsolrot, cut = 0.3, sort = TRUE)
#output the communalities
spfacsolrot$communality


##Step 6: Reliability Analysis

#If you know that variables are grouped, test each group as a separate scale
a<-spd[,seq(1,10)]
b <- spd[, seq(11,23)]
c <- spd[, seq(24,33)]


#Output our Cronbach Alpha values
psych::alpha(a)
#If some items are to be reversed keyed, then either recode or get alpha to reverse code as needed by setting check.keys=TRUE (be careful with this - make sure you know it makes sense)
psych::alpha(a, check.keys=TRUE)

psych::alpha(b)
psych::alpha(c)
