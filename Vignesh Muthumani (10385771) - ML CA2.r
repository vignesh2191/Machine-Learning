#----------------------------------------------------------------------------#
############################## Machine Learning ##############################
#----------------------------------------------------------------------------#
################ Assignment 2: Vignesh Muthumani (10385771) ##################
#----------------------------------------------------------------------------#

##############################################################################
################################# Section 1 ##################################
##############################################################################

# Qsn 1 of this assignment has already been worked out in CA1.

##############################################################################
################################# Section 2 ##################################
##############################################################################

# (a) -----------------------------------------------------------------------#

# ANOVA

# reading the dataset
q5 <- read.csv(file.choose(), header = T)
View(q5)

# assigning the null and alternate hypothesis
# H0: mu1 = mu2 = mu3
# H1: not H0

# performing ANOVA test
aov.set = aov(rock.dens ~ location, data = q5)
summary(aov.set)

print(model.tables(aov.set, "means"), digits = 3)
boxplot(rock.dens ~ location, data = q5)

# Interpretataion:
# From the output, at 0.05 level of significance, the F value is 0.978, 
# and p-value is 0.33 which is higher than alpha. In other words, the variation
# of rock density across the three locations (numerator) is much smaller 
# than the variation of rock density within each location, and our p-value is  
# greater than alpha = 0.05. Hence, we can conclude that for our confidence 
# interval we accept the null hypothesis H0 that there is no significant effect
# on the location for the rock density in Iran.

# (b) -----------------------------------------------------------------------#

# Principal Component Analysis (PCA)

# reading the data
q6 <- read.csv(file.choose(), header = T)
View(q6)

# exploring the data
dim(q6)
str(q6)
data <- q6[,2:13]

fit <- princomp(data, cor=TRUE)
summary(fit) # components 1-4 involve 80% of data variation

loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
# we can see from the above plot that the important components 1-4 fall above
# the variance level of 1.

biplot(fit)

# Interpretation:
# The important principal components involving at least 80% of data variation
# are Comp 1, Comp 2, Comp 3, and Comp 4. 
# Decision strategy: Components above variance = 1 display the important 
# principal components as they contribute to 80% of the data variation, while
# the remaining components contribute to only 20%.

##############################################################################