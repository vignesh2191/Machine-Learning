#----------------------------------------------------------------------------#
############################## Machine Learning ##############################
#----------------------------------------------------------------------------#
################ Assignment 1: Vignesh Muthumani (10385771) ##################
#----------------------------------------------------------------------------#

##############################################################################
################################# Section 1 ##################################
##############################################################################

# (a) -----------------------------------------------------------------------#

# What is the predictive analytics? 

# Predictive analytics is the practice of extracting information from existing
# data sets in order to determine patterns and predict future outcomes and
# trends.  It forecasts what might happen in the future with an acceptable
# level of reliability, and includes what-if scenarios and risk assessment.
# Predictive analytics brings together advanced analytics capabilities
# spanning ad-hoc statistical analysis, predictive modeling, data mining, text
# analytics, optimization, real-time scoring and machine learning. These tools
# help organizations discover patterns in data and go beyond knowing what has
# happened to anticipating what is likely to happen next.
# The key stages for an analytical life cycle include:
# Data preparation - Source, clean and prepare the data for optimal results.
#	Data visualization and exploration - Explore all data to identify relevant variables,
# trends and relationships.
# ???Statistical analysis - Use everything from simple descriptive statistics to comple
# Bayesian analysis to quantify uncertainty, make inferences and drive decisions.
# ???Predictive modeling - Build the predictive model using statistical, data mining or text
# mining algorithms, including the critical capability of transforming and selecting key variables.
# ???Model deployment - Apply the new champion model, once validated and approved, to new data.
# ???Model management and monitoring - Examine model performance to make sure it is up-to-date
# and delivering valid results.

# (b) -----------------------------------------------------------------------#

# What is the main purpose of implementation of machine learning algorithms?

# Machine learning is an application of artificial intelligence (AI) that
# provides systems the ability to automatically learn and improve from
# experience without being explicitly programmed. The main purpose of implementing 
# Machine learning algorithms is to focus on the development of computer programs
# that can access data and use it to learn for themselves.
# Machine learning (ML) is a category of algorithm that allows software applications
# to become more accurate in predicting outcomes without being explicitly programmed.
# The basic premise of machine learning is to build algorithms that can receive input
# data and use statistical analysis to predict an output while updating outputs as new
# data becomes available.
# The processes involved in machine learning are like that of data mining and predictive modeling. 
# Purpose of Machine Learning Implementation:
# At its most basic, machine learning uses programmed algorithms that receive and analyse
# input data to predict output values within an acceptable range. As new data is fed to these
# algorithms, they learn and optimise their operations to improve performance, developing
# 'intelligence' over time.
# Steps to implement a machine learning algorithm, as follows:
# Performance: Open source implementations may be too general and not efficient enough for specific
# use cases.
# ???Correctness: There may be bugs or limitations in the open source implementations for specific
# use cases (such as larger scale datasets).
# ???Programming Language: Implementations may be limited to specific programming languages.
# ???Integration: There may be a need to integrate an algorithm implementation into the infrastructure
# of existing production systems.
# ???Licensing: There may be limitations imposed by the choice of open source license.

# (c) -----------------------------------------------------------------------#

# List the main criteria to select an appropriate machine learning algorithm
# before processing data.

# The selection of an appropriate machine learning algorithm depends on
# 1. The nature of the input variable (if it is continuous or discrete)
# 2. The presence of a target/output variable (supervised or unsupervised)
# 3. The nature of the target variable, if it is continuous or discrete. (classification or regression)
# 4. The number of classes in the discrete output variable. etc.

# (d) -----------------------------------------------------------------------#

# Express the main differences between supervised versus unsupervised learning
# algorithms. 

# The main difference between the two types is that supervised learning is done
# using a ground truth, or in other words, we have prior knowledge of what the
# output values for our samples should be. Therefore, the goal of supervised
# learning is to learn a function that, given a sample of data and desired outputs,
# best approximates the relationship between input and output observable in the
# data. Unsupervised learning, on the other hand, does not have labeled outputs,
# so its goal is to infer the natural structure present within a set of data points.
# Supervised: The main difference between supervised and unsupervised learning algorithm 
# is in the data used. In supervised learning, the input data (x) is used to predict the 
# target variable (y) that is well known and labeled using techniques like classification and regression. 
# Unsupervised: However, the data used in unsupervised 
# learning is not labeled, therefore, hidden patterns need to be uncovered. i.e. no output data to
# predict, which is to find patterns in data based on the relationship between data points using 
# techniques like clustering and association.

# (e) -----------------------------------------------------------------------#

# What are the main steps to design an optimal logistic classifier? 

# Logistic regression is a part of generalized linear model's family, which is binary
# classification algorithm where the response variable is 0 or 1. The assumptions of 
# logistic regression are:
# Response variable should follow binomial distribution
# There should be a linear relationship between independent variables and link (logit) function.
# Response variable should have mutually exclusive and exhaustive categories (Saraswat, n.d.).
# Steps that can be taken to design an optimal logistic classifier are as follows -
# Load the dataset and make a basic understanding of the independent and dependent variables.
# There are few points that must be considered while understanding the data -
# GLM does not assume linear relationship between dependent and independent variables, 
# whereas it does assume that between link function and independent variables.
# Dependent variable need not be normally distributed.
# It used maximum likelihood estimation for parameter estimation.
# Errors should not be normally distributed and should be independent.
# Check for missing values in the dataset and remove them and split the dataset into train and test data.
# Run the linear regression model on the training data to know the number of significant variables.
# If any independent variable is not significant, run the model again without insignificant variables.
# Now predict the response variables of test data set, using the model created on training data.
# Set the threshold probability for each class in the response variable. Since the prediction 
# will give us a value between 0 and 1 and the class is binary, while using R, the value of prediction
# is set to be 1 if the prediction is greater than or equal to 0.5, otherwise 0 (0 and 1 being the two classes).

# (f) -----------------------------------------------------------------------#

# What are the main steps to design an optimal SVM classifier? 

# Some of the main steps to design an optimal SVM classifier are:
# Analyzing the correlation of the data (independent variables shouldn't be 
# highly correlated with each other)
# Feature selection (selection of significant features is a key as it help 
# with the efficiency and accuracy of the model)
# Identifying the type of kernel to be used which could give better results
# there are different type of kernels like linear kernel, polynomial kernel,
# sigmoid kernel and radial kernel.
# Model estimation
# Evaluation of the validity of the model

##############################################################################
################################# Section 2 ##################################
##############################################################################

# (a) -----------------------------------------------------------------------#

q2 <- read.csv(file.choose(), header = T)
View(q2)

library(class) 
library(e1071)
library(caret)

set.seed(108) # for consistency

# splitting the dataset for train and test set
q2[,'train'] <- ifelse(runif(nrow(q2)) < 0.7, 1, 0)

# separating the train and test sets
train <- q2[q2$train == 1, -6]
test <- q2[q2$train == 0, -6]

# dimension of train and test set
dim(train)
dim(test)

# fitting the model
model <- naiveBayes(wantsMore ~ ., data = train)
model
summary(model)

# (b) -----------------------------------------------------------------------#

# prediction of test set
pred <- predict(model, test)

# (c) -----------------------------------------------------------------------#

# confusion matrix
cm <- table(predicted = pred, actual = test$wantsMore)  # confusion matrix 
cm

# accuracy of prediction
accuracy <- mean(pred == test[,3])
accuracy # 66% accuracy

# (d) -----------------------------------------------------------------------#

# classifier
classifier <- naiveBayes(q2[,c(1,2,4,5,6)], q2[,3])  # prior and posterior probability
classifier

##############################################################################
################################# Section 3 ##################################
##############################################################################

# (a) -----------------------------------------------------------------------#

q3 <- warpbreaks
View(q3)
str(q3)

set.seed(1) # for consistency

# splitting the dataset for train and test set
q3[,'train'] <- ifelse(runif(nrow(q3)) < 0.8, 1, 0)

# separating the train and test sets
train <- q3[q3$train == 1, -4]
test <- q3[q3$train == 0, -4]

# dimension of train and test set
dim(train)
dim(test)

# fitting the model
model <- svm(wool ~ ., data=train)

# predicting test set with the trained model
pred <- predict(model, test[,-2])

# confusion matrix to see the performance
cm <- confusionMatrix(pred, test$wool)
cm

# a.met
vect = rep(0,4)
for(i in 1:1000) {
  
  #split into training and test sets
  n=nrow(q3)
  indexes = sample(n,n*(80/100))
  trainset = q3[indexes,]
  testset = q3[-indexes,]

### 

#build model linear kernel and C-classification (soft margin) with default cost (C=1)

svm_lin <- svm(wool~ ., data=trainset, method='C-classification', kernel='linear')
svm_pol <- svm(wool~ ., data=trainset, method='C-classification', kernel='polynomial')
svm_sig <- svm(wool~ ., data=trainset, method='C-classification', kernel='sigmoid')
svm_rad <- svm(wool~ ., data=trainset, method='C-classification', kernel='radial')

#test set predictions
pred_test <-predict(svm_lin,testset)
confusion_matrixlin= table(pred = pred_test, true = testset$wool)
p_lin= mean(pred_test==testset$wool)

#test set predictions
pred_test <-predict(svm_pol,testset)
confusion_matrix= table(pred = pred_test, true = testset$wool)
p_pol= mean(pred_test==testset$wool)

#test set predictions
pred_test <-predict(svm_sig,testset)
confusion_matrix= table(pred = pred_test, true = testset$wool)
p_sig=mean(pred_test==testset$wool)

#test set predictions
pred_test <-predict(svm_rad,testset)
confusion_matrixrad= table(pred = pred_test, true = testset$wool)
p_rad=mean(pred_test==testset$wool)

###
p_vect=c(p_lin, p_pol, p_sig, p_rad)

vect=vect+(1/1000)*p_vect
}
vect

# linear is better

##############################################################################
################################# Section 4 ##################################
##############################################################################

# (a) -----------------------------------------------------------------------#

q4 <- read.csv(file.choose(), header = T)
View(q4)
str(q4)

# loading all the required packages for performing time series analysis
library(timeSeries)
library(timeDate)
library(tseries)
library(forecast)

# checking the class of the dataset
class(q4)

# since it's a data frame, it needs to be converted into a time series data
sales <- ts(q4$sales.b, start = c(1929,1), end = c(2013,1), frequency = 1)
sales # checking the output

# checking the class to see is it has successfully changed
class(sales)
str(sales)

# validating the assumptions for time series analysis
# the mean and variance has to be stationary
par(mfrow=c(1,1)) # resetting the graph layout

plot(sales)
abline(lm(sales~time(sales)), col="blue") # checking if it's stationary

# checking if it's stationary - dft
acf(sales, lag.max = 50)
pacf(sales, lag.max = 50)
adf.test(sales, alternative = 'stationary') # dickey-fuller test for stationary
# since the p-value is greater alpha=0.05 (significance level), we accept the
# null hypothesis that it is not stationary

# Now there are two approaches to do this. One is manual model and two is auto model

# for [MANUAL MODEL]:

# To validate our assumptions, we make the mean and variance stationary
# so we achieve it by applying differentiation and log on it

SALES <- diff(diff(log(sales)))
plot(SALES)
abline(lm(SALES~time(SALES)), col="blue")

# now we can see that both the mean and variance are stationary

# we perform a test of stationary using dickey-fuller test
adf.test(SALES, alternative = 'stationary')
# we can see that the p-value is less than significance level of alpha = 0.05,
# rejecting the null hypothesis and confirming that it is stationary 

acf(SALES, lag.max = 20) # q=2
pacf(SALES, lag.max = 20) # p=3

# hence our assumptions are validated

# (b) -----------------------------------------------------------------------#

# fitting the optimized model for sales

# manual model
man.fit <- arima(SALES, c(3,2,2), seasonal=list(order=c(0,0,0)))
summary(man.fit) 

# auto model
auto.fit <- auto.arima(sales, seasonal = F) # as our data is not seasonal
summary(auto.fit)

# it is always advisable to consider the auto model over manual model as it 
# tends to perform better

# the coefficient estimates of the models are
coef(man.fit)
coef(auto.fit)

# (c) -----------------------------------------------------------------------#

# estimated order for AR and MA is ARIMA(3,2,2) [auto model]
# where, AR   = p = 3 (ar1, ar2, and ar3)
#        Diff = d = 2 (trend requires 2 differentitations to become stationary) 
#        MA   = q = 2 (ma1 and ma2)
# with seasonal set to false as our data is not seasonal

# (d) -----------------------------------------------------------------------#

# forecasting 10 steps ahead on the plot of the original time series for the
# above fitted models

par(mfrow=c(1,1)) # resetting the graph layout

man.forecast <- forecast(man.fit, h=10)
man.forecast # predictions
plot(man.forecast) #forecast of manual model (bad)

auto.forecast <- forecast(auto.fit, h=10)
auto.forecast # predictions
plot(auto.forecast) #forecast of auto model (good)

# hence proved that the estimated order of auto model performs better.

##############################################################################