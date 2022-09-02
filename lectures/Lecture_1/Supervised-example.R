# Supervised learning example
# Source: data and (adapted) code from tinyurl.com/examplewcode 

# install.packages('mlbench')
library(mlbench); library(caret)
 
Context.explanation = paste("\n \n\n\n\n We will use a dataset on breast cancer, built-in into the package caret\n")
Data.explanation=  paste("\n The dataset has several variables describing the characteristics of the cell, such as cell size, mitoses. It also includes the dependent variable of interest: benign or malignant, indicating the type of cancer.") 
RQ.explanation= paste("\n Our research question is simple: We want to predict the type of tumor: benign or malignant, using the cell characteristics.\n")  

cat(Context.explanation);cat(Data.explanation);cat(RQ.explanation)


# data
data(BreastCancer, package="mlbench")
bc <- BreastCancer[complete.cases(BreastCancer), ]  # keep complete rows
View(bc)

# data clean up, prep
bc <- bc[,-1] #remove id column
for(i in 1:9) { bc[, i] <- as.numeric(as.character(bc[, i]))} # convert to numeric
# Change Y values from textual categories to 1's and 0's
bc$Class <- ifelse(bc$Class == "malignant", 1, 0)
bc$Class <- factor(bc$Class, levels = c(0, 1))

# Prep Training and Test data.
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.
set.seed(100)
trainDataIndex <- createDataPartition(bc$Class, p=0.7, list = F)
trainData <- bc[trainDataIndex, ]
testData <- bc[-trainDataIndex, ]

# Class distribution of train data
table(trainData$Class)

# Note that the two classes have a large difference in the number of cases (311 versus 168). 
# The author of this example dataset and code suggest using "down sample" or "up sample" to 
# have the same number of cases. 
# However, this can be a problematic approach. There are other approaches in the literature.
# Furthermore, over-representation and under-representation of a group in a dataset is an
# active area of research within the "fairness in machine learning (FML)" field, where the impact of
# underrepresented minorities in training is a source of concern. For more and better solutions I recommend
# exploring that FML literature and methods. This could also be a nice research question for your final assignment in module 1.
# For now, I am keeping the original author's proposed method below (i.e., down sample) but please be aware of the statistical 
# and fairness issues mentioned above.
set.seed(100)
down_train <- downSample(x = trainData[, colnames(trainData) %ni% "Class"],
                         y = trainData$Class)
table(down_train$Class)


# Build Logistic Model
logitmod <- glm(Class ~ Cl.thickness + Cell.size + Cell.shape, family = "binomial", data=down_train)
summary(logitmod)

# now we finally are able to make the prediction
pred <- predict(logitmod, newdata = testData, type = "response")
pred

# The prediction is continuous. We now discretize into a binary variable (benign or malignant)
# What is the pros and cons of doing this
#      - from a machine learning development point of view? 
#      - from a patient/clinician point of view?
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Class

# Accuracy
mean(y_pred == y_act)  # 94%

