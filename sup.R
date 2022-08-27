# Simple Linear Regression
# Importing the dataset
dataset = read.csv('salary.csv')

# Splitting the dataset into the
# Training set and Test set
install.packages('caTools')
library(caTools)
split = sample.split(dataset$Salary,
					SplitRatio = 0.7)
trainingset = subset(dataset, split == TRUE)
testset = subset(dataset, split == FALSE)

# Fitting Simple Linear Regression
# to the Training set
lm.r = lm(formula = Salary ~ YearsExperience,
						data = trainingset)
coef(lm.r)

# Predicting the Test set results
ypred = predict(lm.r, newdata = testset)

install.packages("ggplot2")
library(ggplot2)

# Visualising the Training set results
ggplot() + geom_point(aes(x = trainingset$YearsExperience,
						y = trainingset$Salary),
						colour = 'red') +
		geom_line(aes(x = trainingset$YearsExperience,
						y = predict(lm.r, newdata = trainingset)),
						colour = 'blue') +
		
ggtitle('Salary vs Experience (Training set)') +
xlab('Years of experience') +
ylab('Salary')

# Visualising the Test set results
ggplot() + geom_point(aes(x = testset$YearsExperience,
						y = testset$Salary),
						colour = 'red') +
		geom_line(aes(x = trainingset$YearsExperience,
						y = predict(lm.r, newdata = trainingset)),
						colour = 'blue') +

ggtitle('Salary vs Experience (Test set)') +
xlab('Years of experience') +
ylab('Salary')
