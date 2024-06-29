# first we convert data values into r

data <- data.frame(
  Years_Exp = c(1.1, 1.3, 1.5, 2.0, 2.2, 2.9, 3.0, 3.2, 3.2, 3.7),
  salary = c(39343.00, 46205.00, 37731.00, 43525.00,
             39891.00, 56642.00, 60150.00, 54445.00, 64445.00, 57189.00)
)

# scatter plot of the given dataset

plot(data$Years_Exp, data$salary,
     xlab = "year experienced",
     ylab = "salary",
     main = "scatter plot" )

#implement simple linear regression

install.packages('caTools')
library(caTools)
split = sample.split(data$salary, SplitRatio = 0.7)
trainingset = subset(data, split = TRUE)
testset = subset(data, split = FALSE)

# fitting simple linear regression to the training set

lm.r = lm(formula = salary~Years_Exp, data = trainingset)
summary(lm.r)

# predict values using predict function

new_data <- data.frame(Years_Exp = c(4.0, 4.5, 5.0))
predicted_salaries <- predict(lm.r, newdata = new_data)
print(predicted_salaries)

# visulizing the training set results

library(ggplot2)

ggplot(trainingset, aes(x = Years_Exp, y = salary)) +  # Define aesthetics in ggplot
  geom_point(colour = 'red') +  # Point layer with color
  geom_line(aes(x = Years_Exp, y = predict(lm.r, newdata = trainingset)), colour = 'blue') +  # Line layer with predicted values
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')

# visualizing the testing set results
library(ggplot2)
ggplot(testset, aes(x=Years_Exp, y= salary))+
  geom_point(color = 'red') +
  geom_line(aes(x = Years_Exp, y= predict(lm.r, newdata = trainingset)),color = 'blue')+
  ggtitle('salary vs experience (test set)')+
  xlab('years of exp')+
  ylab('salary')







