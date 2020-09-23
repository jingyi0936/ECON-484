library(ISLR)
library(tree)

## Problem 9
attach(OJ)

### a
# train the model using 80% of the whole sample
# random sample indices
train_index <- sample(1:nrow(OJ), 800)
test_index <- setdiff(1:nrow(OJ), train_index)

# build x_train, y_train, x_test and y_test
x_train <- OJ[train_index, -18]
y_train <- OJ[train_index, 'Purchase']
train_data <- OJ[train_index,]
x_test <- OJ[test_index, -18]
y_test <- OJ[test_index, 'Purchase']
test_data <- OJ[test_index,]

### b
reg = tree(Purchase~., data = train_data)
summary(reg)
# The tree has 8 terminal nodes and a training error of 0.18

### c
reg
# I will pick node labelled 19. The split is determined by if the 
# PriceDiff is less than than 0.165. The number of observations
# in that branch is 133 with the deviance of 167.40.
# Around 32.3% takes the value CH and 67.7% takes value of MM. 

### d
plot(reg)
text(reg, pretty = 0)
# We can observe that LoyalCh is an important indicator, since first node
# whether LoyalCH is less than 0.705699 differentiates the data into two 
# parts.

### e
pred.tree <- predict(reg, test_data, type = "class")
table(pred.tree, y_test)
test_err = 1- ((145+78)/270)
# test error rate is around 17.4%

### f
cv_reg <- cv.tree(reg, FUN = prune.misclass)

### g
plot(cv_reg$size, cv_reg$dev, type = "b", xlab = "Tree size", ylab = "Deviance")

### h
# We can observe that when tree size is equal to 4, the error is the smallest.

### i
prune_reg <- prune.tree(reg, best = 2)
plot(prune_reg)
text(prune_reg, pretty = 0)

### j
summary(prune_reg)
summary(reg)
# misclassification error is the same

### k
pred_prune <- predict(prune_reg, test_data, type = "class")
table(pred_prune, y_test)
prune_test_err = 1-((145+78)/270)
# test_error is the same