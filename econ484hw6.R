library(ISLR)
library(e1071)

train_index <- sample(1:nrow(OJ), 800)
test_index <- setdiff(1:nrow(OJ), train_index)

# build x_train, y_train, x_test and y_test
train_data <- OJ[train_index, -18]
test_data <- OJ[test_index, -18]

# b
fit_svm = svm(Purchase~., data = OJ, subset = train_index, cost = 0.01, kernel = 'linear')
summary(fit_svm)
# We observe that 436 ibservations are used as support vectors.

# c
y_train = as.vector(train_data[,'Purchase'])
train_pre = predict(fit_svm, train_data)
table(y_train, train_pre)
# train error
mean(y_train != train_pre)

y_test = as.vector(test_data[,'Purchase'])
test_pre = predict(fit_svm, test_data)
table(y_test, test_pre)
# test error
mean(y_test != test_pre)

# d
tune_svm <- tune(svm, Purchase~., data = train_data, 
                 ranges = data.frame(cost = seq(0.01, 10, 25)), kernel = 'linear')
summary(tune_svm)
# Error estimation of ‘svm’ using 10-fold cross validation: 0.17625

# e
svm.pred = predict(tune_svm$best.model, OJ[train_index,])
table(OJ[train_index,'Purchase'], svm.pred)
# train error
mean(OJ$Purchase[train_index] != svm.pred)

svm.pred_test = predict(tune_svm$best.model, OJ[test_index,])
table(OJ[test_index,'Purchase'], svm.pred_test)
# test error
mean(OJ$Purchase[test_index] != svm.pred_test)

# f
fit_svm = svm(Purchase~., data = OJ, subset = train_index, cost = 0.01, kernel = 'radial')
summary(fit_svm)
# We observe that 632 ibservations are used as support vectoors.

y_train = as.vector(train_data[,'Purchase'])
train_pre = predict(fit_svm, train_data)
table(y_train, train_pre)
# train error
mean(y_train != train_pre)

y_test = as.vector(test_data[,'Purchase'])
test_pre = predict(fit_svm, test_data)
table(y_test, test_pre)
# test error
mean(y_test != test_pre)

tune_svm <- tune(svm, Purchase~., data = train_data, 
                 ranges = data.frame(cost = seq(0.01, 10, 25)))
summary(tune_svm)

svm.pred = predict(tune_svm$best.model, OJ[train_index,])
table(OJ[train_index,'Purchase'], svm.pred)
# train error
mean(OJ$Purchase[train_index] != svm.pred)

svm.pred_test = predict(tune_svm$best.model, OJ[test_index,])
table(OJ[test_index,'Purchase'], svm.pred_test)
# test error
mean(OJ$Purchase[test_index] != svm.pred_test)

# g
fit_svm = svm(Purchase~., data = OJ, subset = train_index, cost = 0.01, 
              kernel = 'polynomial', degree = 2)
summary(fit_svm)
# We observe that 634 ibservations are used as support vectoors.

y_train = as.vector(train_data[,'Purchase'])
train_pre = predict(fit_svm, train_data)
table(y_train, train_pre)
# train error
mean(y_train != train_pre)

y_test = as.vector(test_data[,'Purchase'])
test_pre = predict(fit_svm, test_data)
table(y_test, test_pre)
# test error
mean(y_test != test_pre)

tune_svm <- tune(svm, Purchase~., data = train_data, 
                 ranges = data.frame(cost = seq(0.01, 10, 25)))
summary(tune_svm)

svm.pred = predict(tune_svm$best.model, OJ[train_index,])
table(OJ[train_index,'Purchase'], svm.pred)
# train error
mean(OJ$Purchase[train_index] != svm.pred)

svm.pred_test = predict(tune_svm$best.model, OJ[test_index,])
table(OJ[test_index,'Purchase'], svm.pred_test)
# test error
mean(OJ$Purchase[test_index] != svm.pred_test)