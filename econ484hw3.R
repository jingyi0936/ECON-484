library(class) # for KNN analysis 
library(nnet) # for multi-nom


URL <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/cmc/cmc.data'
cmc <- read.table(URL, sep = ',',  na.strings = '?')

colnames(cmc) <- c("wife_age", "wife_edu", "hus_edu", "num_child", "religiom", 
                   "wife_working", "hus_job", "liv_index", "media_exposure", "method")

summary(cmc$method)
# 1 = NO-use 2 = Long-term 3 = Short-term

## train the model using 80% of the whole sample
# random sample indices
train_index <- sample(1:nrow(cmc), 0.8 * nrow(cmc))
test_index <- setdiff(1:nrow(cmc), train_index)

# build x_train, y_train, x_test and y_test
x_train <- cmc[train_index, -10]
y_train <- cmc[train_index, 'method']
train_data <- cbind(x_train, y_train)
x_test <- cmc[test_index, -10]
y_test <- cmc[test_index, 'method']
test_data <- cbind(x_test, y_test)

k = round(sqrt(nrow(cmc)))

knn.pred = knn(train_data, test_data, y_train, k)
knn.pred[1:10]

table(knn.pred, test_data$y_test)
mean(knn.pred == test_data$y_test)

# multinom
fit = multinom(y_train~., data = train_data)
multi_pred = predict(fit, test_data, "class")
summary(fit)
mean(multi_pred == test_data$y_test)

