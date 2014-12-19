rm(list = ls())

# source("./FinalProject/stepwiseAIC.R")
# source("./FinalProject/stepwiseBIC.R")
# source("./FinalProject/lassoCV.R")
# source("./FinalProject/ridgeCV.R")
# source("./FinalProject/RF.R")
# source("./FinalProject/boostTree.R")
# source("./FinalProject/MARS.R")

data1 <- read.table(file = "./FinalProject/data/data1")
data2 <- read.table(file = "./FinalProject/data/data2")
data3 <- read.table(file = "./FinalProject/data/data3")

pdata1 <- read.table(file = "./FinalProject/data/pdata1")
pdata2 <- read.table(file = "./FinalProject/data/pdata2")

data <- data.frame(data1, data2, data3)
pdata <- data.frame(pdata1, pdata2)
## X1 - X30 are predictors, where X1 is {0, 1} and others are continuous
## Y1 - Y2 are responses, where Y1 is continous and Y2 is {0, 1}
##data <- data[, 1:31]
data$X10 <- log(data$X10 + 0.01)
data$X15 <- log(data$X15 + 0.01)
# # 
pdata$X10 <- log(pdata$X10 + 0.01)
pdata$X15 <- log(pdata$X15 + 0.01)


require(doMC)
registerDoMC(2)
require(caret)

set.seed(9876)

fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10, 
    repeats = 1
)

fitControl_tree <- trainControl(## OOB
    method = "oob"
)

fitControl0 <- trainControl(## 10-fold CV
    method = "none",
)


num_iter = 20
result <- matrix(nrow = 20, ncol = num_iter)


for (jj in 1:num_iter)
{
    trainIndex <- createDataPartition(as.factor(data$Y2), p = .6,
                                      list = FALSE,
                                      times = 1)
    data_train <- data[trainIndex, ]
    data_test <- data[-trainIndex, ]
    
    ## regression part
    X_train <- data_train[, c(1:30)]
    y_train <- as.factor(data_train[, 32])
    y_train <- factor(y_train, levels=rev(levels(y_train)))
    X_test <- data_test[, c(1:30)]
    y_test <- as.factor(data_test[, 32])
    y_test <- factor(y_test, levels=rev(levels(y_test)))
    stacking <- matrix(nrow = nrow(X_train), ncol = 20)
    stacking_test <- matrix(nrow = nrow(X_test), ncol = 20)
    i = 1
    
    
    ## simple linear model, glm,
    m <- train(y = y_train, x = X_train,
               trControl = fitControl0,
               method = "glm", family = binomial)
    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[i, jj] <- foo$overall[1]
    
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## stepwise linear model AIC, 2.089027
    m <- train(y = y_train, x = X_train,
               trControl = fitControl0,
               method = "glmStepAIC", trace = FALSE, family = binomial)

    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[i, jj] <- foo$overall[1]
    
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## stepwise linear model BIC, best
    m <- train(y = y_train, x = X_train,
               trControl = fitControl0,
               method = "glmStepAIC", trace = FALSE, 
               k = log(nrow(X_train)), family = binomial)

    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[i, jj] <- foo$overall[1]
    
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## lasso (glmnet) 2.046684
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "glmnet", 
               tuneGrid = expand.grid(alpha = 1, 
                                      lambda = seq(0.001, 0.2, 0.002)), 
               family = "binomial")

    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[i, jj] <- foo$overall[1]
    
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## ridge (glmnet) 2.206863
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "glmnet", 
               tuneGrid = expand.grid(alpha = 0, 
                                      lambda = seq(0.01, 0.2, 0.001)), 
               family = "binomial")

    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[i, jj] <- foo$overall[1]
    
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## elastic net (glmnet) 2.046684
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "glmnet", 
               tuneGrid = expand.grid(alpha = seq(0, 0.2, 0.05), 
                                      lambda = seq(0.01, 0.3, 0.01)), 
               family = "binomial")
    
    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[i, jj] <- foo$overall[1]
    
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    # mars, 1.238396
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "gcvEarth", 
               tuneGrid = expand.grid(degree = 1), 
               glm = list(family = binomial))

    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[i, jj] <- foo$overall[1]
    
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## random forest, 3.860662
    m <- train(y = y_train, x = X_train,
               trControl = fitControl_tree,
               method = "rf", 
               tuneGrid = expand.grid(mtry = seq(1, 8, 1)))

    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[i, jj] <- foo$overall[1]
    
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## gradient boosting tree, 2.710811
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "gbm", 
               tuneGrid = expand.grid(n.trees = seq(100, 400, 50), 
                                      shrinkage = 0.05, 
                                      interaction.depth = 1:4), 
               distribution = "adaboost", 
               n.minobsinnode = 5)
    
    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[i, jj] <- foo$overall[1]
    
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## Gaussian Process with linear kernal, 2.113074
    m <- train(y = y_train, x = X_train,
               trControl = fitControl0,
               method = "gaussprLinear")

    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[i, jj] <- foo$overall[1]
    
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## Gaussian Process with Polynomial Kernel, 1.943231
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "gaussprPoly")
    
    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[i, jj] <- foo$overall[1]
    
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## knn, 7.602605
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "knn",
               tuneGrid = expand.grid(k = seq(25, 39, 2)))
    
    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[i, jj] <- foo$overall[1]
    
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## Support Vector Machines with Linear Kernel, 2.115428
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "svmLinear", 
               tuneGrid = expand.grid(C = seq(0.0001, 0.01, 0.0005)))
    
    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[i, jj] <- foo$overall[1]
    
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## Support Vector Machines with Polynomial Kernel, 1.741593
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "svmPoly", 
               tuneGrid = expand.grid(degree = c(2), 
                                      scale = c(0.001, 0.01, 0.1, 1), 
                                      C = seq(0.001, 0.1, 0.005)))
    
    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[i, jj] <- foo$overall[1]
    
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## Support Vector Machines with Radial Basis Function Kernel
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "svmRadialCost",
               tuneGrid = expand.grid(C = seq(0.1, 2, 0.1)))
    
    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[i, jj] <- foo$overall[1]
    
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## C5.0
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "C5.0", 
               tuneGrid = expand.grid(trials = seq(35, 55, 2), 
                                      model = c("tree"), 
                                      winnow = FALSE))
    
    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[i, jj] <- foo$overall[1]
    
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## lda
    m <- train(y = y_train, x = X_train,
               trControl = fitControl0,
               method = "lda")
    
    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[i, jj] <- foo$overall[1]
    
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## qda
    m <- train(y = y_train, x = X_train,
               trControl = fitControl0,
               method = "qda")
    
    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[i, jj] <- foo$overall[1]
    
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    print(jj)
}
write.table(result, file = "./FinalProject/classResult1")
