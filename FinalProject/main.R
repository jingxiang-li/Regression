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
data$X4sq <- data$X4 ^ 2

pdata$X10 <- log(pdata$X10 + 0.01)
pdata$X15 <- log(pdata$X15 + 0.01)
pdata$X4sq <- pdata$X4 ^ 2


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


num_iter = 20
result <- matrix(nrow = 18, ncol = num_iter)


for (jj in 1:num_iter)
{
    trainIndex <- createDataPartition(data$Y1, p = .6,
                                      list = FALSE,
                                      times = 1)
    data_train <- data[trainIndex, ]
    data_test <- data[-trainIndex, ]
    
    ## regression part
    X_train <- data_train[, c(1:30, 33)]
    y_train <- data_train[, 31]
    X_test <- data_test[, c(1:30, 33)]
    y_test <- data_test[, 31]
    stacking <- matrix(nrow = nrow(X_train), ncol = 20)
    stacking_test <- matrix(nrow = nrow(X_test), ncol = 20)
    i = 1
    
    
    ## simple linear model, lm, 2.115021
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "lm")
    result[i, jj] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## stepwise linear model AIC, 2.089027
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "lmStepAIC", trace = FALSE)
    result[i, jj] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## stepwise linear model BIC, 2.030876
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "lmStepAIC", trace = FALSE, 
               k = log(nrow(X_train) * 9 / 10))
    result[i, jj] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## lasso (lars), 2.046749
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "lars2", 
               tuneGrid = expand.grid(step = 2:31))
    result[i, jj] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## lasso (glmnet) 2.046684
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "glmnet", 
               tuneGrid = expand.grid(alpha = 1, 
                                      lambda = seq(0.001, 0.2, 0.002)))
    result[i, jj] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## ridge (glmnet) 2.206863
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "glmnet", 
               tuneGrid = expand.grid(alpha = 0, 
                                      lambda = seq(0.01, 1, 0.01)))
    result[i, jj] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## elastic net (glmnet) 2.046684
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "glmnet", 
               tuneGrid = expand.grid(alpha = seq(0.7, 1, 0.05), 
                                      lambda = seq(0.001, 0.2, 0.002)))
    result[i, jj] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    # mars, best
    m <- train(y = y_train, x = X_train[, 1:30],
               trControl = fitControl,
               method = "gcvEarth", 
               tuneGrid = expand.grid(degree = 1:5))
    result[i, jj] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## random forest, 3.860662
    m <- train(y = y_train, x = X_train,
               trControl = fitControl_tree,
               method = "rf", 
               tuneGrid = expand.grid(mtry = seq(13, 23, 1)))
    result[i, jj] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## gradient boosting tree, 2.710811
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "gbm", 
               tuneGrid = expand.grid(n.trees = seq(500, 1000, 100), 
                                      shrinkage = 0.05, 
                                      interaction.depth = 1:4), 
               distribution = "gaussian", 
               n.minobsinnode = 5)
    result[i, jj] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## Gaussian Process with linear kernal, 2.113074
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "gaussprLinear")
    result[i, jj] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## Gaussian Process with Polynomial Kernel, 1.943231
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "gaussprPoly")
    result[i, jj] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## knn, 7.602605
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "knn",
               tuneGrid = expand.grid(k = seq(1, 20, 2)))
    result[i, jj] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## pls, 2.111247
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "pls", tuneLength = 25)
    result[i, jj] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## Projection Pursuit Regression, 2.055265
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "ppr", tuneLength = 3)
    result[i, jj] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## relevance vector machines with linear kernel, 2.115428
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "rvmLinear")
    result[i, jj] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    
    ## Relevance Vector Machines with Polynomial Kernel, 1.741593
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "rvmPoly")
    result[i, jj] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    stacking[,i] = predict(m)
    stacking_test[,i] = predict(m, newdata = X_test)
    i = i + 1
    print(jj)
}
## write.table(result, file = "./FinalProject/regResult1")

