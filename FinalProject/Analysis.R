rm(list = ls())


# read data ---------------------------------------------------------------


load("./FinalProject/data.Rda")
load("./FinalProject/pdata.Rda")

picFile <- c("./FinalProject/pic/")
texFile <- c("./FinalProject/tex/")


# preprocessing -----------------------------------------------------------


## distribution of two responses, Histogram
cairo_pdf(filename = paste(picFile, "fig1_histY.pdf", sep = ""), 
          width = 8, height = 4, pointsize = 10)
    par(mfrow = c(1, 2))
    hist(data$Y1, main = "Histogram of Y1", xlab = "Y1")
    hist(data$Y2, main = "Histogram of Y2", xlab = "Y1")
dev.off()

## BoxPlot of predictors
cairo_pdf(filename = paste(picFile, "fig2_boxplot.pdf", sep = ""), 
          width = 8, height = 6, pointsize = 10)
    boxplot(data[, 1:30], main = "Boxplot of X's")
dev.off()

## Distribution of X10 and X15
cairo_pdf(filename = paste(picFile, "fig3_histX10X15.pdf", sep = ""), 
          width = 8, height = 4, pointsize = 10)
    par(mfrow = c(1, 2))
    hist(data$X10, main = "Histogram of X10", xlab = "X10")
    hist(data$X15, main = "Histogram of X15", xlab = "X15")
dev.off()

## Tansform X10 and X15
data$X10 <- log(data$X10 + 0.01)
data$X15 <- log(data$X15 + 0.01)
pdata$X10 <- log(pdata$X10 + 0.01)
pdata$X15 <- log(pdata$X15 + 0.01)

cairo_pdf(filename = paste(picFile, "fig4_histX10X15_trans.pdf", sep = ""), 
          width = 8, height = 4, pointsize = 10)
    par(mfrow = c(1, 2))
    hist(data$X10, main = "Histogram of log(X10 + 0.01)", xlab = "log(X10 + 0.01)")
    hist(data$X15, main = "Histogram of log(X15 + 0.01)", xlab = "log(X15 + 0.01)")
dev.off()


# parametric regression part ----------------------------------------------


## ScatterPlot
cairo_pdf(filename = paste(picFile, "fig5_scatterPlot.pdf", sep = ""), 
          width = 30, height = 22, pointsize = 18)
    par(mfrow = c(5, 6))
    for (i in 1:30) plot(data[, i], data$Y1, 
                         xlab = colnames(data)[i], ylab = "Y1", 
                         main = paste("Y1 against", colnames(data)[i]))
dev.off()

## Include X4^2 into the regression model
data$X4sq <- data$X4^2
pdata$X4sq <- pdata$X4^2

## Base model, multivariate linear regression, reg0
require(car)
reg0 <- lm(Y1 ~ . - Y2, data = data)

cairo_pdf(filename = paste(picFile, "reg_reg0.pdf", sep = ""), 
          width = 8, height = 8, pointsize = 10)
    par(mfrow = c(2, 2))
    plot(reg0)
dev.off()

## Remove outlier, reg1
tmp <- outlierTest(reg0)
outlierIndex <- as.numeric(row.names(as.matrix(tmp$rstudent)))
reg1 <- lm(Y1 ~ . - Y2, data = data, subset = -outlierIndex)

cairo_pdf(filename = paste(picFile, "reg_reg1.pdf", sep = ""), 
          width = 8, height = 8, pointsize = 10)
    par(mfrow = c(2, 2))
    plot(reg1)
dev.off()

ncvTest(reg1)

## Remove redundant predictors by using AIC and BIC regAIC regBIC
regNull <- lm(Y1 ~ 1, data = data, subset = -outlierIndex)

regAIC <- step(reg1, scope = list(lower = regNull, upper = reg1), 
     direction = "both", trace = FALSE)
ncvTest(regAIC)
cairo_pdf(filename = paste(picFile, "reg_regAIC.pdf", sep = ""), 
          width = 8, height = 8, pointsize = 10)
    par(mfrow = c(2, 2))
    plot(regAIC)
dev.off()

regBIC <- step(reg1, scope = list(lower = regNull, upper = reg1), 
              direction = "both", trace = FALSE, k = log(nrow(reg1$model)))
ncvTest(regBIC)
cairo_pdf(filename = paste(picFile, "reg_regBIC.pdf", sep = ""), 
          width = 8, height = 8, pointsize = 10)
    par(mfrow = c(2, 2))
    plot(regBIC)
dev.off()


# 
# texreg(l = list(reg0, reg1, regAIC, regBIC), single.row = T,
#        leading.zero = FALSE, booktabs = TRUE, dcolumn = TRUE)


## Feature Selection using LASSO, lambda tuned by CV
require(glmnet)
X <- as.matrix(data[-outlierIndex, c(1:30, 33)])
y <- as.matrix(data$Y1[-outlierIndex])

regLASSOcv <- cv.glmnet(X, y, nfolds = 10)
# plot(regLASSOcv)

regLASSO_1 <- glmnet(X, y, lambda = regLASSOcv$lambda.min)
regLASSO_2 <- glmnet(X, y, lambda = regLASSOcv$lambda.1se)

cairo_pdf(filename = paste(picFile, "reg_regLASSO.pdf", sep = ""), 
          width = 8, height = 4, pointsize = 10)
    par(mfrow = c(1, 2))
    qqnorm(predict(regLASSO_1, X))
    qqline(y, col = 2)
    qqnorm(predict(regLASSO_2, X))
    qqline(y, col = 2)
dev.off()


## Feature selection using lars, lambda selected by smallest Cp statistic
require(lars)
regLars <- lars(x = X, y = y, type = "lasso")
s <- which.min(summary(regLars)$Cp)
predLars <- predict(regLars, type = "fit", newx = X, s = s, mode = "step")$fit

cairo_pdf(filename = paste(picFile, "reg_regLars.pdf", sep = ""), 
          width = 4, height = 4, pointsize = 10)
    qqnorm(predLars)
    qqline(y, col = 2)
dev.off()

foo <- cbind(as.matrix(coef(regLASSO_1))[2:32], as.matrix(coef(regLASSO_2))[2:32], as.matrix(coef(regLars)[s, ]))
foo <- round(foo, 2)
write.table(foo, "./FinalProject/reglasso")



# prediction power --------------------------------------------------------


## Selection from lm, lmAIC, lmBIC, LASSO by outer cross-validation
require(caret)
require(doMC)
registerDoMC(2)

fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10, 
    repeats = 1
)

fitControl0 <- trainControl(## 10-fold CV
    method = "none",
)

num_iter = 40
result <- matrix(nrow = num_iter, ncol = 6)
data0 <- data[-outlierIndex, ]
set.seed(123)

for (jj in 1:num_iter)
{
    i = 1
    trainIndex <- createDataPartition(data0$Y1, p = .6,
                                      list = FALSE,
                                      times = 1)
    data_train <- data0[trainIndex, ]
    data_test <- data0[-trainIndex, ]
    
    ## regression part
    X_train <- data_train[, c(1:30, 33)]
    y_train <- data_train[, 31]
    X_test <- data_test[, c(1:30, 33)]
    y_test <- data_test[, 31]
    
    ## simple linear regression
    m <- train(y = y_train, x = X_train,
               trControl = fitControl0,
               method = "lm")
    result[jj, i] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    i = i + 1
    
    ## stepwise linear model AIC
    m <- train(y = y_train, x = X_train,
               trControl = fitControl0,
               method = "lmStepAIC", trace = FALSE)
    result[jj, i] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    i = i + 1
    
    ## stepwise linear model BIC
    m <- train(y = y_train, x = X_train,
               trControl = fitControl0,
               method = "lmStepAIC", trace = FALSE, 
               k = log(nrow(X_train)))
    result[jj, i] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    i = i + 1
    
    ## lasso (lars)
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "lars2", 
               tuneGrid = expand.grid(step = 2:31))
    result[jj, i] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    i = i + 1
    
    ## lasso (glmnet) lambda.min
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "glmnet", 
               tuneGrid = expand.grid(alpha = 1, 
                                      lambda = seq(0.001, 0.2, 0.002)))
    result[jj, i] <- sqrt(mean((y_test - predict(m, newdata = X_test))^2))
    i = i + 1
    
    ## lasso (glmnet) lambda.1se
    mcv <- cv.glmnet(as.matrix(X_train), as.matrix(y_train), nfolds = 10, 
                     lambda = seq(0.001, 0.2, 0.002))
    m <- glmnet(as.matrix(X_train), as.matrix(y_train), lambda = mcv$lambda.1se)
    result[jj, i] <- sqrt(mean((y_test - predict(m, as.matrix(X_test)))^2))
    
    print(jj)
}

cairo_pdf(filename = paste(picFile, "reg_rmse6.pdf", sep = ""), 
          width = 7, height = 7, pointsize = 10)
    boxplot(result, main = "Test RMSE for 6 parametric models",
            names = c("lm", "lmAIC", "lmBIC", "Lars", "LASSO.min", "LASSO.1se"))
    abline(median(result[,1]), 0, lty = 2)
    abline(min(apply(result, 2, median)), 0, lty = 2)
dev.off()

##write.table(result, file = "./FinalProject/regResultPara")
