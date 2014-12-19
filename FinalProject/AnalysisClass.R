rm(list = ls())


# read data ---------------------------------------------------------------


load("./FinalProject/data.Rda")
load("./FinalProject/pdata.Rda")

picFile <- c("./FinalProject/pic/")
texFile <- c("./FinalProject/tex/")


# preprocessing -----------------------------------------------------------

## Tansform X10 and X15
data$X10 <- log(data$X10 + 0.01)
data$X15 <- log(data$X15 + 0.01)
pdata$X10 <- log(pdata$X10 + 0.01)
pdata$X15 <- log(pdata$X15 + 0.01)


# parametric binomial model ----------------------------------------------------------

## ScatterPlot
cairo_pdf(filename = paste(picFile, "class_scatterPlot.pdf", sep = ""), 
          width = 30, height = 22, pointsize = 18)
    par(mfrow = c(5, 6))
    for (i in 1:30) plot(data[, i], data$Y2, 
                         xlab = colnames(data)[i], ylab = "Y1", 
                         main = paste("Y1 against", colnames(data)[i]))
dev.off()

## AUC plot
require(AUC)
cairo_pdf(filename = paste(picFile, "class_AUCPlot.pdf", sep = ""), 
          width = 30, height = 22, pointsize = 18)
    par(mfrow = c(5, 6))
    for (i in 1:30) plot(roc(data[, i], as.factor(data$Y2)),
                         main = paste("Y1 against", colnames(data)[i]))
dev.off()


## Base model, multivariate linear regression, reg0
require(car)
reg0 <- glm(as.factor(Y2) ~ . - Y1, data = data, family = binomial)

cairo_pdf(filename = paste(picFile, "class_reg0.pdf", sep = ""), 
          width = 8, height = 8, pointsize = 10)
    par(mfrow = c(2, 2))
    plot(reg0)
dev.off()

## try probit link, no significant improve
reg1 <- glm(as.factor(Y2) ~ . - Y1, data = data, family = binomial(link = "probit"))

cairo_pdf(filename = paste(picFile, "class_reg1probit.pdf", sep = ""), 
          width = 8, height = 8, pointsize = 10)
    par(mfrow = c(2, 2))
    plot(reg1)
dev.off()

## no outlier need to be eliminated
tmp <- outlierTest(reg0)
outlierIndex <- as.numeric(row.names(as.matrix(tmp$rstudent)))

## step AIC BIC, regAIC regBIC
regNull <- glm(as.factor(Y2) ~ 1, data = data, family = "binomial")
regAIC <- step(reg0, scope = list(lower = regNull, upper = reg0), 
               direction = "both", trace = FALSE)
cairo_pdf(filename = paste(picFile, "class_regAIC.pdf", sep = ""), 
          width = 8, height = 8, pointsize = 10)
    par(mfrow = c(2, 2))
    plot(regAIC)
dev.off()

regBIC <- step(reg0, scope = list(lower = regNull, upper = reg0), 
               direction = "both", trace = FALSE, k = log(nrow(reg0$model)))
cairo_pdf(filename = paste(picFile, "class_regBIC.pdf", sep = ""), 
          width = 8, height = 8, pointsize = 10)
    par(mfrow = c(2, 2))
    plot(regBIC)
dev.off()


texreg(l = list(reg0, reg1, regAIC, regBIC), single.row = TRUE, leading.zero = FALSE, 
       booktabs = TRUE, dcolumn = TRUE)





## LASSO glmnet regLASSO, lambda tuned by CV
require(glmnet)
X <- as.matrix(data[, c(1:30)])
y <- as.factor(data$Y2)

regLASSOcv <- cv.glmnet(X, y, nfolds = 10, family = "binomial")
# plot(regLASSOcv)

regLASSO_1 <- glmnet(X, y, family = "binomial", 
                     lambda = regLASSOcv$lambda.min)
regLASSO_2 <- glmnet(X, y, family = "binomial", 
                     lambda = regLASSOcv$lambda.1se)

tmp <- cbind(as.matrix(coef(regLASSO_1)), as.matrix(coef(regLASSO_2)))
write.table(tmp, "~/tmp")

# cairo_pdf(filename = paste(picFile, "class_AUCs.pdf", sep = ""), 
#           width = 6, height = 9, pointsize = 10)
#     par(mfrow = c(3, 2))
#     plot(roc(predict(reg0), y), main = "glm + binomial")
#     tmp <- round(auc(roc(predict(reg0), y)), 4)
#     text(0.3, 0.7, paste("AUC =", tmp), cex = 1.2)
# 
#     plot(roc(predict(reg1), y), main = "glm + probit")
#     tmp <- round(auc(roc(predict(reg1), y)), 4)
#     text(0.3, 0.7, paste("AUC =", tmp), cex = 1.2)
# 
#     plot(roc(predict(regAIC), y), main = "glm + AIC")
#     tmp <- round(auc(roc(predict(regAIC), y)), 4)
#     text(0.3, 0.7, paste("AUC =", tmp), cex = 1.2)
# 
#     plot(roc(predict(regBIC), y), main = "glm + BIC")
#     tmp <- round(auc(roc(predict(regBIC), y)), 4)
#     text(0.3, 0.7, paste("AUC =", tmp), cex = 1.2)
# 
#     plot(roc(predict(regLASSO_1, X), y), main = "glmnet + LASSO.min")
#     tmp <- round(auc(roc(predict(regLASSO_1, X), y)), 4)
#     text(0.3, 0.7, paste("AUC =", tmp), cex = 1.2)
# 
#     plot(roc(predict(regLASSO_2, X), y), main = "glmnet + LASSO.1se")
#     tmp <- round(auc(roc(predict(regLASSO_2, X), y)), 4)
#     text(0.3, 0.7, paste("AUC =", tmp), cex = 1.2)
# dev.off()


# prediction power --------------------------------------------------------


## Selection from glm, glmAIC, glmBIC, LASSO by outer cross-validation
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
result <- matrix(nrow = num_iter, ncol = 5)
data0 <- data[-outlierIndex, ]
set.seed(123)

for (jj in 1:num_iter)
{
    i = 1
    trainIndex <- createDataPartition(data$Y2, p = .6,
                                      list = FALSE,
                                      times = 1)
    data_train <- data[trainIndex, ]
    data_test <- data[-trainIndex, ]
    
    ## classification part
    X_train <- data_train[, c(1:30)]
    y_train <- as.factor(data_train[, 32])
    y_train <- factor(y_train, levels=rev(levels(y_train)))
    X_test <- data_test[, c(1:30)]
    y_test <- as.factor(data_test[, 32])
    y_test <- factor(y_test, levels=rev(levels(y_test)))
    
    
    ## simple linear model, glm,
    m <- train(y = y_train, x = X_train,
               trControl = fitControl0,
               method = "glm", family = binomial)
    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[jj, i] <- foo$overall[1]
    i = i + 1
    
    ## stepwise linear model AIC, 2.089027
    m <- train(y = y_train, x = X_train,
               trControl = fitControl0,
               method = "glmStepAIC", trace = FALSE, family = binomial)
    
    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[jj, i] <- foo$overall[1]
    i = i + 1
    
    ## stepwise linear model BIC, best
    m <- train(y = y_train, x = X_train,
               trControl = fitControl0,
               method = "glmStepAIC", trace = FALSE, 
               k = log(nrow(X_train)), family = binomial)
    
    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[jj, i] <- foo$overall[1]
    i = i + 1
    
    ## lasso (glmnet) 2.046684
    m <- train(y = y_train, x = X_train,
               trControl = fitControl,
               method = "glmnet", 
               tuneGrid = expand.grid(alpha = 1, 
                                      lambda = seq(0.001, 0.2, 0.002)), 
               family = "binomial")
    
    foo <- confusionMatrix(predict(m, newdata = X_test), y_test)
    result[jj, i] <- foo$overall[1]
    i = i + 1
    
    ## lasso (glmnet) lambda.1se
    mcv <- cv.glmnet(as.matrix(X_train), y_train, nfolds = 10, 
                     lambda = seq(0.001, 0.2, 0.002), family = "binomial")
    m <- glmnet(as.matrix(X_train), y_train, lambda = mcv$lambda.1se, 
                family = "binomial")
    
    tmp <- as.factor(as.numeric(predict(m, as.matrix(X_test), type = "class")))
    tmp <- factor(tmp, levels=rev(levels(tmp)))
    foo <- confusionMatrix(tmp, y_test)
    result[jj, i] <- foo$overall[1]
    print(jj)
}

cairo_pdf(filename = paste(picFile, "class_Acc5.pdf", sep = ""), 
          width = 6, height = 7, pointsize = 10)
boxplot(result, main = "Test Accuracy for 5 parametric models",
        names = c("glm", "glmAIC", "glmBIC", "LASSO.min", "LASSO.1se"))
abline(median(result[,1]), 0, lty = 2)
abline(max(apply(result, 2, median)), 0, lty = 2)
dev.off()

##write.table(result, file = "./FinalProject/regResultPara")
