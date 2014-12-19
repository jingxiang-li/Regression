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

pdata$X10 <- log(pdata$X10 + 0.01)
pdata$X15 <- log(pdata$X15 + 0.01)

fitControl0 <- trainControl(## 10-fold CV
    method = "none",
)

require(doMC)
registerDoMC(2)
require(caret)


m1 <- train(y = data$Y1, x = data[, 1:30],
           trControl = fitControl0,
           method = "gcvEarth", 
           tuneGrid = expand.grid(degree = 2))
predY1 <- predict(m1, newdata = pdata)


m2 <- train(y = as.factor(data$Y2), x = data[, 1:30],
           trControl = fitControl0,
           method = "glmStepAIC", trace = FALSE, 
           k = log(nrow(data)), family = binomial)
predY2 <- predict(m2, pdata)
predY2 <- as.numeric(as.character(predY2))

## Start with predY1: prediction vector for Y1
## and predY2: prediction vector for Y2

## Put your first name and last name in place of YourName
JingxiangLi <- data.frame(cbind(predY1, predY2))
save(JingxiangLi, file="./FinalProject/JingxiangLi_8051final.Rda")





m <- lm(Y1 ~ . + I(X4^2), data = data[, c(1:31)])
m <- step(m, direction = "both", trace = 0)
plot(predY1, predict(m, pdata))



m <- glm(as.factor(Y2) ~ ., data = data[, c(1:30, 32)], family = "binomial")
m <- step(m, direction = "both", trace = 0)
cbind(predY2, as.numeric(predict(m, pdata) > 0))




