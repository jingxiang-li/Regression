require(corrplot)

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
data$X10 = log(data$X10 + 0.01)
data$X15 = log(data$X15 + 0.01)
data$X4sq = data$X4 ^ 2
boxplot(data[,1:30])


