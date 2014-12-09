RF <- function(X, y, family = "gaussian")
{
    data <- data.frame(X, y)
    if (family == "gaussian")
    {
        result <- c()
        n1 <- floor(ncol(X) / 3)
        n2 <- floor(ncol(X) / 3 * 2)
        for (i in n1:n2)
        {
            m <- randomForest(y ~ ., data = data, mtry = i, ntree = 300)
            result[i - n1 + 1] <- min(m$mse)
        }
        mtry_opt <- order(result)[1] + n1 - 1
        m <- randomForest(y ~ ., data = data, mtry = mtry_opt, ntree = 300)
        m
    }
    else
    {
        result <- c()
        n1 <- floor(sqrt(ncol(X)))
        n2 <- floor(sqrt(ncol(X)) * 4)
        for (i in n1:n2)
        {
            m <- randomForest(as.factor(y) ~ ., data = data, mtry = i, ntree = 300)
            result[i - n1 + 1] <- min(m$err.rate[,1])
        }
        mtry_opt <- order(result)[1] + n1 - 1
        m <- randomForest(y ~ ., data = data, mtry = mtry_opt, ntree = 300)
        m
    }
}