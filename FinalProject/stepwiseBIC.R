require(car)

stepwiseBIC <- function(X, y, X_test, y_test, family = "gaussian", outlier = FALSE)
{
    result <- list(prediction = NULL, error = NULL, coef = NULL, model = NULL)
    if (family == "gaussian")
    {
        data <- data.frame(X, y)
        ## Establish a model
        m0 <- lm(y ~ 1, data = data)
        m1 <- lm(y ~ ., data = data)
        m_BIC <- step(m0, scope = list(lower = m0, upper = m1),
                      direction = "both", trace = FALSE, k = log(nrow(data)))
        ## Rremove Outliers
        if (outlier)
        {
            data <- data[-as.numeric(names(outlierTest(m_BIC)$bonf.p)), ]
            m0 <- lm(y ~ 1, data = data)
            m1 <- lm(y ~ ., data = data)
            m_BIC <- step(m0, scope = list(lower = m0, upper = m1),
                          direction = "both", trace = FALSE, k = log(nrow(data)))
            
            result$model <- m_BIC
            result$coef <- m_BIC$coefficients
            result$prediction <- predict(m_BIC, newdata = X_test)
            result$error <- mean((result$prediction - y_test)^2)
        }
        else
        {
            result$model <- m_BIC
            result$coef <- m_BIC$coefficients
            result$prediction <- predict(m_BIC, newdata = X_test)
            result$error <- mean((result$prediction - y_test)^2)
        }
    }
    else 
    {
        data <- data.frame(X, y = as.factor(y))
        ## Establish a model
        m0 <- glm(y ~ 1, data = data, family = binomial)
        m1 <- glm(y ~ ., data = data, family = binomial)
        m_BIC <- step(m0, scope = list(lower = m0, upper = m1),
                      direction = "both", trace = FALSE)
        ## Rremove Outliers
        if (outlier)
        {
            data <- data[-as.numeric(names(outlierTest(m_BIC)$bonf.p)), ]
            m0 <- glm(y ~ 1, data = data, family = binomial)
            m1 <- glm(y ~ ., data = data, family = binomial)
            m_BIC <- step(m0, scope = list(lower = m0, upper = m1),
                          direction = "both", trace = FALSE, k = log(nrow(data)))
            result$model <- m_BIC
            result$coef <- m_BIC$coefficients
            result$prediction <- as.numeric(predict(m_BIC, newdata = X_test, type = "response") > 0.5)
            tmp <- table(result$prediction, y_test)
            result$error <- tmp[1,2] + tmp[2,1]
        }
        else
        {
            result$model <- m_BIC
            result$coef <- m_BIC$coefficients
            result$prediction <- as.numeric(predict(m_BIC, newdata = X_test, type = "response") > 0.5)
            tmp <- table(result$prediction, y_test)
            result$error <- tmp[1,2] + tmp[2,1]
        }
    }
    return(result)
}