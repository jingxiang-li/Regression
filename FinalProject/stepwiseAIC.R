require(car)

stepwiseAIC <- function(X, y, X_test, y_test, 
                        family = "gaussian", outlier = FALSE)
{
    result <- list(prediction = NULL, error = NULL, coef = NULL, model = NULL)
    if (family == "gaussian")
    {
        data <- data.frame(X, y)
        ## Establish a model
        m0 <- lm(y ~ 1, data = data)
        m1 <- lm(y ~ ., data = data)
        m_AIC <- step(m0, scope = list(lower = m0, upper = m1),
                      direction = "both", trace = FALSE)
        ## Rremove Outliers
        if (outlier)
        {
            data <- data[-as.numeric(names(outlierTest(m_AIC)$bonf.p)), ]
            m0 <- lm(y ~ 1, data = data)
            m1 <- lm(y ~ ., data = data)
            m_AIC <- step(m0, scope = list(lower = m0, upper = m1),
                          direction = "both", trace = FALSE)
            
            result$model <- m_AIC
            result$coef <- m_AIC$coefficients
            result$prediction <- predict(m_AIC, newdata = X_test)
            result$error <- mean((result$prediction - y_test)^2)
        }
        else
        {
            result$model <- m_AIC
            result$coef <- m_AIC$coefficients
            result$prediction <- predict(m_AIC, newdata = X_test)
            result$error <- mean((result$prediction - y_test)^2)
        }
    }
    else 
    {
        data <- data.frame(X, y = as.factor(y))
        ## Establish a model
        m0 <- glm(y ~ 1, data = data, family = binomial)
        m1 <- glm(y ~ ., data = data, family = binomial)
        m_AIC <- step(m0, scope = list(lower = m0, upper = m1),
                      direction = "both", trace = FALSE)
        ## Rremove Outliers
        if (outlier)
        {
            data <- data[-as.numeric(names(outlierTest(m_AIC)$bonf.p)), ]
            m0 <- glm(y ~ 1, data = data, family = binomial)
            m1 <- glm(y ~ ., data = data, family = binomial)
            m_AIC <- step(m0, scope = list(lower = m0, upper = m1),
                          direction = "both", trace = FALSE)
            result$model <- m_AIC
            result$coef <- m_AIC$coefficients
            result$prediction <- as.numeric(predict(m_AIC, newdata = X_test, type = "response") > 0.5)
            tmp <- table(result$prediction, y_test)
            result$error <- tmp[1,2] + tmp[2,1]
        }
        else
        {
            result$model <- m_AIC
            result$coef <- m_AIC$coefficients
            result$prediction <- as.numeric(predict(m_AIC, newdata = X_test, type = "response") > 0.5)
            tmp <- table(result$prediction, y_test)
            result$error <- tmp[1,2] + tmp[2,1]
        }
    }
    return(result)
}