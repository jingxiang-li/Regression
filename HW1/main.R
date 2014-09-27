require(package = "alr4")
require(package = "ggplot2")

data <- Htwt
plot(x = data$ht, y = data$wt)
abline(lm(wt ~ ht, data = data))

p <- ggplot(data = data, aes(x = ht, y = wt))
p <- p + geom_point()
p <- p + geom_smooth(method = "lm", formula = y ~ x, size = 2, se = FALSE) 
p <- p + theme_bw()
p

y <- Htwt$wt
x <- Htwt$ht
model <- lm(y ~ x)
sum(model$residuals ^ 2) / model$df.residual




data <- cathedral
data$group <- ifelse (data$Type == "Romanesque", 0, 1)
data <- data[1:(nrow(data)-7), ]
model_1 <- t.test (Length ~ Type, data = data, var.equal = TRUE)
model_2 <- lm (Length ~ group, data = data)
model_1
summary(model_2)



data <- cathedral
data$group <- ifelse (data$Type == "Romanesque", 0, 1)
data$group_1 <- (data$group - 0.5) * 2
model_3 <- lm (Length ~ group_1, data = data)
summary(model_3)



data <- Heights
model <- lm(dheight ~ mheight, data = data)
summary(model)


data <- snake
model <- lm(Y ~ X - 1, data = data)
coef(model)

## residuals versus the fitted val

p <- ggplot(data = data, aes(x = model$fitted.values, y = model$residuals))
p <- p + geom_point()
p <- p + geom_abline(intercept = sum(model$residuals), slope = 0, col = "red")
p <- p + theme_bw()
p
    
  

p <- ggplot(data = data, aes(x = model$fitted.values, y = model$residuals))
p <- p + geom_point()
p <- p + geom_abline(intercept = sum(model$residuals), slope = 0, col = "red")
p <- p + theme_bw()
p <- p + xlab("residuals") + ylab("fitted.values")
p <- p + theme(text = element_text(size = 20))
p
