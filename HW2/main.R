
# 3.2 ---------------------------------------------------------------------

# 3.2.1
require(package = "alr4")
require(package = "ggplot2")
require(package = "gridExtra")

data <- data.frame(UN11$fertility, log(UN11$ppgdp), UN11$pctUrban)
# p1 <- ggplot(data = UN11, aes(x = log(ppgdp), y = fertility))
# p1 <- p1 + geom_point()
# p1 <- p1 + geom_smooth(formula = y ~ x, method = "lm", se = FALSE)
# p1 <- p1 + theme_bw() + theme(text = element_text(size = 18))
# p1 <- p1 + ggtitle("fertility ~ log(ppgdp)")
# 
# p2 <- ggplot(data = UN11, aes(x = pctUrban, y = fertility))
# p2 <- p2 + geom_point()
# p2 <- p2 + geom_smooth(formula = y ~ x, method = "lm", se = FALSE)
# p2 <- p2 + theme_bw() + theme(text = element_text(size = 18))
# p2 <- p2 + ggtitle("fertility ~ pctUrban")
# grid.arrange(p1, p2, ncol=2)

panel.cor <- function(x, y, digits = 2, prefix = "cor = ", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(data, lower.panel = panel.smooth, upper.panel = panel.cor)

# 3.2.2
m1 <- lm(fertility ~ log(ppgdp), data = UN11)
m2 <- lm(fertility ~ pctUrban, data = UN11)
summary(m1)
summary(m2)

# 3.2.3
e_f2lp <- m1$residuals
e_f2pU <- m2$residuals
e_lp2pU <- lm(log(ppgdp) ~ pctUrban, data = UN11)$residuals
e_pU2lp <- lm(pctUrban ~ log(ppgdp), data = UN11)$residuals

p1 <- ggplot(data.frame(x = e_pU2lp, y = e_f2lp), aes(x = x, y = y))
p1 <- p1 + geom_point()
p1 <- p1 + geom_smooth(formula = y ~ x, method = "lm", se = FALSE)
p1 <- p1 + theme_bw() + theme(text = element_text(size = 18))
p1 <- p1 + ggtitle("pctUrban after log(ppgdp)")

p2 <- ggplot(data.frame(x = e_lp2pU, y = e_f2pU), aes(x = x, y = y))
p2 <- p2 + geom_point()
p2 <- p2 + geom_smooth(formula = y ~ x, method = "lm", se = FALSE)
p2 <- p2 + theme_bw() + theme(text = element_text(size = 18))
p2 <- p2 + ggtitle("log(ppgdp) after pctUrban")
grid.arrange(p1, p2, ncol=2)

# 3.2.4
m1 <- coef(lm(fertility ~ log(ppgdp) + pctUrban, data = UN11))
m2 <- coef(lm(e_f2pU ~ e_lp2pU))

# 3.2.5
m1 <- lm(fertility ~ log(ppgdp) + pctUrban, data = UN11)
m2 <- lm(e_f2pU ~ e_lp2pU)
p <- ggplot(data.frame(m1$residuals, m2$residuals), 
            aes(x = m1$residuals, y = m2$residuals))
p <- p + geom_point()
p <- p + theme_bw() + theme(text = element_text(size = 16))
p <- p + ggtitle("Residuals VS Residuals")
p <- p + xlab("Residuals from added-variable model") +
    ylab("Residuals from two predictor model")
p

# 3.2.6
summary(m1)
summary(m2)


# 4.2 ---------------------------------------------------------------------
Transact$a <- (Transact$t1 + Transact$t2) / 2
Transact$d <- (Transact$t1 - Transact$t2)
m1 <- lm(time ~ t1 + t2, data = Transact)
m2 <- lm(time ~ a + d, data = Transact)
m3 <- lm(time ~ t2 + d, data = Transact)
m4 <- lm(time ~ t1 + t2 + a + d, data = Transact)


m4 <- lm(time ~ a + d + t1 + t2, data = Transact)


##4.12

n <- 300
x <- rnorm(n = n)
sigma <- 1
y <- 2 + 3 * x + rnorm(n = n, sd = sigma)

f412 = function(x, y)
{
p <- ggplot(data.frame(x, y), aes(x = x, y = y))
p <- p + geom_point()
p <- p + geom_abline(intercept = 2, slope = 3, linetype = 2, size = 1, col = "#ff0000")
p <- p + geom_smooth(formula = y ~ x, method = "lm", se = FALSE, size = 1)
p <- p + theme_bw() + theme(text = element_text(size = 14))
p <- p + ggtitle("scatterplot of Y versus X")
return(p)
}

f412(x, y = 2 + 3 * x + rnorm(n = n, sd = 3))
f412(x, y = 2 + 3 * x + rnorm(n = n, sd = 5))


e <- rnorm(n = n, sd = 1) / rnorm(n = n, sd = 1)
f412(x, y = 2 + 3 * x + e)


# 5.8 ---------------------------------------------------------------------
## cakes
m <- lm(Y ~ X1 + X2 + I(X1^2) + I(X2^2) + I(X1*X2), data = cakes)
summary(m)


cakes$block <- as.numeric(as.character(cakes$block))
m1 <- lm(Y ~ X1 + X2 + I(X1^2) + I(X2^2) + I(X1*X2) + 
             block + I(X1 * block) + I(X2 * block) + 
             I(X1^2 * block) + I(X2^2 * block) + 
             I(X1*X2 * block), data = cakes)
m2 <- step(object = m1, direction = "both", trace = FALSE)
summary(m2)


# 5.14 --------------------------------------------------------------------
##BGSall
rm(list = la())
library(package = alr4)

BGSall$Sex <- factor(BGSall$Sex)
p <- ggplot(data = BGSall, aes(x = HT9, y = HT18))
p <- p + geom_point(aes(shape = Sex))
p

m <- lm(HT18 ~ HT9 + Sex, data = BGSall)
summary(m)
confint(object = m, level = .95)

