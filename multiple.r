Y <- c(78.5, 74.3, 104.3, 87.6, 95.9, 109.2, 72.5, 93.1)
X1 <- c(7.1, 11, 11, 7, 11, 3, 1, 21)
X2 <- c(26, 29, 56, 31, 52, 55, 71, 31)
X3 <- c(6, 1, 8, 8, 6, 9, 17, 22)
X4 <- c(60, 52, 20, 47, 33, 22, 6, 4)

df <- data.frame(Y, X1, X2, X3, X4)

# fit multiple regression model
model <- lm(Y ~ X1 + X2 + X3 + X4, data = df)

# print summary of model
summary(model)

#perform backward stepwise regression
model_bwd <- step(model, direction="backward")
summary(model_bwd)
