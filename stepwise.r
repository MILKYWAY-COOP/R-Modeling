library(readxl)

# Load the dataset
data <- read_excel("C:/Steve/class work/4.2/model/r/data.xlsx")

# extract predictor variables
educ <- data$Educ
precip <- data$Precip
nonWhite <- data$Nonwhite

# extract dependent variable
mort <- data$Mort

# perform stepwise regression
model <- lm(mort ~ educ + precip + nonWhite)
model <- step(model)
summary(model)

# perform forward stepwise regression
model_fwd <- lm(mort ~ 1)
model_fwd <- step(model_fwd, scope = list(lower = ~1, upper = ~educ + precip + nonWhite), direction = "forward")

# perform backward stepwise regression
model_bwd <- lm(mort ~ educ + precip + nonWhite)
model_bwd <- step(model_bwd, direction = "backward")

# print summary of both models
summary(model_fwd)
summary(model_bwd)
