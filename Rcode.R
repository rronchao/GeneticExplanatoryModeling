library(MASS)
library(corrplot)
library(leaps)
library(knitr)

dat <- read.csv("C:\\Users\\ronal\\Desktop\\resume projects\\Project 2\\project2\\SP21_P2_782362.csv")

# Modeling using only explanatory variables
summary(lm(Y ~ E1+E2+E3+E4, data=dat))$adj.r.square
# R squared value: 0.51575

# Create a model based off of correlation plot
corrplot(cor(dat))
model_corr <- lm(Y ~ E1+E3+E4+G1+G2, data=dat)
summary(model_corr)$adj.r.square
# R squared value: 0.5556857

# Create a model with all variables up to second order interactions
model2 <- lm(Y ~ .^2, data = dat)
summary(model2)

# Using step-wise regression to find a more optimal model
stepwise_reg <- function(model, method, title) {
  M <- regsubsets(model.matrix(model)[,-1], model.frame(model)$Y,
                   nbest = 1 , nvmax=6, 
                   method = method, intercept = TRUE )
  temp <- summary(M)
  Var <- colnames(model.matrix(model))
  M_select <- apply(temp$which, 1, 
                    function(x) paste0(Var[x], collapse='+'))
  kable(data.frame(cbind(model = M_select, adjR2 = temp$adjr2, BIC = temp$bic)),
        caption=paste(title, ", method:", method), "simple")
}


model1 <- lm(Y~., data=dat)
stepwise_reg(model1, 'forward', 'Model: Y~.')
stepwise_reg(model1, 'backward', 'Model: Y~.')

stepwise_reg(model2, 'forward', 'Model: Y~.^2')
stepwise_reg(model2, 'backward', 'Model: Y~.^2')

# box cox of model with second order interactions
boxcox(model2)
# We see that lambda = 2
model1_trans <- lm(Y^2 ~ ., data=dat)
stepwise_reg(model1_trans, 'forward', 'Model: Y^2~.')
stepwise_reg(model1_trans, 'backward', 'Model: Y^2~.')

model2_trans <- lm(Y^2 ~ .^2, data=dat)
stepwise_reg(model2_trans, 'forward', 'Model: Y^2~.^2')
stepwise_reg(model2_trans, 'backward', 'Model: Y^2~.^2')

# Looking at significance of the variables. We see G4 is not significant at 0.0001 level.
temp <- summary(model1_trans)
temp$coefficients[ abs(temp$coefficients[,4]) <= 0.0001, ]

temp <- summary(model2_trans)
temp$coefficients[ abs(temp$coefficients[,4]) <= 0.0001, ]


#Plot Actual vs Fitted
# For variables chosen by correlation
par(mfrow=c(1,2))

plot(dat$Y, model_corr$fitted.values, 
     main="Actual vs Fitted", 
     ylab = "Y~E1+E3+E4+G1+G2",
     xlab = "Y",
     sub=paste("adj. r-square = ", summary(model_corr)$adj.r.square))
abline(0, 1)

# For variables chosen by best formula chosen by stepwise regression
model_step <- lm(Y^2~E1+E3+E4+G1+G2, data=dat)
plot(dat$Y^2, model_step$fitted.values, 
     main="Actual vs Fitted" , 
     ylab = "Y^2~ E1+E3+E4+G1+G2",
     xlab = "Y^2",
     sub = paste("adj. r-square = ", summary(model_step)$adj.r.square))
abline(0, 1)

plot(resid(model_corr) ~ fitted(model_corr), main='Residual Plot', sub = "Y = E1+E3+E4+G1+G2")
plot(resid(model_step) ~ fitted(model_step), main='Residual Plot', sub = "Y^2 = E1+E3+E4+G1+G2")

