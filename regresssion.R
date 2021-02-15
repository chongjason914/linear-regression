# Set working directory 
setwd("C:/Users/Jason Chong/Downloads")

# Loa data
baby = read.table("baby weights.txt", header = T)

# Show data
baby

# Plot data
plot(baby, main = "Estimated baby weights during pregnancy", xlab = "Gestation period (weeks)", ylab = "Weight (kg)")

# Attach data 
attach(baby)

# Compute correlation between gestation period and baby weight
cor(gestation, weight)

# Test whether correlation coefficient is zero
cor.test(gestation, weight)

# Fit linear model
model = lm(weight ~ gestation)

# Examine model 
model
summary(model)

# Add regression line
abline(model, col = "red", lty = "dashed")

# Obtain fitted values 
model$fitted.values
fitted(model)
predict(model)

# Add fitted values to regression line 
points(gestation, fitted(model), col = "blue", pch = 16)

# ANOVA
anova(model)

x = gestation; y = weight
# Total sum of squares 
(SStot = sum((y-mean(y))^2))
sum(anova(model)[,2])

# Residual sum of squares 
(SSres = sum((y-fitted(model))^2))
anova(model)[2,2]

# Regression sum of squares 
(SSreg = sum((fitted(model)-mean(y))^2))
anova(model)[1,2]

# Coefficient of determination
summary(model)$r.squared
cor(x,y)^2
SSreg/SStot

# Confidence interval for slope and regression coefficient 
confint(model, level = 0.99)

# Predict baby's weight at 33 weeks
# First principle
coef(model)[1]+coef(model)[2]*33

# Using predict function 
newdata = data.frame(gestation = 33)
predict(model, newdata)

# Confidence interval for prediction
predict(model, newdata, interval = "confidence", level = 0.9)

# Prediction interval for prediction 
predict(model, newdata, interval = "predict", level = 0.9)

# Residuals
resid(model)

# Residuals by first principle 
resid(model)
weight - fitted(model)

# Plot residuals vs fitted 
plot(model, 1)
 
# QQ plot
plot(model, 2)

# Exclude gestation = 32
model2 = update(model, subset = (gestation != 32))
summary(model)$r.squared; summary(model2)$r.squared