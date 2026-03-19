# Load required library
library(ISLR2)

# Set YOUR seed (last 4 digits of student ID)
set.seed(5698)

# Split data: randomly select 196 out of 392 observations for training
train <- sample(392, 196)

# Attach the Auto dataset so we can reference columns directly
attach(Auto)

# --- Quadratic Fit (poly degree 2) ---
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mse_quad <- mean((mpg - predict(lm.fit2, Auto))[-train]^2)
cat("Quadratic Test MSE:", mse_quad, "\n")

# --- Cubic Fit (poly degree 3) ---
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mse_cubic <- mean((mpg - predict(lm.fit3, Auto))[-train]^2)
cat("Cubic Test MSE:", mse_cubic, "\n")