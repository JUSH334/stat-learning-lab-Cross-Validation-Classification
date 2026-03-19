# Task 2: Different train/test ratios using quadratic (best from Task 1)
set.seed(5698)

n <- nrow(Auto)  # 392 total observations

# --- Ratio 1: 60/40 ---
train1 <- sample(n, n * 0.6)  # 235 training obs
lm.fit2a <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train1)
mse_60 <- mean((mpg - predict(lm.fit2a, Auto))[-train1]^2)
cat("60/40 Quadratic Test MSE:", mse_60, "\n")

# --- Ratio 2: 70/30 ---
set.seed(5698)
train2 <- sample(n, n * 0.7)  # 274 training obs
lm.fit2b <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train2)
mse_70 <- mean((mpg - predict(lm.fit2b, Auto))[-train2]^2)
cat("70/30 Quadratic Test MSE:", mse_70, "\n")

# --- Ratio 3: 80/20 ---
set.seed(5698)
train3 <- sample(n, n * 0.8)  # 314 training obs
lm.fit2c <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train3)
mse_80 <- mean((mpg - predict(lm.fit2c, Auto))[-train3]^2)
cat("80/20 Quadratic Test MSE:", mse_80, "\n")