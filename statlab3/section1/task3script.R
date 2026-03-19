# Task 3: LOOCV for poly orders 1:8 using WEIGHT to predict mpg
library(boot)

cv.error <- rep(0, 8)

for (i in 1:8) {
  glm.fit <- glm(mpg ~ poly(weight, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}

# Print all CV errors
cat("LOOCV Errors for poly orders 1:8 (weight -> mpg):\n")
for (i in 1:8) {
  cat("  Order", i, ":", cv.error[i], "\n")
}

# Plot the results
plot(1:8, cv.error, type = "b", 
     xlab = "Polynomial Order", 
     ylab = "LOOCV Test MSE",
     main = "LOOCV: Poly Orders 1-8 (Weight -> MPG)",
     col = "blue", pch = 19)