# Task 4: k-Fold CV using DISPLACEMENT to predict mpg
library(boot)
set.seed(5698)

# --- 5-Fold CV for poly orders 1:8 ---
cv.error.5 <- rep(0, 8)
for (i in 1:8) {
  glm.fit <- glm(mpg ~ poly(displacement, i), data = Auto)
  cv.error.5[i] <- cv.glm(Auto, glm.fit, K = 5)$delta[1]
}

cat("5-Fold CV Errors (displacement -> mpg):\n")
for (i in 1:8) {
  cat("  Order", i, ":", cv.error.5[i], "\n")
}

# --- 10-Fold CV for poly orders 1:8 ---
set.seed(5698)
cv.error.10 <- rep(0, 8)
for (i in 1:8) {
  glm.fit <- glm(mpg ~ poly(displacement, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

cat("\n10-Fold CV Errors (displacement -> mpg):\n")
for (i in 1:8) {
  cat("  Order", i, ":", cv.error.10[i], "\n")
}

# --- Plot both side by side ---
par(mfrow = c(1, 2))

plot(1:8, cv.error.5, type = "b",
     xlab = "Polynomial Order", ylab = "CV Test MSE",
     main = "5-Fold CV (Displacement -> MPG)",
     col = "blue", pch = 19, ylim = range(c(cv.error.5, cv.error.10)))

plot(1:8, cv.error.10, type = "b",
     xlab = "Polynomial Order", ylab = "CV Test MSE",
     main = "10-Fold CV (Displacement -> MPG)",
     col = "red", pch = 19, ylim = range(c(cv.error.5, cv.error.10)))