# Task 5: Bootstrap with different sample sizes
# Quadratic fit function for horsepower -> mpg
boot.fn <- function(data, index)
  coef(lm(mpg ~ horsepower + I(horsepower^2), 
          data = data, subset = index))

# --- R = 250 ---
set.seed(5698)
boot_250 <- boot(Auto, boot.fn, R = 250)
cat("=== Bootstrap R = 250 ===\n")
print(boot_250)

# --- R = 500 ---
set.seed(5698)
boot_500 <- boot(Auto, boot.fn, R = 500)
cat("\n=== Bootstrap R = 500 ===\n")
print(boot_500)

# --- R = 2500 ---
set.seed(5698)
boot_2500 <- boot(Auto, boot.fn, R = 2500)
cat("\n=== Bootstrap R = 2500 ===\n")
print(boot_2500)

# Summary comparison
cat("\n=== Standard Error Comparison ===\n")
cat("         Intercept SE | Horsepower SE | Horsepower^2 SE\n")
cat("R=250:  ", boot_250$t0, "\n")
cat("R=500:  ", boot_500$t0, "\n")
cat("R=2500: ", boot_2500$t0, "\n")