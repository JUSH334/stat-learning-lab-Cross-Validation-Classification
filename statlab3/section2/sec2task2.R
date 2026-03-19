# ============================================
# Section 2, Task 2: Linear Discriminant Analysis
# ============================================
library(MASS)

# Fit LDA using same predictors and training data
lda.fit <- lda(exam_class ~ study_hours + sleep_hours + focus_index + mental_health_score,
               data = train_data)
lda.fit

# Plot the LDA model
par(mfrow = c(1, 1))
par(mar = c(4, 4, 3, 1))
plot(lda.fit)

# Predictions on test set
lda.pred <- predict(lda.fit, test_data)
lda.class <- lda.pred$class

# Confusion matrix and accuracy
table(lda.class, test_data$exam_class)
cat("LDA Accuracy:", mean(lda.class == test_data$exam_class), "\n")

# ---- Compare Logistic Regression vs LDA ----
cat("\n=== Model Comparison ===\n")
cat("Logistic Regression Accuracy:", mean(glm.pred == test_data$exam_class), "\n")
cat("LDA Accuracy:", mean(lda.class == test_data$exam_class), "\n")