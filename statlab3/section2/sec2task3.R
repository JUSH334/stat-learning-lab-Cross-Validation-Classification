# ============================================
# Section 2, Task 3: Classification with k-Fold CV
# ============================================
library(MASS)
library(boot)

# Part A: Fit LDA model with train/test split
# Using productivity_score as a different angle
# Target: exam_class (same as before)
# Features: study_hours, focus_index, caffeine_intake_mg, exercise_minutes, gaming_hours

set.seed(5698)
train_idx3 <- sample(nrow(data), nrow(data) * 0.7)
train3 <- data[train_idx3, ]
test3 <- data[-train_idx3, ]

lda.fit3 <- lda(exam_class ~ study_hours + focus_index + caffeine_intake_mg + exercise_minutes + gaming_hours,
                data = train3)
lda.fit3

lda.pred3 <- predict(lda.fit3, test3)
table(lda.pred3$class, test3$exam_class)
cat("LDA Test Accuracy:", mean(lda.pred3$class == test3$exam_class), "\n")

# Part B: k-Fold Cross-Validation
# We need to use glm for cv.glm, so we create a numeric target
data$exam_binary <- ifelse(data$exam_class == "Low", 1, 0)

# 5-Fold CV
set.seed(5698)
glm.fit.cv <- glm(exam_binary ~ study_hours + focus_index + caffeine_intake_mg + exercise_minutes + gaming_hours,
                  data = data, family = binomial)
cv.err.5 <- cv.glm(data, glm.fit.cv, K = 5)$delta[1]
cat("5-Fold CV Error:", cv.err.5, "\n")
cat("5-Fold CV Accuracy:", 1 - cv.err.5, "\n")

# 10-Fold CV
set.seed(5698)
cv.err.10 <- cv.glm(data, glm.fit.cv, K = 10)$delta[1]
cat("10-Fold CV Error:", cv.err.10, "\n")
cat("10-Fold CV Accuracy:", 1 - cv.err.10, "\n")

# Summary comparison
cat("\n=== Final Comparison ===\n")
cat("LDA Test Set Accuracy (70/30):", mean(lda.pred3$class == test3$exam_class), "\n")
cat("5-Fold CV Accuracy:", 1 - cv.err.5, "\n")
cat("10-Fold CV Accuracy:", 1 - cv.err.10, "\n")