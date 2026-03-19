# ============================================
# Section 2, Task 1: Logistic Regression
# ============================================

# Load the dataset (update path if needed)
data <- read.csv("ultimate_student_productivity_dataset_5000.csv")

# Explore the structure
dim(data)
names(data)
summary(data)

# Create binary classification target: High vs Low exam performance
# Split at the median
data$exam_class <- ifelse(data$exam_score >= median(data$exam_score), "High", "Low")
data$exam_class <- as.factor(data$exam_class)
table(data$exam_class)

# ---- Distribution Plots to assess suitability for classification ----
# Reset plot device and make sure Plots panel is large
try(dev.off(), silent = TRUE)
par(mar = c(4, 4, 3, 1))  # Smaller margins to prevent errors
par(mfrow = c(2, 2))

boxplot(study_hours ~ exam_class, data = data, 
        col = c("tomato", "steelblue"),
        main = "Study Hours by Exam Class",
        xlab = "Exam Class", ylab = "Study Hours")

boxplot(sleep_hours ~ exam_class, data = data, 
        col = c("tomato", "steelblue"),
        main = "Sleep Hours by Exam Class",
        xlab = "Exam Class", ylab = "Sleep Hours")

boxplot(focus_index ~ exam_class, data = data, 
        col = c("tomato", "steelblue"),
        main = "Focus Index by Exam Class",
        xlab = "Exam Class", ylab = "Focus Index")

boxplot(mental_health_score ~ exam_class, data = data, 
        col = c("tomato", "steelblue"),
        main = "Mental Health Score by Exam Class",
        xlab = "Exam Class", ylab = "Mental Health Score")

# ---- Logistic Regression ----
# Train/test split (70/30)
set.seed(5698)
train_idx <- sample(nrow(data), nrow(data) * 0.7)
train_data <- data[train_idx, ]
test_data <- data[-train_idx, ]

# Fit logistic regression
glm.fit <- glm(exam_class ~ study_hours + sleep_hours + focus_index + mental_health_score,
               data = train_data, family = binomial)
summary(glm.fit)

# Predictions on test set
glm.probs <- predict(glm.fit, test_data, type = "response")
glm.pred <- rep("High", nrow(test_data))
glm.pred[glm.probs > 0.5] <- "Low"
glm.pred <- as.factor(glm.pred)

# Confusion matrix and accuracy
table(glm.pred, test_data$exam_class)
cat("Logistic Regression Accuracy:", mean(glm.pred == test_data$exam_class), "\n")

# ---- Logistic Regression Curve Plot ----
par(mfrow = c(1, 1))
par(mar = c(5, 4, 4, 2))
order_idx <- order(test_data$focus_index)
plot(test_data$focus_index[order_idx], glm.probs[order_idx],
     type = "l", col = "blue", lwd = 2,
     xlab = "Focus Index", ylab = "P(Low Exam Score)",
     main = "Logistic Regression Curve: Focus Index vs P(Low)")
abline(h = 0.5, lty = 2, col = "red")
legend("topleft", legend = c("Predicted Probability", "0.5 Threshold"),
       col = c("blue", "red"), lty = c(1, 2), lwd = c(2, 1))