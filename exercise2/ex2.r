install.packages(c("tidyverse", "caret", "e1071", "randomForest", "xgboost", "corrplot", "pROC", "patchwork"))
library(tidyverse)
library(caret)
library(e1071)
library(randomForest)
library(xgboost)
library(corrplot)
library(pROC)

#----------------------------------------------------------------------------------------------

df <- read_csv("heart.csv")
head(df)

num_cols <- c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")
cat_cols <- c("Sex", "ChestPainType", "FastingBS", "RestingECG", "ExerciseAngina", "ST_Slope")

#----------------------------------------------------------------------------------------------

y <- df[, "HeartDisease", drop = FALSE]
X <- df[, !(names(df) %in% "HeartDisease")]

cat("X head:\n")
print(head(X), row.names = FALSE)
cat("\n")

cat("Y head:\n")
print(head(y), row.names = FALSE)

encoders <- list()

for (col in cat_cols) {
  X[[col]] <- as.factor(X[[col]])
  levs <- levels(X[[col]])
  
  encoders[[col]] <- list(
    to_codes = setNames(0:(length(levs) - 1), levs),
    to_labels = setNames(levs, 0:(length(levs) - 1))
  )
  
  X[[col]] <- as.integer(X[[col]]) - 1
}

#----------------------------------------------------------------------------------------------

cat("Missing values per column:\n")
print(colSums(is.na(X)))

cat("\nMissing values per column:\n")
print(colSums(is.na(y)))

outliers <- list()

for (col in num_cols) {
  Q1 <- quantile(X[[col]], 0.25)
  Q3 <- quantile(X[[col]], 0.75)
  IQR <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  
  outliers[[col]] <- X[[col]][X[[col]] < lower | X[[col]] > upper]
}

for (col in num_cols) {
  median_value <- median(X[[col]], na.rm = TRUE)
  
  # Replace zeros
  X[[col]][X[[col]] == 0] <- median_value
  
  # Replace negative values
  X[[col]][X[[col]] < 0] <- median_value
}

#----------------------------------------------------------------------------------------------

#Display people with and without heart disease
heart_dis_count <- sum(y$HeartDisease == 1)
no_heart_dis_count <- sum(y$HeartDisease == 0)

heart_disease <- c("Heart Disease", "No Heart Disease")
count <- c(heart_dis_count, no_heart_dis_count)

barplot(count, names.arg = heart_disease, col = c("#FF6B6B", "#4ECDC4"))

bp <- barplot(count, names.arg = heart_disease, col = c("#FF6B6B", "#4ECDC4"), 
              main = "Heart Disease Count", xlab = "Category", ylab = "Count",
              ylim = c(0, max(count) * 1.1))

text(x = bp, y = count, labels = count, pos = 3, cex = 1.2)

#----------------------------------------------------------------------------------------------

#Plot nummeric attributes
X_hd <- X[y$HeartDisease == 1, ]
X_no_hd <- X[y$HeartDisease == 0, ]

stats_hd <- data.frame(
  Mean = round(sapply(X_hd[num_cols], mean), 2),
  Median = round(sapply(X_hd[num_cols], median), 2),
  Std = round(sapply(X_hd[num_cols], sd), 2)
)

stats_no_hd <- data.frame(
  Mean = round(sapply(X_no_hd[num_cols], mean), 2),
  Median = round(sapply(X_no_hd[num_cols], median), 2),
  Std = round(sapply(X_no_hd[num_cols], sd), 2)
)

num <- length(num_cols)
rows <- ceiling(num / 2)

par(mfrow = c(rows, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))

for (i in seq_along(num_cols)) {
  col <- num_cols[i]
  
  hd_values <- as.numeric(stats_hd[col, ])
  no_hd_values <- as.numeric(stats_no_hd[col, ])
  
  max_val <- max(c(hd_values, no_hd_values))
  
  # Create the grouped barplot
  plot_data <- rbind(hd_values, no_hd_values)
  bp <- barplot(plot_data, beside = TRUE, 
                col = c("#4ECDC4", "#FF6B6B"),
                names.arg = c("Mean", "Median", "Std"),
                main = col, ylab = "Value",
                ylim = c(0, max_val * 1.2))
  
  # Add labels above bars
  text(x = bp, y = plot_data, labels = plot_data, pos = 3, cex = 0.8)
  
  # Legend management
  if (i == 1) {
    legend("topright", legend = c("Heart Disease", "No Heart Disease"), 
           fill = c("#4ECDC4", "#FF6B6B"), bty = "n", cex = 0.8)
  }
}

#----------------------------------------------------------------------------------------------

# Plot categorical attributes
for (col in cat_cols) {
  cat("\n=== ", col, " ===\n")
  print(round(prop.table(table(df$HeartDisease, df[[col]]), 1) * 100, 3))
}

n <- length(cat_cols)
rows <- ceiling(n / 2)
par(mfrow = c(rows, 2), mar = c(4, 4, 3, 1))

for (i in seq_along(cat_cols)) {
  col <- cat_cols[i]
  
  ct <- prop.table(table(df[[col]], df$HeartDisease), margin = 1) * 100
  
  bp <- barplot(t(ct), 
                beside = TRUE, 
                col = c("#4ECDC4", "#FF6B6B"),
                main = paste(col, "distribution by Heart Disease (%)"),
                ylab = "Percentage",
                xlab = col,
                ylim = c(0, 110))
  
  if (i == 1) {
    legend("topright", 
           legend = c("No Heart Disease", "Heart Disease"), 
           fill = c("#4ECDC4", "#FF6B6B"), 
           bty = "n", 
           cex = 0.8)
  }
}

#----------------------------------------------------------------------------------------------
#Plot correlatiosn between attributes

plot_correlation_heatmap <- function(data, columns, title) {
  corr_matrix <- round(cor(data[, columns], use = "complete.obs"), 2)
  
  col_palette <- colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200)
  
  heatmap(corr_matrix, 
          Rowv = NA, Colv = NA, 
          col = col_palette, 
          scale = "none", 
          margins = c(10, 10),
          main = title,
          add.expr = text(row(corr_matrix), col(corr_matrix), labels = corr_matrix))
}

# All data
plot_correlation_heatmap(df, c(num_cols, "HeartDisease"), "Correlation Heatmap (All Data)")

# Heart Disease=1
df_hd <- df[df$HeartDisease == 1, ]
plot_correlation_heatmap(df_hd, num_cols, "Correlation Heatmap (Heart Disease = 1)")

# Heart Disease=0
df_no <- df[df$HeartDisease == 0, ]
plot_correlation_heatmap(df_no, num_cols, "Correlation Heatmap (Heart Disease = 0)")

#----------------------------------------------------------------------------------------------

#Preprocessing class, it scales data and creates training/test datasets
DataPreprocessor <- setRefClass(
  "DataPreprocessor",
  fields = list(
    test_size = "numeric",
    random_state = "numeric",
    shuffle = "logical",
    stratify = "logical",
    scaler_center = "numeric",
    scaler_scale = "numeric",
    X_train = "data.frame",
    X_test = "data.frame",
    y_train = "numeric",
    y_test = "numeric"
  ),
  methods = list(
    initialize = function(test_size = 0.25, random_state = 42, shuffle = TRUE, stratify = TRUE) {
      test_size <<- test_size
      random_state <<- random_state
      shuffle <<- shuffle
      stratify <<- stratify
    },
    
    prepare_data = function(X, y, num_cols, scaling_type = "standard") {
      set.seed(random_state)
      y_vec <- as.numeric(as.matrix(y))
      
      n <- nrow(X)
      indices <- 1:n
      
      if (stratify) {
        train_idx <- caret::createDataPartition(y_vec, p = 1 - test_size, list = FALSE)
      } else {
        if (shuffle) indices <- sample(indices)
        train_idx <- indices[1:round((1 - test_size) * n)]
      }
      
      X_train <<- X[train_idx, ]
      X_test <<- X[-train_idx, ]
      y_train <<- y_vec[train_idx]
      y_test <<- y_vec[-train_idx]
      
      if (scaling_type == "standard") {
        train_subset <- X_train[, num_cols]
        scaled_data <- scale(train_subset)
        
        scaler_center <<- attr(scaled_data, "scaled:center")
        scaler_scale <<- attr(scaled_data, "scaled:scale")
        
        X_train[, num_cols] <<- as.data.frame(scaled_data)
        X_test[, num_cols] <<- as.data.frame(scale(X_test[, num_cols], 
                                                  center = scaler_center, 
                                                  scale = scaler_scale))
      }
      
      return(list(X_train, X_test, y_train, y_test))
    }
  )
)

dp <- DataPreprocessor$new(test_size = 0.25, random_state = 42)
result <- dp$prepare_data(X, y, num_cols = num_cols, scaling_type = "standard")
X_train <- result[[1]]
X_test <- result[[2]]
y_train <- result[[3]]
y_test <- result[[4]]

#----------------------------------------------------------------------------------------------

#Evaluator class, takes model as input along with the X and y, trains teh model, produces results and plots the confusion matrix along AUC
ModelEvaluator <- setRefClass(
  "ModelEvaluator",
  fields = list(
    X_train = "data.frame",
    y_train = "numeric",
    X_test = "data.frame",
    y_test = "numeric",
    y_pred = "numeric",
    y_prob = "numeric",
    model = "ANY",
    labels = "character"
  ),
  methods = list(
    initialize = function(X_train, y_train, X_test, y_test, labels = NULL) {
      X_train <<- X_train
      y_train <<- y_train
      X_test <<- X_test
      y_test <<- y_test
      labels <<- if (is.null(labels)) c("Class 0", "Class 1") else labels
    },

    train_model = function(model_obj) {
      model <<- model_obj
    },

    predict_results = function() {
      if (is.null(model)) stop("Model has not been trained yet.")
      
      if (inherits(model, "naiveBayes")) {
        y_pred <<- as.numeric(as.character(predict(model, X_test)))
        y_prob <<- predict(model, X_test, type = "raw")[, 2]
      } else if (inherits(model, "randomForest")) {
        y_pred <<- as.numeric(as.character(predict(model, X_test)))
        y_prob <<- predict(model, X_test, type = "prob")[, 2]
      } else if (inherits(model, "xgb.Booster")) {
        y_prob <<- predict(model, as.matrix(X_test))
        y_pred <<- as.numeric(y_prob > 0.5)
      } else {
        y_prob <<- predict(model, X_test, type = "response")
        y_pred <<- as.numeric(y_prob > 0.5)
      }
    },

    print_report = function() {
      cat("=== Classification Report ===\n")
      actual <- factor(y_test, levels = c(0, 1), labels = labels)
      predicted <- factor(y_pred, levels = c(0, 1), labels = labels)
      cm <- caret::confusionMatrix(predicted, actual, mode = "everything", positive = labels[2])
      print(cm)
    },

    plot_roc_curve = function() {
      roc_obj <- pROC::roc(y_test, y_prob, quiet = TRUE)
      auc_val <- pROC::auc(roc_obj)
      plot(roc_obj, col = "#4ECDC4", lwd = 3, main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"))
      abline(a = 0, b = 1, lty = 2, col = "grey")
    },

    run_full_pipeline = function(model_object) {
      train_model(model_object)
      predict_results()
      print_report()
      
      # Set layout to 2 rows, 1 column for vertical stacking
      par(mfrow = c(2, 1)) 
      
      # Plot 1: Confusion Matrix (Top)
      actual <- factor(y_test, levels = c(0, 1), labels = labels)
      predicted <- factor(y_pred, levels = c(0, 1), labels = labels)
      fourfoldplot(table(Actual = actual, Predicted = predicted), 
                   color = c("#CC6666", "#99CC99"), 
                   conf.level = 0, 
                   margin = 1, 
                   main = "Confusion Matrix")
      
      # Plot 2: ROC Curve (Bottom)
      plot_roc_curve()
      
      # Reset back to single plot layout
      par(mfrow = c(1, 1))
    }
  )
)
#---------------------------------------------------------------------------------------------

#Naive bayes
nb_model <- e1071::naiveBayes(X_train, as.factor(y_train))
eval_nb <- ModelEvaluator$new(X_train, y_train, X_test, y_test, labels = c("No HD", "HD"))
eval_nb$run_full_pipeline(nb_model)

#----------------------------------------------------------------------------------------------

#Random forest
rf_model <- randomForest::randomForest(
  x = X_train,
  y = as.factor(y_train),
  ntree = 500,
  maxnodes = 2^10,
  nodesize = 2,
  importance = TRUE
)

eval_rf <- ModelEvaluator$new(X_train, y_train, X_test, y_test, labels = c("No HD", "HD"))

eval_rf$run_full_pipeline(rf_model)

#----------------------------------------------------------------------------------------------

#xGBoost
dtrain <- xgboost::xgb.DMatrix(data = as.matrix(X_train), label = y_train)

xgb_model <- xgboost::xgboost(
    data = dtrain,
    nrounds = 100,
    objective = "binary:logistic",
    verbose = 0
)

eval_xgb <- ModelEvaluator$new(X_train, y_train, X_test, y_test, labels = c("No HD", "HD"))

eval_xgb$run_full_pipeline(xgb_model)