# Plotting functions for stock crossover classification models
# These functions work for both upside and downside classification

#' Plot ROC Curve
#'
#' @param y_true True labels (0 or 1)
#' @param y_pred_prob Predicted probabilities
#' @param title Plot title
#' @param color Main color for the curve
#' @param polygon_color Color for the area under curve
plot_roc_curve <- function(y_true, y_pred_prob, title, color = "darkgreen", 
                           polygon_color = rgb(0, 0.8, 0, 0.2)) {
  if (requireNamespace("pROC", quietly = TRUE)) {
    roc_obj <- pROC::roc(y_true, y_pred_prob, quiet = TRUE)
    
    plot(roc_obj,
         main = title,
         col = color,
         lwd = 3,
         print.auc = TRUE,
         auc.polygon = TRUE,
         auc.polygon.col = polygon_color)
    
    abline(a = 0, b = 1, lty = 2, col = "gray50")
  } else {
    cat("Install pROC package for ROC curves: install.packages('pROC')\n")
  }
}

#' Plot Confusion Matrix Heatmap
#'
#' @param metrics List containing tp, tn, fp, fn metrics
#' @param label_negative Label for negative class (e.g., "No Upside")
#' @param label_positive Label for positive class (e.g., "Upside")
plot_confusion_matrix <- function(metrics, label_negative, label_positive) {
  conf_matrix <- matrix(c(metrics$tn, metrics$fp,
                          metrics$fn, metrics$tp),
                        nrow = 2, byrow = TRUE)
  par(mar = c(5, 5, 4, 2))
  
  # Create color matrix: green for correct predictions, red for errors
  color_matrix <- matrix(c("lightgreen", "lightcoral",
                           "lightcoral", "lightgreen"),
                         nrow = 2, byrow = TRUE)
  
  plot(1, type = "n", xlim = c(0.5, 2.5), ylim = c(0.5, 2.5),
       xlab = "Predicted", ylab = "Actual",
       main = "Confusion Matrix",
       xaxt = "n", yaxt = "n")
  axis(1, at = 1:2, labels = c(label_negative, label_positive))
  axis(2, at = 1:2, labels = c(label_negative, label_positive))
  
  # Draw colored rectangles
  rect(0.5, 0.5, 1.5, 1.5, col = color_matrix[1, 1], border = "black", lwd = 2)
  rect(1.5, 0.5, 2.5, 1.5, col = color_matrix[1, 2], border = "black", lwd = 2)
  rect(0.5, 1.5, 1.5, 2.5, col = color_matrix[2, 1], border = "black", lwd = 2)
  rect(1.5, 1.5, 2.5, 2.5, col = color_matrix[2, 2], border = "black", lwd = 2)
  
  # Add text values
  text(1, 1, conf_matrix[1, 1], cex = 2.5, font = 2)
  text(2, 1, conf_matrix[1, 2], cex = 2.5, font = 2)
  text(1, 2, conf_matrix[2, 1], cex = 2.5, font = 2)
  text(2, 2, conf_matrix[2, 2], cex = 2.5, font = 2)
  
  # Add labels inside cells
  text(1, 0.7, "TN", cex = 1, col = "darkgreen", font = 3)
  text(2, 0.7, "FP", cex = 1, col = "darkred", font = 3)
  text(1, 1.7, "FN", cex = 1, col = "darkred", font = 3)
  text(2, 1.7, "TP", cex = 1, col = "darkgreen", font = 3)
}

#' Plot Probability Distribution by Class
#'
#' @param y_pred_prob Predicted probabilities
#' @param y_true True labels (0 or 1)
#' @param decision_threshold Decision threshold value
#' @param title Plot title
#' @param label_negative Label for negative class
#' @param label_positive Label for positive class
#' @param positive_color Color for positive class histogram
plot_probability_distribution <- function(y_pred_prob, y_true, decision_threshold,
                                         title, label_negative, label_positive,
                                         positive_color = rgb(0, 0.8, 0, 0.6)) {
  par(mar = c(5, 4, 4, 2))
  hist(y_pred_prob[y_true == 0], breaks = 30,
       col = rgb(0.7, 0.7, 0.7, 0.6),
       main = title,
       xlab = "Predicted Probability",
       ylab = "Frequency",
       xlim = c(0, 1))
  hist(y_pred_prob[y_true == 1], breaks = 30,
       col = positive_color, add = TRUE)
  legend("topright",
         legend = c(sprintf("%s (n=%d)", label_negative, sum(y_true == 0)),
                    sprintf("%s (n=%d)", label_positive, sum(y_true == 1))),
         fill = c(rgb(0.7, 0.7, 0.7, 0.6), positive_color),
         cex = 1.2)
  abline(v = decision_threshold, col = "black", lwd = 2, lty = 2)
  text(decision_threshold, par("usr")[4] * 0.95,
       sprintf("Threshold = %.2f", decision_threshold), pos = 4, cex = 1.2)
  grid()
}

#' Plot Precision-Recall Curve
#'
#' @param y_pred_prob Predicted probabilities
#' @param y_true True labels (0 or 1)
#' @param color Curve color
plot_precision_recall <- function(y_pred_prob, y_true, color = "darkgreen") {
  if (requireNamespace("PRROC", quietly = TRUE)) {
    pr_obj <- PRROC::pr.curve(scores.class0 = y_pred_prob[y_true == 1],
                              scores.class1 = y_pred_prob[y_true == 0],
                              curve = TRUE)
    plot(pr_obj, main = sprintf("Precision-Recall Curve (AUC = %.4f)", pr_obj$auc.integral),
         col = color, lwd = 3, auc.main = FALSE)
    grid()
  } else {
    cat("Install PRROC package for PR curves: install.packages('PRROC')\n")
  }
}

#' Plot Probability vs Actual Returns
#'
#' @param actual_y Actual return values
#' @param y_pred_prob Predicted probabilities
#' @param y_true True labels (0 or 1)
#' @param threshold Classification threshold value
#' @param decision_threshold Decision threshold value
#' @param title Plot title
#' @param ylab Y-axis label
#' @param label_positive Label for positive cases
#' @param label_negative Label for negative cases
#' @param threshold_label Label for threshold
#' @param positive_color Color for positive cases
#' @param threshold_color Color for threshold line
plot_probability_vs_actual <- function(actual_y, y_pred_prob, y_true, 
                                       threshold, decision_threshold,
                                       title, ylab, label_positive, label_negative,
                                       threshold_label, positive_color = rgb(0, 0.8, 0, 0.7),
                                       threshold_color = "darkgreen") {
  par(mar = c(5, 4, 4, 8), xpd = TRUE)
  plot(actual_y, y_pred_prob,
       main = title,
       xlab = "Actual y",
       ylab = ylab,
       pch = 16,
       col = ifelse(y_true == 1, positive_color, rgb(0.7, 0.7, 0.7, 0.4)),
       cex = 1.2)
  abline(v = threshold, col = threshold_color, lwd = 3, lty = 2)
  abline(h = decision_threshold, col = "black", lwd = 2, lty = 1)
  legend(par("usr")[2], par("usr")[4],
         legend = c(label_positive, label_negative,
                    sprintf("%s (%.2f)", threshold_label, threshold),
                    sprintf("Decision Threshold (%.2f)", decision_threshold)),
         col = c(positive_color, rgb(0.7, 0.7, 0.7, 0.4), threshold_color, "black"),
         pch = c(16, 16, NA, NA),
         lty = c(NA, NA, 2, 1),
         lwd = c(NA, NA, 3, 2),
         cex = 0.9,
         xjust = 0,
         yjust = 1)
  grid()
  par(xpd = FALSE)
}

#' Plot Density of Signal Returns
#'
#' @param signals_y Signal return values
#' @param threshold Classification threshold value
#' @param title Plot title
#' @param threshold_label Label for threshold
#' @param color Main curve color
#' @param polygon_color Fill color for area under curve
#' @param threshold_color Threshold line color
plot_signal_density <- function(signals_y, threshold, title, threshold_label,
                               color = "darkgreen", 
                               polygon_color = rgb(0, 0.8, 0, 0.3),
                               threshold_color = "darkgreen") {
  if (length(signals_y) < 3) {
    cat("Need at least 3 signals to plot density\n")
    return(invisible(NULL))
  }
  
  par(mar = c(5, 4, 4, 2))
  
  # Calculate density
  density_obj <- density(signals_y, adjust = 1.5)
  
  # Create the plot
  plot(density_obj,
       main = title,
       xlab = "Actual Return (y)",
       ylab = "Density",
       col = color,
       lwd = 3,
       las = 1)
  
  # Fill area under curve
  polygon(density_obj, col = polygon_color, border = NA)
  
  # Add vertical line at threshold
  abline(v = threshold, col = threshold_color, lwd = 2, lty = 2)
  
  # Add vertical line at median
  abline(v = median(signals_y), col = "darkblue", lwd = 2, lty = 3)
  
  # Add legend
  legend("topright",
         legend = c(sprintf("%s (%.2f)", threshold_label, threshold),
                    sprintf("Median Return (%.2f)", median(signals_y)),
                    sprintf("Mean Return (%.2f)", mean(signals_y)),
                    sprintf("SD = %.2f", sd(signals_y))),
         col = c(threshold_color, "darkblue", NA, NA),
         lty = c(2, 3, NA, NA),
         lwd = c(2, 2, NA, NA),
         cex = 0.9,
         bg = "white")
  
  # Add grid
  grid()
  
  cat(sprintf("\nSignals Return Statistics:
  Mean: %.2f
  Median: %.2f
  SD: %.2f
  Min: %.2f
  Max: %.2f
", mean(signals_y), median(signals_y), sd(signals_y), min(signals_y), max(signals_y)))
}
