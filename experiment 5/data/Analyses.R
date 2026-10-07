# ---------------------------------------
# Clear workspace
# ---------------------------------------
# Remove all objects from the current R environment so the analysis starts clean.
rm(list = ls(all = TRUE))

# ---------------------------------------
# Load libraries
# ---------------------------------------
# Install these packages once if they are not already available.
# install.packages(c("tidyverse", "pwr", "rstudioapi"))
library(tidyverse)
library(pwr)

# ---------------------------------------
# Set working directory
# ---------------------------------------
# Set the working directory to the folder containing the current R script.
# This allows the script to read all CSV files in that same folder.
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()

# ---------------------------------------
# Read and combine all CSV files
# ---------------------------------------
# Read all CSV files in the working directory and combine them row-wise into one data frame.

d <-
  list.files(pattern = "\\.csv$", full.names = TRUE) %>%
  map_df(~read_csv(.,
                   col_types = cols(subjectID = col_character()),
                   show_col_types = FALSE))


# ---------------------------------------
# Create feature-condition variable
# ---------------------------------------
# Separate single-feature changes into dynamic changes and static color changes.
# consistent: neither disk changes its feature.
# change: the dynamic disk switches between frequency and orientation.
# static: the colored disk changes color.
# inconsistent_2: both disks change their features.
feature_conditions <- c("consistent", "change", "static", "inconsistent_2")
condition_labels <- c(consistent = "Neither changed", change = "Dynamic feature changed",
                      static = "Color changed", inconsistent_2 = "Both changed")
dTest2 <- d %>%
  mutate(
    reactTime = as.numeric(reactTime),
    responseC = as.numeric(responseC),
    change_A = (ball_A_mode_pre == "freq" & ball_A_mode_post == "orient") |
      (ball_A_mode_pre == "orient" & ball_A_mode_post == "freq"),
    change_B = (ball_B_mode_pre == "freq" & ball_B_mode_post == "orient") |
      (ball_B_mode_pre == "orient" & ball_B_mode_post == "freq"),
    FeatCond = case_when(
      FeaturalConsistencyType == "consistent" ~ "consistent",
      FeaturalConsistencyType == "inconsistent_2" ~ "inconsistent_2",
      FeaturalConsistencyType == "inconsistent_1" & (change_A | change_B) ~ "change",
      FeaturalConsistencyType == "inconsistent_1" & !(change_A | change_B) ~ "static",
      TRUE ~ NA_character_
    )
  )

# ---------------------------------------
# Mark correct vs. incorrect responses
# ---------------------------------------
# Mark same and same_swap responses as correct for response 1, and new for response 0.
# Missing responses remain NA rather than being counted as incorrect.
d_correct_incorrect <- dTest2 %>%
  mutate(responseAccuracy = case_when(
    is.na(responseC) ~ NA_character_,
    matchType %in% c("same", "same_swap") & responseC == 1 ~ "True",
    matchType == "new" & responseC == 0 ~ "True",
    matchType %in% c("same", "same_swap", "new") & responseC %in% c(0, 1) ~ "Incorrect",
    TRUE ~ NA_character_
  ))

# ---------------------------------------
# Remove incorrect responses
# ---------------------------------------

dWithoutIncorrect <- d_correct_incorrect %>%
  filter(responseAccuracy == "True")

# ---------------------------------------
# Prepare cleaned RT dataset
# ---------------------------------------

dTest3 <- dWithoutIncorrect %>%
  filter(reactTime <= 40000)

# ---------------------------------------
# Subject-level descriptive statistics
# ---------------------------------------
# Recompute subject-level mean and SD after excluding extremely long RTs.
theGroups <- dTest3 %>%
  group_by(subjectID) %>%
  summarise(avg = mean(reactTime), stDev = sd(reactTime), .groups = "drop")

# ---------------------------------------
# Standard error helper function
# ---------------------------------------
standard_error <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2) return(NA_real_)
  sd(x) / sqrt(length(x))
}

# ---------------------------------------
# Descriptive statistics by condition
# ---------------------------------------
# Compute means, SDs, medians, and standard errors across trials in each condition.
meansTable <- dTest3 %>%
  group_by(FeatCond, matchType) %>%
  summarise(mean = mean(reactTime), sd = sd(reactTime),
            median = median(reactTime), error = standard_error(reactTime),
            .groups = "drop")
print(meansTable)

# ---------------------------------------
# Simple bar plots comparing two conditions
# ---------------------------------------
# Plot Swap and Same separately for each of the four feature conditions.

plot_condition <- function(cond_name) {
  temp <- meansTable %>%
    filter(FeatCond == cond_name, matchType %in% c("same_swap", "same")) %>%
    mutate(matchType = factor(matchType, levels = c("same_swap", "same"))) %>%
    arrange(matchType)
  if (nrow(temp) != 2 || any(!is.finite(temp$error))) return(invisible(NULL))
  values <- temp$mean
  errors <- temp$error
  names <- c("Swap", "Same")
  colors <- c("#808080", "#808080")
  y_min <- min(1000, floor(min(values - errors) / 50) * 50)
  y_max <- ceiling(max(values + errors) / 50) * 50
  if (y_max <= y_min) y_max <- y_min + 50
  par(mar = c(5, 7, 4, 2))
  plot(1, type = "n", xlim = c(0.5, 2.5), ylim = c(y_min, y_max),
       xlab = "Trial Type", ylab = "", xaxt = "n", yaxt = "n", cex.lab = 1.3)
  axis(2, at = seq(y_min, y_max, by = 50), las = 2, cex.axis = 1.2)
  axis(1, at = seq_along(values), labels = names, cex.axis = 1.2)
  mtext("Reaction Time (ms)", side = 2, line = 4.5, cex = 1.3)
  title(main = condition_labels[[cond_name]])
  for (i in seq_along(values)) {
    rect(i - 0.4, y_min, i + 0.4, values[i], col = colors[i])
    arrows(x0 = i, y0 = values[i] - errors[i], x1 = i, y1 = values[i] + errors[i],
           code = 3, angle = 90, length = 0.1)
  }
}
for (g in feature_conditions) plot_condition(g)



# ---------------------------------------
# Accuracy analysis for same vs. same_swap
# ---------------------------------------
# Use both correct and incorrect responses to calculate accuracy.
filtered_data <- d_correct_incorrect %>%
  filter(matchType %in% c("same", "same_swap")) %>%
  mutate(is_correct = responseAccuracy == "True")

# Calculate overall accuracy across trials in each condition.
overall_accuracy <- filtered_data %>%
  group_by(FeatCond, matchType) %>%
  summarise(overall_accuracy_rate = mean(is_correct, na.rm = TRUE), .groups = "drop")
print(overall_accuracy)

# ---------------------------------------
# Subject-level means for inferential tests
# ---------------------------------------
# Compute each participant's mean RT in each condition,
# then reshape to wide format for paired comparisons.
mean_by_subject <- dTest3 %>%
  filter(matchType %in% c("same", "same_swap")) %>%
  group_by(subjectID, FeatCond, matchType) %>%
  summarise(meanRT = mean(reactTime), .groups = "drop") %>%
  pivot_wider(names_from = matchType, values_from = meanRT)

# ---------------------------------------
# Paired t-tests, effect sizes, and power
# ---------------------------------------
# Compare Swap > Same separately within each feature condition.
results_ttest <- list()
effect_sizes <- list()
achieved_power <- list()
sensitivity <- list()
for (g in feature_conditions) {
  # Use complete participant pairs for the test, effect size, and power calculations.
  temp <- mean_by_subject %>%
    filter(FeatCond == g, is.finite(same_swap), is.finite(same))

  results_ttest[[g]] <- t.test(temp$same_swap, temp$same,
                              paired = TRUE, alternative = "greater")

  # Compute difference scores and paired-samples dz.
  diffsm <- temp$same_swap - temp$same
  dzm <- mean(diffsm) / sd(diffsm)
  nm <- length(diffsm)
  effect_sizes[[g]] <- dzm

  # Achieved power calculated from the observed effect size.
  achieved_power[[g]] <- pwr.t.test(n = nm, d = dzm, type = "paired",
                                   sig.level = 0.05, alternative = "greater")

  # Effect size detectable with 80% power at the available sample size.
  sensitivity[[g]] <- pwr.t.test(n = nm, power = 0.80, type = "paired",
                                sig.level = 0.05, alternative = "greater")
}
print(results_ttest)
print(effect_sizes)
print(achieved_power)
print(sensitivity)

