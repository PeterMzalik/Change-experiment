# ---------------------------------------
# Clear workspace
# ---------------------------------------
# Remove all objects from the current R environment so the analysis starts clean.
rm(list=ls(all=TRUE))



# ---------------------------------------
# Install required packages
# ---------------------------------------
install.packages("tidyverse")
install.packages("lme4")
install.packages("doBy")
install.packages("effsize")
install.packages("pwr")
install.packages("here")  

# ---------------------------------------
# Load libraries
# ---------------------------------------
library(tidyverse)
library(lme4)
library(doBy)
library(effsize)
library(pwr)
library(here)    


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
  list.files( pattern = "*.csv", full.names = TRUE) %>% 
  map_df(~read_csv(. , show_col_types = FALSE) )
d


# ---------------------------------------
# Parse trial-type strings
# ---------------------------------------
# Split the spatiotemporalType string on underscores.
trialTypeString <- strsplit(d$spatiotemporalType, "_")

# Inspect the second component of the first entry.
trialTypeString[[1]][2]


 
# ---------------------------------------
# Participant-level mean RTs
# ---------------------------------------
# Compute the average reaction time for each participant.  
d %>%
  group_by(subjectID)  %>%
  summarize(avg = mean(reactTime)) -> allGroups




# ---------------------------------------
# Helper function for extracting string components
# ---------------------------------------
# Returns the nth element after splitting a string by underscores.
splitString = function(x,n) {
  x = strsplit(x, "_")[[1]][n]
  return(x)
}

# ---------------------------------------
# Create trialType variable
# ---------------------------------------
# Extract the second component of spatiotemporalType
# (e.g., "consistent" or "inconsistent").
dTest2 <- d %>%
  mutate(trialType = sapply(X = spatiotemporalType, FUN = splitString, n = 2))


# ---------------------------------------
# Mark correct vs. incorrect responses
# ---------------------------------------
# Start by assigning a placeholder label to all responses.
dWithoutIncorrect <- dTest2 %>%
  mutate(responseAccuracy = "Incorrect")

for(i in 1:length(dWithoutIncorrect$reactTime)) {
  if (dWithoutIncorrect$matchType[i] == "same" && dWithoutIncorrect$responseC[i] == 1){  
    dWithoutIncorrect$responseAccuracy[i] <- "True"
  } else if (dWithoutIncorrect$matchType[i] == "same_swap" && dWithoutIncorrect$responseC[i] == 1) {
    dWithoutIncorrect$responseAccuracy[i] <- "True"
  } else if (dWithoutIncorrect$matchType[i] == "new" && dWithoutIncorrect$responseC[i] == 0) {
    dWithoutIncorrect$responseAccuracy[i] <- "True"
  }
}

# Save a version that still contains both correct and incorrect responses.
d_correct_incorrect <- dWithoutIncorrect

# ---------------------------------------
# Remove incorrect responses
# ---------------------------------------
# Keep only trials that were marked correct.
dWithoutIncorrect <- dWithoutIncorrect %>%
  filter(responseAccuracy == "True")



# ---------------------------------------
# Prepare cleaned RT dataset
# ---------------------------------------

dTest3 <- dWithoutIncorrect

# Subtract 620 ms from all RTs, so that reaction time is relative to test Onset
dTest3$reactTime <- dTest3$reactTime - 620


# Remove extremely long RTs. 
dTest3 <- dTest3 %>%
  filter(reactTime <= 40000)  

# ---------------------------------------
# Subject-level descriptive statistics
# ---------------------------------------


# Recompute subject-level mean and SD after trimming.
dTest3 %>%
  group_by(subjectID) %>% 
  summarise(avg = mean(reactTime), stDev = sd(reactTime)) -> theGroups


# ---------------------------------------
# Standard error helper function
# ---------------------------------------
standard_error <- function(x) {
  sd(x) / sqrt(length(x))
}

# ---------------------------------------
# Descriptive statistics by condition
# ---------------------------------------

# New / consistent
varNewConsistent = filter(dTest3, matchType == "new", trialType == "consistent" )
meanVarNewConsistent = mean(varNewConsistent$reactTime)
sdVarNewConsistent = sd(varNewConsistent$reactTime)
medianVarNewConsistent = median(varNewConsistent$reactTime)
seVarNewConsistent = standard_error(varNewConsistent$reactTime)

# New / inconsistent
varNewInconsistent = filter(dTest3, matchType == "new", trialType == "inconsistent" )
meanVarNewInconsistent = mean(varNewInconsistent$reactTime)
sdVarNewInconsistent = sd(varNewInconsistent$reactTime)
medianVarNewInconsistent = median(varNewInconsistent$reactTime)
seVarNewInconsistent = standard_error(varNewInconsistent$reactTime)

# Same-swap / consistent
varSameSwapConsistent = filter(dTest3, matchType == "same_swap", trialType == "consistent" )
meanVarSameSwapConsistent = mean(varSameSwapConsistent$reactTime)
sdVarSameSwapConsistent = sd(varSameSwapConsistent$reactTime)
medianVarSameSwapConsistent = median(varSameSwapConsistent$reactTime)
seVarSameSwapConsistent = standard_error(varSameSwapConsistent$reactTime)

# Same-swap / inconsistent
varSameSwapInconsistent = filter(dTest3, matchType == "same_swap", trialType == "inconsistent" )
meanVarSameSwapInconsistent = mean(varSameSwapInconsistent$reactTime)
sdVarSameSwapInconsistent = sd(varSameSwapInconsistent$reactTime)
medianVarSameSwapInconsistent = median(varSameSwapInconsistent$reactTime)
seVarSameSwapInconsistent = standard_error(varSameSwapInconsistent$reactTime)

# Same / consistent
varSameConsistent = filter(dTest3, matchType == "same", trialType == "consistent" )
meanVarSameConsistent = mean(varSameConsistent$reactTime)
sdVarSameConsistent = sd(varSameConsistent$reactTime)
medianVarSameConsistent = median(varSameConsistent$reactTime)
seVarSameConsistent = standard_error(varSameConsistent$reactTime)

# Same / inconsistent
varSameInconsistent = filter(dTest3, matchType == "same", trialType == "inconsistent" )
meanVarSameInconsistent = mean(varSameInconsistent$reactTime)
sdVarSameInconsistent = sd(varSameInconsistent$reactTime)
medianVarSameInconsistent = median(varSameInconsistent$reactTime)
seVarSameInconsistent = standard_error(varSameInconsistent$reactTime)


# ---------------------------------------
# Table of condition means
# ---------------------------------------
meansTable <- data.frame(name = character(), mean = numeric(), sd = numeric(), median = numeric(), error = numeric())
meansTable <- add_row(meansTable, name = 'New Consistent', mean = meanVarNewConsistent, sd = sdVarNewConsistent, median = medianVarNewConsistent, error = seVarNewConsistent)
meansTable <- add_row(meansTable, name = 'New Inconsistent', mean = meanVarNewInconsistent, sd = sdVarNewInconsistent,median = medianVarNewInconsistent, error = seVarNewInconsistent)
meansTable <- add_row(meansTable, name = 'Same swap consistent', mean = meanVarSameSwapConsistent, sd = sdVarSameSwapConsistent,  median = medianVarSameSwapConsistent ,error = seVarSameSwapConsistent)
meansTable <- add_row(meansTable, name = 'Same Swap inconsistent', mean = meanVarSameSwapInconsistent, sd = sdVarSameSwapInconsistent,  median =  medianVarSameSwapInconsistent,error = seVarSameSwapInconsistent)
meansTable <- add_row(meansTable, name = 'Same consistent', mean = meanVarSameConsistent, sd = sdVarSameConsistent, median =  medianVarSameConsistent ,error = seVarSameConsistent)
meansTable <- add_row(meansTable, name = 'Same inconsistent', mean = meanVarSameInconsistent, sd = sdVarSameInconsistent, median = medianVarSameInconsistent  ,error = seVarSameInconsistent)



# ---------------------------------------
# Simple bar plot comparing two conditions
# ---------------------------------------

values <- c(meanVarSameSwapInconsistent, meanVarSameInconsistent)
errors <- c(seVarSameSwapInconsistent, seVarSameInconsistent)
names <- c("Swap", "Same")
colors <- c("#808080", "#808080")

# Round the upper limit up to the next 50 ms.
y_max <- ceiling(max(values + errors) / 50) * 50

# Base plot
par(mar = c(5, 7, 4, 2))

plot(1, type = "n",
     xlim = c(0.5, length(values) + 0.5),
     ylim = c(1000, y_max),
     xlab = "Trial Type", ylab = "",
     xaxt = "n", yaxt = "n",
     cex.lab = 1.3)

axis(2, at = seq(1000, y_max, by = 50),
     las = 2, cex.axis = 1.2)

axis(1, at = seq_along(values), labels = names,
     cex.axis = 1.2)

mtext("Reaction Time (ms)", side = 2, line = 4.5, cex = 1.3)

# Adding bars and error bars
for (i in seq_along(values)) {
  rect(i - 0.4, 1000, i + 0.4, values[i], col = colors[i])
  
  arrows(x0 = i, y0 = values[i] - errors[i],
         x1 = i, y1 = values[i] + errors[i],
         code = 3, angle = 90, length = 0.1)
}

# ---------------------------------------
# Inspect distributions
# ---------------------------------------
# Histograms used to inspect skew for the two target conditions.

hist(varSameSwapInconsistent$reactTime, breaks = 100)

hist(varSameInconsistent$reactTime, breaks = 100)


# ---------------------------------------
# Accuracy analysis for same vs. same_swap
# ---------------------------------------

# Filter for relevant trial types
filtered_data <- d_correct_incorrect %>%
  filter(matchType %in% c("same", "same_swap"))

# Create a binary column for correctness
filtered_data <- filtered_data %>%
  mutate(is_correct = responseAccuracy == "True")  # TRUE for correct responses

# Summarize accuracy by trial type and subject
accuracy_by_matchType <- filtered_data %>%
  group_by(subjectID, matchType) %>%
  summarize(
    accuracy_rate = mean(is_correct, na.rm = TRUE),  # Calculate accuracy rate
    .groups = "drop"
  )

# Display the results
print(accuracy_by_matchType)

# Optional: Calculate overall accuracy per trial type (across all subjects)
overall_accuracy <- filtered_data %>%
  group_by(matchType) %>%
  summarize(
    overall_accuracy_rate = mean(is_correct, na.rm = TRUE),
    .groups = "drop"
  )

# Display overall accuracy
print(overall_accuracy)



# ---------------------------------------
# Subject-level means for inferential tests
# ---------------------------------------
# Compute each participant's mean RT in each condition,
# then reshape to wide format for paired comparisons.
mean_by_subject <- dTest3 %>%
  group_by(subjectID, matchType, trialType) %>%
  summarize(meanRT = mean(reactTime), .groups = "drop") %>%
  unite(condition, matchType, trialType) %>%  # e.g. "same_inconsistent"
  pivot_wider(names_from = condition, values_from = meanRT)

# ---------------------------------------
# Paired t-test
# ---------------------------------------
# Compare same_swap_inconsistent vs same_inconsistent with a one-sided paired test.
t.test(mean_by_subject$same_swap_inconsistent, mean_by_subject$same_inconsistent,  paired = TRUE, alternative = "greater" )




# ---------------------------------------
# Paired-samples effect size and power
# ---------------------------------------
# Compute difference scores, paired-samples dz, and achieved power.
diffsm <- mean_by_subject$same_swap_inconsistent - mean_by_subject$same_inconsistent

dzm <- mean(diffsm) / sd(diffsm)

nm <- length(diffsm)

power_paired_m <- pwr.t.test(n = nm, d = dzm, type = "paired", sig.level = 0.05, alternative = "greater")
print(power_paired_m)


# Effect size detectable with 80% power at the available sample size.
sensitivity <- pwr.t.test(
  n = nm,
  power = 0.80,
  type = "paired",
  sig.level = 0.05,
  alternative = "greater"
)

print(sensitivity)


