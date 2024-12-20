rm(list=ls(all=TRUE))

getwd()
setwd("/Users/petermazalik/Library/CloudStorage/OneDrive-JohnsHopkins/1 Project Change/exp_scripts/experiment 7 shapes/data")

install.packages("tidyverse")
install.packages("lme4")
install.packages("doBy")


library(tidyverse)
library(lme4)
library(doBy)


d <-
  list.files( pattern = "*.csv", full.names = TRUE) %>% 
  map_df(~read_csv(. , show_col_types = FALSE) )
d



trialTypeString <- strsplit(d$spatiotemporalType, "_")

trialTypeString[[1]][2]

numerOfObservations <- length(d$spatiotemporalType)
 
  
d %>%
  group_by(subjectID)  %>%
  summarize(avg = mean(reactTime)) -> allGroups


  
numberOfParticipants <- length(allGroups$subjectID) 
participants <- allGroups$subjectID   



splitString = function(x,n) {
  x = strsplit(x, "_")[[1]][n]
  return(x)
}

dTest2 <- d %>%
  mutate(trialType = sapply(X = spatiotemporalType, FUN = splitString, n = 2))



dWithoutIncorrect <- dTest2 %>%
  mutate(responseAccuracy = "gavagai")



for( i in 1 : length(dWithoutIncorrect$reactTime)) {
  if (dWithoutIncorrect$matchType[i] == "same" && dWithoutIncorrect$responseC[i] == 1){  
   dWithoutIncorrect$responseAccuracy[i] <- "True"
  } else if (dWithoutIncorrect$matchType[i] == "same_swap" && dWithoutIncorrect$responseC[i] == 1) {
    dWithoutIncorrect$responseAccuracy[i] <- "True"
  } else if (dWithoutIncorrect$matchType[i] == "new" && dWithoutIncorrect$responseC[i] == 0) {
    dWithoutIncorrect$responseAccuracy[i] <- "True"
  }
}


d_correct_incorrect <- dWithoutIncorrect


count_same_swap <- d_correct_incorrect %>%
  filter(matchType == "same_swap" & responseAccuracy == "True") %>%
  nrow()

count_same_ <- d_correct_incorrect %>%
  filter(matchType == "same" & responseAccuracy == "True") %>%
  nrow()

dWithoutIncorrect <- dWithoutIncorrect %>%
  filter(responseAccuracy != "gavagai")


length(which(dTest2$subjectID == participants[3]))
length(which(dWithoutIncorrect$subjectID == participants[3]))


dWithoutIncorrect %>%
  group_by(subjectID)  %>%
  summarize(avg = mean(reactTime)) -> allGroups_without_incorrect

participants_without_incorrect <- allGroups_without_incorrect$subjectID  



dTest3 <- dWithoutIncorrect

dTest3$reactTime <- dTest3$reactTime - 620

mean_dTest3 <- mean(dTest3$reactTime)

sd_dTest3 <- sd(dTest3$reactTime)

dTest3 %>%
  group_by(subjectID) %>% 
  summarise(avg = mean(reactTime), stDev = sd(reactTime)) -> theGroups

dTest3 <- dTest3 %>%
  group_by(subjectID) %>%
  mutate(avg = mean(reactTime), stDev = sd(reactTime))

dTest3 <- dTest3  %>% 
  group_by(subjectID) %>%
  mutate(zscore = (reactTime - mean(reactTime))/sd(reactTime))
 
dTest3 <- dTest3 %>%
 filter(reactTime >= 400) %>%
 filter(reactTime <= 2500)  



dTest3 %>%
  group_by(subjectID) %>% 
  summarise(avg = mean(reactTime), stDev = sd(reactTime)) -> theGroups


standard_error <- function(x) {
  sd(x) / sqrt(length(x))
}


varNewConsistent = filter(dTest3, matchType == "new", trialType == "consistent" )
meanVarNewConsistent = mean(varNewConsistent$reactTime)
sdVarNewConsistent = sd(varNewConsistent$reactTime)
medianVarNewConsistent = median(varNewConsistent$reactTime)
seVarNewConsistent = standard_error(varNewConsistent$reactTime)


varNewInconsistent = filter(dTest3, matchType == "new", trialType == "inconsistent" )
meanVarNewInconsistent = mean(varNewInconsistent$reactTime)
sdVarNewInconsistent = sd(varNewInconsistent$reactTime)
medianVarNewInconsistent = median(varNewInconsistent$reactTime)
seVarNewInconsistent = standard_error(varNewInconsistent$reactTime)


varSameSwapConsistent = filter(dTest3, matchType == "same_swap", trialType == "consistent" )
meanVarSameSwapConsistent = mean(varSameSwapConsistent$reactTime)
sdVarSameSwapConsistent = sd(varSameSwapConsistent$reactTime)
medianVarSameSwapConsistent = median(varSameSwapConsistent$reactTime)
seVarSameSwapConsistent = standard_error(varSameSwapConsistent$reactTime)


varSameSwapInconsistent = filter(dTest3, matchType == "same_swap", trialType == "inconsistent" )
meanVarSameSwapInconsistent = mean(varSameSwapInconsistent$reactTime)
sdVarSameSwapInconsistent = sd(varSameSwapInconsistent$reactTime)
medianVarSameSwapInconsistent = median(varSameSwapInconsistent$reactTime)
seVarSameSwapInconsistent = standard_error(varSameSwapInconsistent$reactTime)


varSameConsistent = filter(dTest3, matchType == "same", trialType == "consistent" )
meanVarSameConsistent = mean(varSameConsistent$reactTime)
sdVarSameConsistent = sd(varSameConsistent$reactTime)
medianVarSameConsistent = median(varSameConsistent$reactTime)
seVarSameConsistent = standard_error(varSameConsistent$reactTime)


varSameInconsistent = filter(dTest3, matchType == "same", trialType == "inconsistent" )
meanVarSameInconsistent = mean(varSameInconsistent$reactTime)
sdVarSameInconsistent = sd(varSameInconsistent$reactTime)
medianVarSameInconsistent = median(varSameInconsistent$reactTime)
seVarSameInconsistent = standard_error(varSameInconsistent$reactTime)


meansTable <- data.frame(name = character(), mean = numeric(), sd = numeric(), median = numeric(), error = numeric())
meansTable <- add_row(meansTable, name = 'New Consistent', mean = meanVarNewConsistent, sd = sdVarNewConsistent, median = medianVarNewConsistent, error = seVarNewConsistent)
meansTable <- add_row(meansTable, name = 'New Inconsistent', mean = meanVarNewInconsistent, sd = sdVarNewInconsistent,median = medianVarNewInconsistent, error = seVarNewInconsistent)
meansTable <- add_row(meansTable, name = 'Same swap consistent', mean = meanVarSameSwapConsistent, sd = sdVarSameSwapConsistent,  median = medianVarSameSwapConsistent ,error = seVarSameSwapConsistent)
meansTable <- add_row(meansTable, name = 'Same Swap inconsistent', mean = meanVarSameSwapInconsistent, sd = sdVarSameSwapInconsistent,  median =  medianVarSameSwapInconsistent,error = seVarSameSwapInconsistent)
meansTable <- add_row(meansTable, name = 'Same consistent', mean = meanVarSameConsistent, sd = sdVarSameConsistent, median =  medianVarSameConsistent ,error = seVarSameConsistent)
meansTable <- add_row(meansTable, name = 'Same inconsistent', mean = meanVarSameInconsistent, sd = sdVarSameInconsistent, median = medianVarSameInconsistent  ,error = seVarSameInconsistent)



t.test(varSameSwapInconsistent$reactTime, varSameInconsistent$reactTime, paired = FALSE, var.equal = FALSE)


values <- c( meanVarSameSwapInconsistent, meanVarSameInconsistent)

errors <- c(seVarSameSwapInconsistent, seVarSameInconsistent)

names <- c( "Swap", "Same")

colors <- c( "#808080", "#808080")

# Base Plot
plot(1, type = "n", xlim = c(0.5, length(values) + 0.5), ylim = c(1300, max(values + 10 + errors)),
     xlab = "Trial Type", ylab = "Reaction Time (ms)",  
     xaxt = "n", yaxt = "n")

# Adding custom axes
axis(2, at = seq(1300, max(values + 50 + errors), by = 50), las = 2) # Y-axis
axis(1, at = 1:length(values), labels = names) # X-axis

# Adding bars
for (i in 1:length(values)) {
  rect(i - 0.4, 1300, i + 0.4, values[i], col = colors[i])
  
  # Adding error bars
  arrows(x0 = i, y0 = values[i] - errors[i], x1 = i, y1 = values[i] + errors[i], 
         code = 3, angle = 90, length = 0.1)
}



#Is distribution skewed

hist(varSameSwapInconsistent$reactTime, breaks = 100)

hist(varSameInconsistent$reactTime, breaks = 100)



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




















