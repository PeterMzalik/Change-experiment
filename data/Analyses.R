rm(list=ls(all=TRUE))

getwd()
setwd("/Users/petermazalik/Library/CloudStorage/OneDrive-JohnsHopkins/1 Project Change/exp_scripts/experiment 3 shapes/data")

#install.packages("tidyverse")
#install.packages("lme4")
#install.packages("doBy")


library(tidyverse)
library(lme4)
library(doBy)




#d = read_csv(("all data.csv"),
#    col_types = cols(
#  spatiotemporalType = col_character(), matchType = col_character(), responseC = col_double(), reactTime = col_double() 
#), na = "NA"
#)



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


#if matchType = same or sameswap then responseC should be 1
#if matchType = new then responseC should be 0

#&& dWithoutIncorrect$responseC[i] == 1



for( i in 1 : length(dWithoutIncorrect$reactTime)) {
  if (dWithoutIncorrect$matchType[i] == "same" && dWithoutIncorrect$responseC[i] == 1){  
   dWithoutIncorrect$responseAccuracy[i] <- "True"
  } else if (dWithoutIncorrect$matchType[i] == "same_swap" && dWithoutIncorrect$responseC[i] == 1) {
    dWithoutIncorrect$responseAccuracy[i] <- "True"
  } else if (dWithoutIncorrect$matchType[i] == "new" && dWithoutIncorrect$responseC[i] == 0) {
    dWithoutIncorrect$responseAccuracy[i] <- "True"
  }
}

#number of observations for a participant (this is dTest2), 
#eliminate false ones (this is dWithoutIncorrect), 
#if remaining number < initial number *.08, remove participant

d_correct_incorrect <- dWithoutIncorrect



count_same_swap <- d_correct_incorrect %>%
  filter(matchType == "same_swap" & responseAccuracy == "True") %>%
  nrow()

count_same <- d_correct_incorrect %>%
  filter(matchType == "same" & responseAccuracy == "True") %>%
  nrow()

total_trials <- d %>%
  nrow()

count_same_swap_accuracy <- count_same_swap / (total_trials / 3)
count_same_accuracy <- count_same / (total_trials / 3)

dWithoutIncorrect <- dWithoutIncorrect %>%
  filter(responseAccuracy != "gavagai")


#length(which(dTest2$subjectID == participants[3]))
#length(which(dWithoutIncorrect$subjectID == participants[3]))





#for (i in 1 : length(participants)){
#  print( length(which(dWithoutIncorrect$subjectID == participants[i]))  < length(which(dTest2$subjectID == participants[i]))  * 0.8 )
#}

#length(which(dTest2$subjectID == participants[1]))
#length(which(dWithoutIncorrect$subjectID == participants[1]))


dWithoutIncorrect %>%
  group_by(subjectID)  %>%
  summarize(avg = mean(reactTime)) -> allGroups_without_incorrect

participants_without_incorrect <- allGroups_without_incorrect$subjectID  



dTest3 <- dWithoutIncorrect

dTest3$reactTime <- dTest3$reactTime - 260

mean_dTest3 <- mean(dTest3$reactTime)

sd_dTest3 <- sd(dTest3$reactTime)

dTest3 %>%
  group_by(subjectID) %>% 
  summarise(avg = mean(reactTime), stDev = sd(reactTime)) -> theGroups

dTest3 <- dTest3 %>%
  group_by(subjectID) %>%
  mutate(avg = mean(reactTime), stDev = sd(reactTime))
 
  
dTest3 <- dTest3 %>%
  filter(reactTime >= 400) %>%
  filter(reactTime <= 2500)  

   
#dTest3 <- dTest3 %>%
 # filter(reactTime >= avg - (2.5 * stDev)) %>%
 # filter(reactTime <= avg + (2.5 * stDev))






#filter(dTest3, subjectID != participants[1]) -> dTest3
#filter(dTest3, subjectID != participants[9]) -> dTest3

dTest3 %>%
  group_by(subjectID) %>% 
  summarise(avg = mean(reactTime), stDev = sd(reactTime)) -> theGroups

standard_error <- function(x) {
  sd(x) / sqrt(length(x))
}


varNewConsistent = filter(dTest3, matchType == "new", trialType == "consistent" )
meanVarNewConsistent = mean(varNewConsistent$reactTime)
sdVarNewConsistent = sd(varNewConsistent$reactTime)
seVarNewConsistent = standard_error(varNewConsistent$reactTime)
#errorNmBl0 = qt(0.95 + (1 - 0.95)/2, df = length(varNmBl0$subject) - 1) * sd(varNmBl0$responseTime)/sqrt(length(varNmBl0$subject))

varNewInconsistent = filter(dTest3, matchType == "new", trialType == "inconsistent" )
meanVarNewInconsistent = mean(varNewInconsistent$reactTime)
sdVarNewInconsistent = sd(varNewInconsistent$reactTime)
seVarNewInconsistent = standard_error(varNewInconsistent$reactTime)


varSameSwapConsistent = filter(dTest3, matchType == "same_swap", trialType == "consistent" )
meanVarSameSwapConsistent = mean(varSameSwapConsistent$reactTime)
sdVarSameSwapConsistent = sd(varSameSwapConsistent$reactTime)
seVarSameSwapConsistent = standard_error(varSameSwapConsistent$reactTime)

varSameSwapInconsistent = filter(dTest3, matchType == "same_swap", trialType == "inconsistent" )
meanVarSameSwapInconsistent = mean(varSameSwapInconsistent$reactTime)
sdVarSameSwapInconsistent = sd(varSameSwapInconsistent$reactTime)
seVarSameSwapInconsistent = standard_error(varSameSwapInconsistent$reactTime)


varSameConsistent = filter(dTest3, matchType == "same", trialType == "consistent" )
meanVarSameConsistent = mean(varSameConsistent$reactTime)
sdVarSameConsistent = sd(varSameConsistent$reactTime)
seVarSameConsistent = standard_error(varSameConsistent$reactTime)


varSameInconsistent = filter(dTest3, matchType == "same", trialType == "inconsistent" )
meanVarSameInconsistent = mean(varSameInconsistent$reactTime)
sdVarSameInconsistent = sd(varSameInconsistent$reactTime)
seVarSameInconsistent = standard_error(varSameInconsistent$reactTime)


meansTable <- data.frame(name = character(), mean = numeric(), sd = numeric(), error = numeric())
meansTable <- add_row(meansTable, name = 'New Consistent', mean = meanVarNewConsistent, sd = sdVarNewConsistent, error = seVarNewConsistent)
meansTable <- add_row(meansTable, name = 'New Inconsistent', mean = meanVarNewInconsistent, sd = sdVarNewInconsistent, error = seVarNewInconsistent)
meansTable <- add_row(meansTable, name = 'Same swap consistent', mean = meanVarSameSwapConsistent, sd = sdVarSameSwapConsistent, error = seVarSameSwapConsistent)
meansTable <- add_row(meansTable, name = 'Same Swap inconsistent', mean = meanVarSameSwapInconsistent, sd = sdVarSameSwapInconsistent, error = seVarSameSwapInconsistent)
meansTable <- add_row(meansTable, name = 'Same consistent', mean = meanVarSameConsistent, sd = sdVarSameConsistent, error = seVarSameConsistent)
meansTable <- add_row(meansTable, name = 'Same inconsistent', mean = meanVarSameInconsistent, sd = sdVarSameInconsistent, error = seVarSameInconsistent)





t.test(varSameSwapInconsistent$reactTime, varSameInconsistent$reactTime, paired = FALSE, var.equal = FALSE)








hist(varSameSwapInconsistent$reactTime, breaks = 100)

hist(varSameInconsistent$reactTime, breaks = 100)


#Reaction time vs accuracy

# Step 1: Create a binary variable for correctness
d_correct_incorrect <- d_correct_incorrect %>%
  mutate(is_correct = responseAccuracy == "True")  # TRUE for correct responses, FALSE otherwise

# Step 2: Summarize data by subject
summary_data <- d_correct_incorrect %>%
  group_by(subjectID) %>%
  summarize(
    avg_reactTime = mean(reactTime, na.rm = TRUE),  # Average reaction time per subject
    accuracy_rate = mean(is_correct, na.rm = TRUE), # Accuracy rate per subject
    .groups = "drop"
  )

# Step 3: Calculate correlation between reaction time and accuracy
correlation_result <- cor.test(summary_data$avg_reactTime, summary_data$accuracy_rate)

# Step 4: Visualize the relationship
ggplot(summary_data, aes(x = avg_reactTime, y = accuracy_rate)) +
  geom_point(alpha = 0.7) +  # Scatter plot
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regression line
  labs(
    title = "Speed-Accuracy Trade-off",
    x = "Average Reaction Time (ms)",
    y = "Accuracy Rate"
  ) +
  theme_minimal()

# Step 5: Print the correlation results
print(correlation_result)

#######################


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




values <- c( meanVarSameSwapInconsistent, meanVarSameInconsistent)

errors <- c(seVarSameSwapInconsistent, seVarSameInconsistent)

names <- c( "Swap", "Same")

colors <- c( "#808080", "#808080")

# Base Plot
plot(1, type = "n", xlim = c(0.5, length(values) + 0.5), ylim = c(1000, max(values + errors)),
     xlab = "Trial Type", ylab = "Reaction Time (ms)",  
     xaxt = "n", yaxt = "n")

# Adding custom axes
axis(2, at = seq(1000, max(values + errors), by = 50), las = 2) # Y-axis
axis(1, at = 1:length(values), labels = names) # X-axis

# Adding bars
for (i in 1:length(values)) {
  rect(i - 0.4, 1000, i + 0.4, values[i], col = colors[i])
  
  # Adding error bars
  arrows(x0 = i, y0 = values[i] - errors[i], x1 = i, y1 = values[i] + errors[i], 
         code = 3, angle = 90, length = 0.1)
}


meanVarSameSwapInconsistent

meanVarSameInconsistent













