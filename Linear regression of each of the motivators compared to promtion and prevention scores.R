#correlation test


library(broom)


Several correlations in the matrix are above 0.8, indicating potential multicollinearity concerns:
  
Achievement - Help_Science (0.87)
Power - Social_Expansion (0.88)
Conformity - Benevolence (0.83)
Universalism_Nature - Social (0.82)
Face - Hedonism (0.81)
Self_Direction - Stimulation (0.79)

As these high correlations suggest Including all these variables in a single regression model can lead to unreliable coefficient 
estimates and difficulty interpreting the effects of individual motivations.


#Active Duration


# List of predictors
predictors <- c("Security", "Teaching", "Self_Direction", "Stimulation", "Hedonism",
                "Achievement", "Face", "Conformity", "Benevolence", "Universalism_Social",
                "Universalism_Nature", "Routine", "Social_Expansion", "Power", "Help_Science",
                "Promotion", "Prevention")



# Initialize an empty data frame to store results
results <- data.frame(Predictor = character(), Beta = numeric(), SE = numeric(),
                      t_value = numeric(), p_value = numeric(), stringsAsFactors = FALSE)



for (pred in predictors) {
  formula <- as.formula(paste("Active_Duration ~", pred))
  model <- lm(formula, data = data)
  tidy_model <- tidy(model)[2, ] 
  
  # Append results
  results <- results %>%
    add_row(Predictor = pred, Beta = as.numeric(tidy_model$estimate),
            SE = as.numeric(tidy_model$std.error), t_value = as.numeric(tidy_model$statistic),
            p_value = as.numeric(tidy_model$p.value))
}






# Rename columns appropriately
names(results) <- c("Predictor", "Beta", "SE", "t_value", "p_value")

# Output the complete results table
print(results)

# Creating a summary table with the absolute value of Beta and the p-values
summary_table <- results %>%
  mutate(Abs_Beta = abs(Beta)) %>%
  arrange(desc(Abs_Beta)) %>%
  mutate(Beta_Rank = ifelse(row_number() <= 5, "**bold**", "")) %>%
  arrange(p_value) %>%
  mutate(p_value_Rank = ifelse(row_number() <= 5, "**bold**", ""))

# Output the modified summary table
print(summary_table[, c("Predictor", "Abs_Beta", "Beta_Rank", "p_value", "p_value_Rank")])

#both promotion and prevention had the highest p-values showing the most reliable obsereved relationship with active participation duration







#Total Reports Filled


# List of predictors
predictors <- c("Security", "Teaching", "Self_Direction", "Stimulation", "Hedonism",
                "Achievement", "Face", "Conformity", "Benevolence", "Universalism_Social",
                "Universalism_Nature", "Routine", "Social_Expansion", "Power", "Help_Science",
                "Promotion", "Prevention")

# Initialize an empty data frame to store results
results <- data.frame(Predictor = character(), Beta = numeric(), SE = numeric(),
                      t_value = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

# Loop through each predictor and perform regression
for (pred in predictors) {
  # Update the formula to use 'Total_Rprts_Filled' as the dependent variable
  formula <- as.formula(paste("Total_Bite_Rprts_Filled ~", pred))
  model <- lm(formula, data = data)
  tidy_model <- tidy(model)[2, ]  # Select only the row for the predictor
  
  # Append results
  results <- results %>%
    add_row(Predictor = pred, Beta = as.numeric(tidy_model$estimate),
            SE = as.numeric(tidy_model$std.error), t_value = as.numeric(tidy_model$statistic),
            p_value = as.numeric(tidy_model$p.value))
}

# Output the complete results table
print(results)

# Creating a summary table with the absolute value of Beta and the p-values
summary_table <- results %>%
  mutate(Abs_Beta = abs(Beta)) %>%
  arrange(desc(Abs_Beta)) %>%
  mutate(Beta_Rank = ifelse(row_number() <= 5, "**bold**", "")) %>%
  arrange(p_value) %>%
  mutate(p_value_Rank = ifelse(row_number() <= 5, "**bold**", ""))

# Output the modified summary table
print(summary_table[, c("Predictor", "Abs_Beta", "Beta_Rank", "p_value", "p_value_Rank")])


#the promotion and prevention could not predict the number of reports filled by the user properly 




#next do the correct answers for accuracy


#Accuracy


# List of predictors
predictors <- c("Security", "Teaching", "Self_Direction", "Stimulation", "Hedonism",
                "Achievement", "Face", "Conformity", "Benevolence", "Universalism_Social",
                "Universalism_Nature", "Routine", "Social_Expansion", "Power", "Help_Science",
                "Promotion", "Prevention")



# Initialize an empty data frame to store results
results <- data.frame(Predictor = character(), Beta = numeric(), SE = numeric(),
                      t_value = numeric(), p_value = numeric(), stringsAsFactors = FALSE)



for (pred in predictors) {
  formula <- as.formula(paste("Accuracy ~", pred))
  model <- lm(formula, data = data)
  tidy_model <- tidy(model)[2, ] 
  
  # Append results
  results <- results %>%
    add_row(Predictor = pred, Beta = as.numeric(tidy_model$estimate),
            SE = as.numeric(tidy_model$std.error), t_value = as.numeric(tidy_model$statistic),
            p_value = as.numeric(tidy_model$p.value))
}






# Rename columns appropriately
names(results) <- c("Predictor", "Beta", "SE", "t_value", "p_value")

# Output the complete results table
print(results)

# Creating a summary table with the absolute value of Beta and the p-values
summary_table <- results %>%
  mutate(Abs_Beta = abs(Beta)) %>%
  arrange(desc(Abs_Beta)) %>%
  mutate(Beta_Rank = ifelse(row_number() <= 5, "**bold**", "")) %>%
  arrange(p_value) %>%
  mutate(p_value_Rank = ifelse(row_number() <= 5, "**bold**", ""))

# Output the modified summary table
print(summary_table[, c("Predictor", "Abs_Beta", "Beta_Rank", "p_value", "p_value_Rank")])

#both promotion and prevention have values in the top 5







##TESTING


# List of predictors
predictors <- c("Security", "Teaching", "Self_Direction", "Stimulation", "Hedonism",
                "Achievement", "Face", "Conformity", "Benevolence", "Universalism_Social",
                "Universalism_Nature", "Routine", "Social_Expansion", "Power", "Help_Science",
                "Promotion", "Prevention")



# Initialize an empty data frame to store results
results <- data.frame(Predictor = character(), Beta = numeric(), SE = numeric(),
                      t_value = numeric(), p_value = numeric(), stringsAsFactors = FALSE)



for (pred in predictors) {
  formula <- as.formula(paste("accuracy_2 ~", pred))
  model <- lm(formula, data = data)
  tidy_model <- tidy(model)[2, ] 
  
  # Append results
  results <- results %>%
    add_row(Predictor = pred, Beta = as.numeric(tidy_model$estimate),
            SE = as.numeric(tidy_model$std.error), t_value = as.numeric(tidy_model$statistic),
            p_value = as.numeric(tidy_model$p.value))
}






# Rename columns appropriately
names(results) <- c("Predictor", "Beta", "SE", "t_value", "p_value")

# Output the complete results table
print(results)

# Creating a summary table with the absolute value of Beta and the p-values
summary_table <- results %>%
  mutate(Abs_Beta = abs(Beta)) %>%
  arrange(desc(Abs_Beta)) %>%
  mutate(Beta_Rank = ifelse(row_number() <= 5, "**bold**", "")) %>%
  arrange(p_value) %>%
  mutate(p_value_Rank = ifelse(row_number() <= 5, "**bold**", ""))

# Output the modified summary table
print(summary_table[, c("Predictor", "Abs_Beta", "Beta_Rank", "p_value", "p_value_Rank")])

#no relation of promotion and prevention with score but teaching and routine really significant, for accuracy_2 prev an pro were in 3 and 4th place
# no significance 0.16 0.22),



