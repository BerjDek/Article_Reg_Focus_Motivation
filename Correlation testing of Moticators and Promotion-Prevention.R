
# Correlation with Spearman.

predictors <- c("Security", "Teaching", "Self_Direction", "Stimulation", "Hedonism", 
                       "Achievement", "Face", "Conformity", "Benevolence", "Universalism_Social", 
                       "Universalism_Nature", "Routine", "Social_Expansion", "Power", "Help_Science", "Promotion", "Prevention")

outcomes <-  c("Active_Duration", "Accuracy")


# Calculate Spearman's correlation for motivators and engagement metrics
correlation_motivators <- cor(data[, c(predictors, outcomes)], 
                              method = "spearman",
                              use = "pairwise.complete.obs")

cor(data$Prevention, data$Active_Duration, method = "spearman",  use = "pairwise.complete.obs")


# Extract the relevant part of the correlation matrix for motivators
correlation_motivators_results <- correlation_motivators[length(predictors) + (1:length(outcomes)), 1:length(predictors)]
print(correlation_motivators_results)


Correlation <- as.data.frame(t(correlation_motivators_results))
Correlation <- round(Correlation, 2)


style_values <- function(data, column) {
  sorted_values <- sort(data[[column]], decreasing = TRUE)
  top_values <- sorted_values[1:5]  # Highest values
  bottom_values <- sorted_values[(length(sorted_values) - 4):length(sorted_values)]  # Lowest values (to make it the 6 lowest values replace 4 by 5)
  
  data[[column]] <- ifelse(data[[column]] %in% top_values,
                           cell_spec(data[[column]], "html", bold = TRUE),
                           data[[column]])
  data[[column]] <- ifelse(data[[column]] %in% bottom_values,
                           cell_spec(data[[column]], "html", italic = TRUE),
                           data[[column]])
  data
}


#make this into a graph 


melted_corr_matrix <- melt(correlation_motivators_results)
ggplot(data = melted_corr_matrix, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(label=round(value, 2)), color="black", size=3) +
  scale_fill_gradient2(midpoint=0, low="red", mid="white", high="red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill="Correlation")


#note; update the numbers and maybe consider them in absolute

The correlation analysis demonstrated varied strengths in the relationships between motivational values, regulatory focus, and engagement metrics:
  
  Motivators and Active Duration: Few motivators showed modest positive correlations (e.g., "Routine" ρ = 0.098), while most others exhibited weak to modest negative correlations with "Active_Duration". For example, "Achievement" was negatively correlated (ρ = -0.100).

Motivators and Total Reports Filed: Several motivators such as "Teaching" (ρ = 0.133) and "Benevolence" (ρ = 0.105) were positively correlated with "Total_Rprts_Filled", suggesting a stronger predictive power regarding participant reporting activity.

Regulatory Focus and Engagement: Both "Promotion" and "Prevention" demonstrated stronger negative correlations with "Active_Duration" (ρ = -0.156 and ρ = -0.164, respectively) and "Total_Rprts_Filled" with promotion (ρ = -0.155)
rm(correlation_data,correlation_matrix,correlation_motivators,correlation_motivators_results,melted_corr_matrix,
   engagement_metrics,motivator_columns,outcomes,predictors,regulatory_focus_columns)





# Correlation with Pearson.

predictors <- c("Security", "Teaching", "Self_Direction", "Stimulation", "Hedonism", 
                "Achievement", "Face", "Conformity", "Benevolence", "Universalism_Social", 
                "Universalism_Nature", "Routine", "Social_Expansion", "Power", "Help_Science", "Promotion", "Prevention")

outcomes <-  c("Active_Duration", "Accuracy")


# Calculate Spearman's correlation for motivators and engagement metrics
correlation_motivators <- cor(data[, c(predictors, outcomes)], 
                              method = "pearson",
                              use = "pairwise.complete.obs")


# Extract the relevant part of the correlation matrix for motivators
correlation_motivators_results <- correlation_motivators[length(predictors) + (1:length(outcomes)), 1:length(predictors)]
print(correlation_motivators_results)


melted_corr_matrix <- melt(correlation_motivators_results)
ggplot(data = melted_corr_matrix, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(label=round(value, 2)), color="black", size=3) +
  scale_fill_gradient2(midpoint=0, low="red", mid="white", high="red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill="Correlation")
