# Correlation of reports activity and regfocus/motivators
library(reshape2)

predictors <- c("Openness_To_Change", "Self_Enhancement", "Continuity", "Self_Transcendence","Security", "Teaching","Self_Direction","Stimulation",
                "Hedonism","Achievement", "Face","Conformity","Benevolence","Universalism_Social","Universalism_Nature", "Routine", "Social_Expansion",
                "Power", "Help_Science", "Prom_1","Prom_2","Prom_3","Prom_4", "Prom_5", "Prev_1", "Prev_2", "Prev_3", 
                "Prev_4","Prev_5", "Reg_Orientation", "Promotion","Prevention")
outcomes <- c("Network", "Total_Rprts_Filled", "Rprts_Filled_2023", "Season_Rprts_Filled_2023",  "Total_Bite_Rprts_Filled",  "Total_Adult_Rprts_Filled", "Total_Site_Rprts_Filled","Active_Duration")

# Compute the correlation matrix
correlation_matrix <- cor(data[, c(predictors, outcomes)], use = "complete.obs")

correlation_data <- as.data.frame(correlation_matrix)

# Plotting the correlation matrix
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, addCoef.col = "black")


# Alternative with ggplot2 (for a different style of visualization)

melted_corr_matrix <- melt(correlation_matrix)
ggplot(data = melted_corr_matrix, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(label=round(value, 2)), color="black", size=3) +
  scale_fill_gradient2(midpoint=0, low="blue", mid="white", high="red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill="Correlation")

colnames(data)
#Active days correlated with promotion and prevention active days not so much




#new correlation with Spearman.

predictors <- c("Security", "Teaching", "Self_Direction", "Stimulation", "Hedonism", 
                       "Achievement", "Face", "Conformity", "Benevolence", "Universalism_Social", 
                       "Universalism_Nature", "Routine", "Social_Expansion", "Power", "Help_Science", "Promotion", "Prevention")

outcomes <-  c("Active_Duration", "Total_Rprts_Filled")


# Calculate Spearman's correlation for motivators and engagement metrics
correlation_motivators <- cor(data[, c(predictors, outcomes)], 
                              method = "spearman")


# Extract the relevant part of the correlation matrix for motivators
correlation_motivators_results <- correlation_motivators[length(predictors) + (1:length(outcomes)), 1:length(predictors)]
print(correlation_motivators_results)


#make this into a graph like above


melted_corr_matrix <- melt(correlation_motivators_results)
ggplot(data = melted_corr_matrix, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(label=round(value, 2)), color="black", size=3) +
  scale_fill_gradient2(midpoint=0, low="red", mid="white", high="red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill="Correlation")


The correlation analysis demonstrated varied strengths in the relationships between motivational values, regulatory focus, and engagement metrics:
  
  Motivators and Active Duration: Few motivators showed modest positive correlations (e.g., "Routine" ρ = 0.098), while most others exhibited weak to modest negative correlations with "Active_Duration". For example, "Achievement" was negatively correlated (ρ = -0.100).

Motivators and Total Reports Filed: Several motivators such as "Teaching" (ρ = 0.133) and "Benevolence" (ρ = 0.105) were positively correlated with "Total_Rprts_Filled", suggesting a stronger predictive power regarding participant reporting activity.

Regulatory Focus and Engagement: Both "Promotion" and "Prevention" demonstrated stronger negative correlations with "Active_Duration" (ρ = -0.156 and ρ = -0.164, respectively) and "Total_Rprts_Filled" (ρ = -0.155 and ρ = -0.128, respectively), indicating that higher scores in regulatory focus were associated with less engagement in terms of duration and report filing.


rm(correlation_data,correlation_matrix,correlation_motivators,correlation_motivators_results,melted_corr_matrix,
   engagement_metrics,motivator_columns,outcomes,predictors,regulatory_focus_columns)
