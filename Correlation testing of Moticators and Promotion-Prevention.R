# Correlation of reports activity and regfocus/motivators


predictors <- c("Openness_To_Change", "Self_Enhancement", "Continuity", "Self_Transcendence","Security", "Teaching","Self_Direction","Stimulation",
                "Hedonism","Achievement", "Face","Conformity","Benevolence","Universalism_Social","Universalism_Nature", "Routine", "Social_Expansion",
                "Power", "Help_Science", "Dislike", "Env_Change", "Prom_1","Prom_2","Prom_3","Prom_4", "Prom_5", "Prev_1", "Prev_2", "Prev_3", 
                "Prev_4","Prev_5", "Reg_Orientation", "Promotion","Prevention")
outcomes <- c("Network", "Total_Rprts_Filled", "Rprts_Filled_2023", "Season_Rprts_Filled_2023",  "Total_Bite_Rprts_Filled",  "Total_Adult_Rprts_Filled", "Total_Site_Rprts_Filled","Active_Duration")

# Compute the correlation matrix
correlation_matrix <- cor(data[, c(predictors, outcomes)], use = "complete.obs")


# Plotting the correlation matrix
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, addCoef.col = "black")


# Alternative with ggplot2 (for a different style of visualization)
library(reshape2)
melted_corr_matrix <- melt(correlation_matrix)
ggplot(data = melted_corr_matrix, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(label=round(value, 2)), color="black", size=3) +
  scale_fill_gradient2(midpoint=0, low="blue", mid="white", high="red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill="Correlation")
