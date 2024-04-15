# Analysis

table(survey_data$Reg_Orientation_Cat)

### Chceking the mean regulatory focus
mean_reg_orientation <- mean(survey_data$Reg_Orientation, na.rm = TRUE) #-1.402

#To check if the distribution of regulatory focus scores of participants is significantly different we conduct a one-sample t-test 
#to compare the mean of the sample to the average regulatory focus mean of a general population which should be 0.
t_test_result <- t.test(survey_data$Reg_Orientation, mu = 0, na.rm = TRUE)
t_test_result
#  Reg focus average is signinficantly Prevention oriented when compared to a neutral population,  t = -4.3063, df = 215, p-value = 2.524e-05,95 percent confidence interval:
# -2.0448547 -0.7607009

# The average regulatory focus of the participant pool that responded to the survey was significantly different than the norm, 
# generally considered to be neutral population-wide, with a tendency of western countries to be more promotion oriented and yield a positive average,
# the t-test comparing the survey result to that the population yielded a t-statistic equal to to **-4.3** with 215 degrees of freedom, 
# and with a p-value of **2.524e-05** well below the significance threshold, indicating that the mean regulatory orientation for users 
# who completed the survey is significantly different from 0 and more Prevention oriented.

rm(mean_reg_orientation, t_test_result)


ggplot(survey_data, aes(x = Reg_Orientation)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "#b4464b") +
  geom_vline(xintercept = -1.402778, color = "#82b446", linetype = "dashed") +
  geom_vline(xintercept = -2.0448547, color = "#b47846", linetype = "dashed") +
  geom_vline(xintercept = -0.7607009, color = "#b47846", linetype = "dashed") +
  annotate("text", x = -1.402778, y = Inf, label = "Mean (-1.40)", vjust = 0.8, color = "#82b446", size = 3) +
  annotate("text", x = -2.0448547, y = Inf, label = "Lower CI (-2.04)", vjust = 2,hjust = 1.2, color = "#b47846", size = 3) +
  annotate("text", x = -0.7607009, y = Inf, label = "Upper CI (-0.76)", vjust = 2, hjust = -0.3, color = "#b47846", size = 3) +
  labs(title = "One Sample t-test Result Visualization",
       x = "Reg Orientation Value",
       y = "Density") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, size = 7))


### Direct comparison between the number of Promotion and Prevention oriented participants.

oriented_data <- subset(survey_data, Reg_Orientation_Cat %in% c('Prevention', 'Promotion'))
oriented_counts <- table(oriented_data$Reg_Orientation_Cat)
oriented_counts <- oriented_counts[names(oriented_counts) != "Neutral"]
oriented_counts

# Since we expect the counts to be equal, the expected frequency for each is half of the total count
total_expected_count <- sum(oriented_counts)
expected_counts <- rep(total_expected_count / 2, 2)

chi_squared_test <- chisq.test(oriented_counts, p = rep(1/2, 2))
print(chi_squared_test) # p-value = 1.536e-05

#The test yielded a chi-squared statistic equal to **18.692** and a p-value of **1.536e-05** suggesting  a significant 
# association or difference in the users' regulatory orientation categories significantly different from a uniform one.


rm(orientation_counts, chi_squared_test, oriented_data, oriented_counts, total_expected_count, expected_counts)



### Motivation Data
#checking if some motivators were more important than others

Motivator_Means <- survey_data %>%
  summarise(across(Security:Help_Science, ~mean(., na.rm = TRUE))) %>%  
  pivot_longer(everything(), names_to = "Motivator", values_to = "Mean")


motivator_ratings <- survey_data %>%
  dplyr::select(Security:Help_Science) %>%
  pivot_longer(cols = everything(), names_to = "Motivator", values_to = "Rating") %>%
  drop_na(Rating)

anova_result <- aov(Rating ~ Motivator, data = motivator_ratings)
summary(anova_result)

rm(motivator_ratings,anova_result)
#ANOVA confirmed a significant result that different motivators were considered more important that others, not sure this should be included




# To check if Regulatory focus impact motivation rating of users, we attempt to see if the rating each segment have given various motivators varied
# we first calculate the mean of each motivator based on regulatory focus 
Motivator_Means_By_Reg_Focus <- survey_data %>%
  filter(!is.na(Reg_Orientation_Cat))  %>%
  group_by(Reg_Orientation_Cat) %>%
  summarise(across(Security:Help_Science, mean, na.rm = TRUE))%>%
  pivot_longer(-Reg_Orientation_Cat, names_to = "Motivator", values_to = "Mean") %>%
  pivot_wider(names_from = Reg_Orientation_Cat, values_from = Mean) 

# top 6 of the motivators are shared, top motivation is the same 

motivator_columns <- c("Security","Teaching", "Self_Direction","Stimulation", "Hedonism", "Achievement","Face","Conformity","Benevolence",
                       "Universalism_Social" ,"Universalism_Nature" ,"Routine",            
                       "Social_Expansion" ,"Power","Help_Science")


#We start off by completing a series of One-way ANOVAs, to treat each motivator as a separate dependent variable, to see if their 
# scoring of the motivator is significantly different between groups
for (motivator in motivator_columns) {
  formula <- as.formula(paste(motivator, "~ Reg_Orientation_Cat"))
  anova_result <- aov(formula, data = survey_data)
  cat("\nANOVA results for", motivator, ":\n")
  print(summary(anova_result))
  cat("\n")
}

# The results show that the only significant results was the difference in the way Help_Science, Benevolence, and Stimulation were rated 
# differently with varying regulatory focus orientations 


#doing tukey for each significant ANOVA

#Help_Science
anova_result <- aov(Help_Science ~ Reg_Orientation_Cat, data = survey_data)

tukey_result <- TukeyHSD(anova_result)
print(tukey_result) #Promotion-Neutral P = 0.0290

#Benevolence
anova_result <- aov(Benevolence ~ Reg_Orientation_Cat, data = survey_data)

tukey_result <- TukeyHSD(anova_result)
print(tukey_result) #Prevention-Neutral P = 0.05 NOT SIGNIFICANT

#Stimulation
anova_result <- aov(Stimulation ~ Reg_Orientation_Cat, data = survey_data)

tukey_result <- TukeyHSD(anova_result)
print(tukey_result) #Promotion-Neutral P= 0.0046  Prevention-Neutral 0.0096

rm(anova_result, motivator_columns)

#results show that there was no significant difference between any two groups for the scoring Benevolence, but participants with Neutral Orientation
# scored Help_Science signicantly lower than those with Promotion Orientation, while maintaining to be ranked first amongst all motivators similarly
# to both prevention and promotion oriented participants and Stimulation significantly lower than participants with both Prevention and Promotion
# orientations. No significant differences in scoring were found for between promotion and prevention orientation volunteers


