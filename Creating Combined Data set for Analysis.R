#Creating Data set for Analysis


reports_data_clean <- reports_data %>%
  filter(User_ID %in% survey_data$User_ID) %>% 
  group_by(User_ID) %>%
  mutate(Total_Rprts_Filled = n(),
         Total_Bite_Rprts_Filled = sum(Rprt_Type == "bite"),
         Total_Adult_Rprts_Filled = sum(Rprt_Type == "adult"),
         Total_Site_Rprts_Filled = sum(Rprt_Type == "site"),
         Rprts_Filled_2023 = sum(Rprt_Date >= "2023-01-01" & Rprt_Date <= "2023-12-31"),
         Rprts_Filled_2022 = sum(Rprt_Date >= "2022-01-01" & Rprt_Date <= "2022-12-31"),
         Rprts_Filled_2021 = sum(Rprt_Date >= "2021-01-01" & Rprt_Date <= "2021-12-31"),
         Season_Rprts_Filled_2023 = sum(Rprt_Date >= "2023-05-01" & Rprt_Date <= "2023-10-30"),
         Season_Rprts_Filled_2022 = sum(Rprt_Date >= "2022-05-01" & Rprt_Date <= "2022-10-30"),
         Season_Rprts_Filled_2021 = sum(Rprt_Date >= "2021-05-01" & Rprt_Date <= "2021-10-30"),
         Active_Duration = as.integer(difftime(max(Rprt_Date, na.rm = TRUE), min(Rprt_Date, na.rm = TRUE), units = "days"))) %>% 
  slice(1L) %>% 
  ungroup() %>%
  select(-Rprt_Date, -Rprt_Type, -location_choice)  



data <- full_join(survey_data, reports_data_clean, by = "User_ID")


colnames(data)




write.csv(data, "Research_Data.csv", row.names = FALSE)

colnames(data)
