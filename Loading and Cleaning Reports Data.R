
raw_reports_data <- read.csv(file="full_reports_table.csv", header = TRUE) %>% 
  mutate(creation_time= as.POSIXct(creation_time, format = "%Y-%m-%d"))

min(raw_reports_data$creation_time, na.rm = TRUE)

#Since the initiation of the Citizen Science project until 21/11/2023 179995 Reports have been filled


#To create a reports data that can be assessed and merged with a main data set 
#The raw data is cleaned to limit it to the final Update on 2nd of October 2020, after which all User Id's have been reset


reports_data <- raw_reports_data %>%
  dplyr::select(user_id, location_choice, creation_time, type) %>% 
  rename(User_ID = user_id,
         Rprt_Date = creation_time,
         Rprt_Type = type) %>% 
  filter(User_ID != "" & Rprt_Date >= as.POSIXct("2020-10-02") & Rprt_Date <= as.POSIXct("2023-12-31"))%>% 
  mutate(Rprt_Type = as.factor(Rprt_Type))


n_distinct(reports_data$User_ID) # 54853 unique users have filled reports
min(reports_data$Rprt_Date, na.rm = TRUE)


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
  select(-Rprt_Date, -location_choice)  


write.csv(reports_data, "CleanReportsData.csv", row.names = FALSE)
rm(raw_reports_data)

#Since 12/10/2020 till the date of the analysis 147,123 reports have been filled.



summary(reports_data)



