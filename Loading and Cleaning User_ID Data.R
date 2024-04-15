##Loading this is unncecasary for the regulatory focus paper, as I dont need to compare between the general po;uation and the ones that filled the survey.
#the n says the number of reports, but ask Agusti where the scores are coming from.

raw_user_data <- read.csv(file="reports_by_uuid.csv", header = TRUE)


##

#since the beginning of the project, there have been 354,712 registration/downloads
raw_user_data <- raw_user_data %>% 
  rename( User_ID = user_UUID,
          Registered_Participation_Date = registration_time,
          Registered_Total_Reports = n) %>% 
  mutate(Registered_Participation_Date = as.POSIXct(Registered_Participation_Date, format = "%Y-%m-%d %H:%M:%S"),
         Registered_Total_Reports = as.integer(Registered_Total_Reports)) %>%
  replace_na(list(Registered_Total_Reports = 0)) 



# Count of unique users that have submitted at least 1 report
nrow(raw_user_data %>%
       filter(!is.na(Registered_Total_Reports) & Registered_Total_Reports >= 1))


# 70,799 of the registered users have  filled a report between update and end of 2023

nrow(raw_user_data %>%
       filter(!is.na(Registered_Total_Reports) & Registered_Total_Reports >= 1 & 
                Registered_Participation_Date >= as.POSIXct("2020-10-02") & Registered_Participation_Date <= as.POSIXct("2023-12-31")))

# 54,844 registered users since the cutoff date of 2020-10-02. There is a small difference in the numbers between the unique users here and ones from the 
# reports data set, 54,844 vs 54,853, even though the data was extracted/requested on the same day probably in the minutes between reports some reporsts were filled.

#the number from reports csv is going to be used



#checking the number of users registered after the update.
nrow(raw_user_data %>%
       filter(Registered_Participation_Date >= as.POSIXct("2020-10-02")))
#   310924 users in total, this number is going to be used as the pool to attach to the main data set. 


## NOTE there is a cutoff at end of 2024, if needs to be removed
user_data <- raw_user_data %>%
  filter(Registered_Participation_Date >= as.POSIXct("2020-10-02") & Registered_Participation_Date <= as.POSIXct("2023-12-31"))

write.csv(user_data, "CleanUserData.csv", row.names = FALSE)
rm(raw_user_data)

#rm(user_data)
