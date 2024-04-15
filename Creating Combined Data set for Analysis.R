#Creating Data set for Analysis

data <- full_join(survey_data, message_data, by = "User_ID")