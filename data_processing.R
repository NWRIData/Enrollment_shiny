library(dplyr)
library(tidyr)
library(here)

#establish date of the system for file labelling
date<-Sys.Date()


#this script processes the data for the shiny app to use
#load old data
olddata <- read.csv(here("tracker", "data","olddata_test.csv"))


#load most recent data


# Specify the directory
dir_path <- here("tracker", "data")

# List all CSV files in the directory with full paths
csv_files <- list.files(path = dir_path, pattern = "\\.csv$", full.names = TRUE)

# Get file info (including modification time)
file_info <- file.info(csv_files)

# Order by modification time (descending)
file_info <- file_info[order(file_info$mtime, decreasing = TRUE), ]

# Get the path of the most recently modified CSV file
most_recent_csv <- rownames(file_info)[1]

# Print it
print(most_recent_csv)
recentdata<-read.csv(most_recent_csv,
                     sep = ",")


#filter out unneeded columns
olddata2<-olddata %>%
  select(colnames(recentdata)) %>%
  relocate(EnrollmentDate, .before = AdmissionDate)

fdoe_enroll2<-rbind(olddata2, recentdata)

cycles <- data.frame(
  cycle_id = c('Y4', 'Y5'),
  start_date = as.Date(c("2024-06-11", "2025-06-23")),
  end_date = as.Date(c("2025-06-22", NA))  # NA for open-ended
)




app_data <- fdoe_enroll2 %>%
  filter(EnrollmentDate > "2024-06-11", EnrollmentDate < "2025-06-23") %>%
  select(NWRIEnrollmentID, DistrictName, AdmissionDate, EnrollmentDate) %>%
  mutate(
    cycle_id = case_when(
      AdmissionDate >= as.Date("2024-06-11") & AdmissionDate <= as.Date("2025-06-22") ~ "Y4",
      AdmissionDate >= as.Date("2025-06-23") ~ "Y5",
      TRUE ~ NA_character_
    )
  )



applied_not_enrolled<-app_data %>%
  filter(is.na(cycle_id)) %>%
  left_join(cycles, by = "cycle_id") %>%
  mutate(
    week_of_cycle = as.integer(floor(as.numeric(difftime(EnrollmentDate, start_date, units = "days")) / 7) + 1),
    start_date_cycle = start_date + (week_of_cycle - 1) * 7,
    end_date_cycle = start_date_cycle + 6
  ) 
saveRDS(applied_not_enrolled, file = here("tracker", "data","app_data",paste0("applicationdata",date,".rds")))

app_data_enrolled<-app_data %>%
  filter(!is.na(cycle_id)) %>%
  left_join(cycles, by = "cycle_id") %>%
  mutate(
    week_of_cycle = as.integer(floor(as.numeric(difftime(AdmissionDate, start_date, units = "days")) / 7) + 1),
    start_date_cycle = start_date + (week_of_cycle - 1) * 7,
    end_date_cycle = start_date_cycle + 6
  ) 


max_week_old <- max(app_data_enrolled$week_of_cycle, na.rm = TRUE)
max_week_curr <- max(app_data_enrolled %>%
                       filter(cycle_id == "Y5") %>%
                       pull(week_of_cycle), na.rm = TRUE)

# Create full range from 1 to max_week
olddataweek <- app_data_enrolled %>%
  filter(cycle_id == "Y4") %>%
  group_by(week_of_cycle,start_date_cycle) %>%
  count(name = "n") %>%
  ungroup() %>%
  complete(week_of_cycle = 1:max_week_old, fill = list(n = 0)) %>%
  arrange(week_of_cycle) %>%
  mutate(
    cumulative_applicants = cumsum(n)
  ) %>%
  mutate(Year = "Previous Year")

olddataweek <- olddataweek %>%
  mutate(start_date_cycle = if_else(
    is.na(start_date_cycle),
    lag(start_date_cycle) + 7,
    start_date_cycle
  ))

curreentdataweek <- app_data_enrolled %>%
  filter(cycle_id == "Y5") %>%
  group_by(week_of_cycle,start_date_cycle) %>%
  count(name = "n") %>%
  ungroup() %>%
  complete(week_of_cycle = 1:max_week_curr, fill = list(n = 0)) %>%
  arrange(week_of_cycle) %>%
  mutate(
    cumulative_applicants = cumsum(n)
  ) %>%
  mutate(Year = "Current Year")

curreentdataweek <- curreentdataweek %>%
  mutate(start_date_cycle = if_else(
    is.na(start_date_cycle),
    lag(start_date_cycle) + 7,
    start_date_cycle
  ))

totaldata = rbind(olddataweek,curreentdataweek)

saveRDS(totaldata, file = here("tracker", "data","total_data",paste0("totaldata",date,".rds")))


diff_total<-totaldata %>%
  filter(week_of_cycle == max_week_curr) %>%
  pivot_wider(id_cols = week_of_cycle, names_from = Year, values_from = c(n, cumulative_applicants))

saveRDS(diff_total, file = here("tracker", "data","diff_total",paste0("diff_total",date,".rds")))


#complete the full range for all schools 

district_df_old <- app_data_enrolled %>%
  filter(cycle_id == "Y4") %>%
  group_by(DistrictName, week_of_cycle) %>%
  count(name = "n") %>%
  ungroup() %>%
  complete(DistrictName, week_of_cycle = 1:max_week_old, fill = list(n = 0)) %>%
  arrange(DistrictName, week_of_cycle) %>%
  group_by(DistrictName) %>%
  mutate(cumulative_applicants = cumsum(n)) %>%
  ungroup() %>%
  drop_na(DistrictName) %>%
  mutate(Year = "Previous year")


district_df_current <- app_data_enrolled %>%
  filter(cycle_id == "Y5") %>%
  group_by(DistrictName, week_of_cycle) %>%
  count(name = "n") %>%
  ungroup() %>%
  complete(DistrictName, week_of_cycle = 1:max_week_curr, fill = list(n = 0)) %>%
  arrange(DistrictName, week_of_cycle) %>%
  group_by(DistrictName) %>%
  mutate(cumulative_applicants = cumsum(n)) %>%
  ungroup() %>%
  drop_na(DistrictName) %>%
  mutate(Year = "Current year")

district_data<-rbind(district_df_old, district_df_current)



saveRDS(district_data, file = here("tracker", "data","district_df",paste0("district_df",date,".rds")))
