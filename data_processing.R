library(dplyr)
library(tidyr)
library(here)

#establish date of the system for file labelling
date<-Sys.Date()


#this script processes the data for the shiny app to use
#load old data
olddata <- read.csv(here("tracker", "data","Y4_data","olddata_test.csv"))
district_code_data <- readRDS(here("tracker", "data","MSID","MSID_08112025.rds"))


last_processed_path <- here("tracker", "data", "last_processed.txt")

#load most recent data

# Specify the directory
dir_path <- here("tracker", "data")

files <- list.files(here("tracker", "data"), pattern = "enrollments_\\d{8}\\.csv", full.names = TRUE)

# Extract dates from filenames
dates <- as.Date(gsub(".*_(\\d{8})\\.csv", "\\1", files), format = "%Y%m%d")

# Get the most recent based on the date in the filename
latest_file <- files[which.max(dates)]

cat("Latest file based on filename date:", latest_file, "\n")


# Read last processed filename if exists
last_processed <- if (file.exists(last_processed_path)) {
  readLines(last_processed_path)
} else {
  NA_character_
}

# If latest file is same as last processed, skip processing
if (!is.na(last_processed) && latest_file == last_processed) {
  cat("No new data file found. Skipping processing.")
  quit(save = "no")  # Exit the script early
}

cat("writing new last processed path with this:",latest_file)
# Otherwise, update last processed record
writeLines(latest_file, last_processed_path)

# Print it
print(latest_file)
recentdata <- read.csv(latest_file, na.strings = c("NA", ""))
cat("Reading file from:", latest_file, "\n")

#save the lost kids:
lostkids<-recentdata %>%
  filter(is.na(DistrictID)) %>%
  filter(!is.na(AdmissionDate))

#save lost kids data:

saveRDS(lostkids, file = here("tracker", "data","lostkids","raw", paste0("lost_kids_raw",date,".rds")))

no_lost_kids<-nrow(lostkids)
saveRDS(no_lost_kids, file = here("tracker", "data","lostkids","count", paste0("lost_kids_count",date,".rds")))

#save a copy for grade levels
recentdatacopy<-recentdata


#get rid of mappedschool
recentdata<-recentdata %>%
  select(-MappedDOESchool) %>%
  filter(!is.na(DistrictID))

#clean up district names so they match up to the district names in the application and enrollment data
district_code_data<-district_code_data %>%
  distinct(DISTRICT ,DISTRICT_NAME) %>%
  mutate(DISTRICT_NAME = ifelse(DISTRICT_NAME == "MIAMI-DADE", "MIAMI DADE", DISTRICT_NAME)) %>%
  mutate(DISTRICT_NAME = ifelse(DISTRICT_NAME == "IDEA PUB SCH", "IDEA PUBLIC SCHOOLS", DISTRICT_NAME)) %>%
  mutate(DISTRICT_NAME = ifelse(DISTRICT_NAME == "FAMU LAB SCH", "FAMU LAB SCHOOL", DISTRICT_NAME)) %>%
  mutate(DISTRICT_NAME = ifelse(DISTRICT_NAME == "FAU LAB SCH", "FAU LAB SCHOOL", DISTRICT_NAME)) %>%
  mutate(DISTRICT_NAME = ifelse(DISTRICT_NAME == "UF LAB SCH", "UF LAB SCHOOL", DISTRICT_NAME)) %>%
  mutate(DISTRICT_NAME = ifelse(DISTRICT_NAME == "FSU LAB SCH", "FSU LAB SCHOOL", DISTRICT_NAME)) %>%
  mutate(DISTRICT_NAME = ifelse(DISTRICT_NAME == "FL VIRTUAL", "VIRTUAL SCHOOL", DISTRICT_NAME)) %>%
  mutate(DISTRICT_NAME = ifelse(DISTRICT_NAME == "DEAF/BLIND", "FLORIDA SCHOOL FOR THE DEAF & BLIND", DISTRICT_NAME)) %>%
  rename(DistrictName = DISTRICT_NAME)

#now join the old data with these new district names and codes
olddata<-olddata %>%
  rename(DISTRICT_NAME = DistrictName) %>%
  rename(DistrictName = DISTRICT_NAME) %>%
  mutate(DistrictName = toupper(DistrictName)) %>%
  mutate(DistrictName = ifelse(DistrictName %in%  c("IDEA","IDEA CHARTER"), "IDEA PUBLIC SCHOOLS", DistrictName)) %>%
  mutate(DistrictName = ifelse(DistrictName %in%  c("MATER"), "MIAMI DADE", DistrictName)) %>%
  left_join(.,district_code_data, by = c("DistrictName")) 

#fix up most recent data
recentdata<-recentdata %>%
  mutate(DistrictName = toupper(DistrictName)) %>%
  rename(DistrictID = DistrictID) %>%
  select(-c(SchoolID,Grade))

#filter out unneeded columns
olddata2<-olddata %>%
  select(colnames(recentdata %>%
                    select(-DistrictID))) %>%
  relocate(EnrollmentDate, .before = AdmissionDate)

fdoe_enroll2<-rbind(olddata2, recentdata %>%
                      select(-DistrictID))

cycles <- data.frame(
  cycle_id = c('Y4', 'Y5'),
  start_date = as.Date(c("2024-06-11", "2025-06-23")),
  end_date = as.Date(c("2025-06-22", NA))  # NA for open-ended
)

app_data <- fdoe_enroll2 %>%
  filter(EnrollmentDate > "2024-06-11") %>%
  select(NWRIEnrollmentID, DistrictName, AdmissionDate, EnrollmentDate) %>%
  mutate(
    cycle_id = case_when(
      AdmissionDate >= as.Date("2024-06-11") & AdmissionDate <= as.Date("2025-06-22") ~ "Y4",
      AdmissionDate >= as.Date("2025-06-23") ~ "Y5",
      TRUE ~ NA_character_
    )
  )

applied_not_enrolled<-app_data %>%
  filter(!is.na(cycle_id)) %>%
  filter(is.na(AdmissionDate)) %>%
  left_join(cycles, by = "cycle_id") %>%
  mutate(
    week_of_cycle = as.integer(floor(as.numeric(difftime(EnrollmentDate, start_date, units = "days")) / 7) + 1),
    start_date_cycle = start_date + (week_of_cycle - 1) * 7,
    end_date_cycle = start_date_cycle + 6
  ) 

saveRDS(applied_not_enrolled, file = here("tracker", "data","app_data", paste0("applicationdata",date,".rds")))

#here we make a dataset for grade-level-specific data
grade_level_data<-recentdatacopy %>%
  filter(EnrollmentDate > "2024-06-11") %>%
  mutate(
    cycle_id = case_when(
      AdmissionDate >= as.Date("2024-06-11") & AdmissionDate <= as.Date("2025-06-22") ~ "Y4",
      AdmissionDate >= as.Date("2025-06-23") ~ "Y5",
      TRUE ~ NA_character_
    )
  ) %>%
  drop_na(cycle_id) %>%
  filter(!is.na(AdmissionDate)) %>%
  left_join(cycles, by = "cycle_id")  %>%
  mutate(
    week_of_cycle = as.integer(floor(as.numeric(difftime(AdmissionDate, start_date, units = "days")) / 7) + 1),
    start_date_cycle = start_date + (week_of_cycle - 1) * 7,
    end_date_cycle = start_date_cycle + 6
  ) 
#####

app_data_enrolled<-app_data %>%
  filter(!is.na(cycle_id)) %>%
  left_join(cycles, by = "cycle_id") %>%
  mutate(
    week_of_cycle = as.integer(floor(as.numeric(difftime(AdmissionDate, start_date, units = "days")) / 7) + 1),
    start_date_cycle = start_date + (week_of_cycle - 1) * 7,
    end_date_cycle = start_date_cycle + 6
  ) 


start_date_202425<-as.Date("2024-06-11")
start_date_202526<-as.Date("2025-06-23")

max_week_old <- max(app_data_enrolled$week_of_cycle, na.rm = TRUE)
max_week_curr <- max(app_data_enrolled %>%
                       filter(cycle_id == "Y5") %>%
                       pull(week_of_cycle), na.rm = TRUE)



# Create full range from 1 to max_week

olddataweek <- app_data_enrolled %>%
  filter(cycle_id == "Y4") %>%
  count(week_of_cycle, name = "n") %>%                 # don't group by start_date_cycle
  complete(week_of_cycle = 1:max_week_old, fill = list(n = 0L)) %>%
  mutate(
    start_date_cycle = start_date_202425 + (week_of_cycle - 1) * 7
  ) %>%
  arrange(week_of_cycle) %>%
  mutate(
    n_includingold = n,
    n_includingold = if_else(row_number() == 1, 224375, n_includingold),   # set first n to 252000
    cumulative_applicants = cumsum(n),
    cumulative_n_includingold = cumsum(n_includingold)
  ) %>%
  mutate(Year = "Previous Year") %>%
  ungroup()


curreentdataweek<-app_data_enrolled %>%
  filter(cycle_id == "Y5") %>%
  count(week_of_cycle, name = "n") %>%                 # don't group by start_date_cycle
  complete(week_of_cycle = 1:max_week_old, fill = list(n = 0L)) %>%
  mutate(
    start_date_cycle = start_date_202526 + (week_of_cycle - 1) * 7
  ) %>%
  arrange(week_of_cycle) %>%
  mutate(
    n_includingold = n,
    n_includingold = if_else(row_number() == 1, 251797, n_includingold),   # set first n to 252000
    cumulative_applicants = cumsum(n),
    cumulative_n_includingold = cumsum(n_includingold)
  ) %>%
  mutate(Year = "Current Year") %>%
  ungroup() %>%
  filter(! week_of_cycle > max_week_curr)


totaldata = rbind(olddataweek,curreentdataweek)

saveRDS(totaldata, file = here("tracker", "data","total_data",paste0("totaldata",date,".rds")))


diff_total<-totaldata %>%
  filter(week_of_cycle == max_week_curr) %>%
  pivot_wider(id_cols = week_of_cycle, names_from = Year, values_from = c(n, cumulative_applicants))

saveRDS(diff_total, file = here("tracker", "data","diff_total",paste0("diff_total",date,".rds")))

currentgrade_level_data<-grade_level_data %>%
  filter(cycle_id == "Y5") %>%
  group_by(week_of_cycle, Grade) %>%
  count() %>%
  ungroup() %>%
  complete(week_of_cycle = 1:max_week_old,
           Grade,
           fill = list(n = 0L)) %>%
  mutate(
    start_date_cycle = start_date_202526 + (week_of_cycle - 1) * 7
  ) %>%
  arrange(week_of_cycle) %>%
  group_by(Grade) %>%
  mutate(
    cumulative_applicants = cumsum(n) 
    ) %>%
  mutate(Year = "Current Year") %>%
  ungroup() %>%
  filter(! week_of_cycle > max_week_curr)
  

oldgrade_level_data<-grade_level_data %>%
  filter(cycle_id == "Y4") %>%
  group_by(week_of_cycle, Grade) %>%
  count() %>%
  ungroup() %>%
  complete(week_of_cycle = 1:max_week_old,
           Grade,
           fill = list(n = 0L)) %>%
  mutate(
    start_date_cycle = start_date_202425 + (week_of_cycle - 1) * 7
  ) %>%
  arrange(week_of_cycle) %>%
  group_by(Grade) %>%
  mutate(
    cumulative_applicants = cumsum(n) 
  ) %>%
  mutate(Year = "Previous Year") %>%
  ungroup() 

totalgrade = rbind(oldgrade_level_data,currentgrade_level_data)
saveRDS(totalgrade, file = here("tracker", "data","grade_levels",paste0("grade_levels",date,".rds")))

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
