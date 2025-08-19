library("mgcv")


data_total<-read.csv("tracker/data/Y4_data/olddata_test.csv")

datamodel<-readRDS("tracker/data/total_data/totaldata2025-08-19.rds")

datamodel<-datamodel %>% filter(!Year == "Goal")

datamodel<-datamodel %>%
  mutate(year = ifelse(Year == "Current Year", 2025, 2024))


enrollment_data_2024<-datamodel %>%
  filter(year == 2024) %>%
  arrange(week_of_cycle)
week_2024 <- enrollment_data_2024$week_of_cycle
y_2024 <- enrollment_data_2024$cumulative_applicants


fit_gam <- gam(y_2024 ~ s(week_2024, k=10),family = poisson(link = "log"))  # k=10 is a reasonable starting point
summary(fit_gam)

week_2025 <- 1:52  # full cycle
pred_2025 <- predict(fit_gam, newdata = data.frame(week_2024=week_2025), se.fit = TRUE)
proj_2025 <- exp(pred_2025$fit)
