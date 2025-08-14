
# Load data
district_data   <- readRDS("tracker/data/district_df/district_df2025-08-12.rds")
totaldata       <- readRDS("tracker/data/total_data/totaldata2025-08-13.rds")
diff_total_data <- readRDS("tracker/data/diff_total/diff_total2025-08-12.rds")
lost_kids_count <- readRDS("tracker/data/lostkids/count/lost_kids_count2025-08-12.rds")
lost_kids_count <- read.csv("tracker/data/enrollments_20250813.csv")


gradelevels <- readRDS("tracker/data/grade_levels/grade_levels2025-08-14.rds")

wtf<-gradelevels %>%
  group_by(Year,week_of_cycle, start_date_cycle) %>%
  mutate(perc = 100*round(n/sum(n)))


ggplot() +
  geom_line(data = gradelevels, aes(x=week_of_cycle,
                                    y= cumulative_applicants,
                                    color = Grade)) +
  
  facet_wrap(~Year, nrow = 2,scales = "free_y") +
  theme_light()

#make a graph of the moving average by week for all the. data
ggplot() +
  geom_col(data = totaldata, aes(x=week_of_cycle, y= cumulative_n_includingold)) +
  geom_rug(data = totaldata %>%
             filter(!n== 0), aes(x=week_of_cycle,
                                 color = log(n)),linewidth = 2) + 
  facet_wrap(~Year, nrow = 2,scales = "free_y") +
  scale_color_viridis_c() +
  theme_light()

