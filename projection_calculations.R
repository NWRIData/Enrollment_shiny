#projection
library(tidyverse)
projdata = readRDS("tracker/data/total_data/totaldata2026-03-18.rds")
projdata_current = projdata %>%
  filter(Year == "Current Year")

plot(projdata_current$week_of_cycle, projdata_current$cumulative_applicants)
plot(projdata_current$week_of_cycle, projdata_current$cumulative_n_includingold)


projfinal_df=projdata_current %>%
  select(start_date_cycle,week_of_cycle, cumulative_n_includingold, n)
dput(projfinal_df)


library(dplyr)
library(ggplot2)

# Load your initial data
df <-projfinal_df
# 2. Calculate the average weekly growth for the last 2, 4, and 8 weeks
avg_2_wk <- mean(tail(df$n, 2))
avg_4_wk <- mean(tail(df$n, 4))
avg_8_wk <- mean(tail(df$n, 8))

# 3. Identify the starting point for projections (Week 38)
last_week <- max(df$week_of_cycle)
last_cum <- max(df$cumulative_n_includingold)

# 4. Create the projection dataset (Weeks 38 to 52)
# Starting at week 38 allows the lines to connect seamlessly in ggplot
proj_df <- data.frame(
  week_of_cycle = last_week:52
) %>%
  mutate(
    # Week 38 gets 0 multiplier, Week 39 gets 1, Week 40 gets 2, etc.
    weeks_ahead = week_of_cycle - last_week, 
    
    # Calculate the cumulative projections
    proj_2wk = last_cum + (weeks_ahead * avg_2_wk),
    proj_4wk = last_cum + (weeks_ahead * avg_4_wk),
    proj_8wk = last_cum + (weeks_ahead * avg_8_wk)
  )

# 5. Plot the data using ggplot2
ggplot() +
  # Actual historical data (Solid line)
  geom_line(data = df, 
            aes(x = week_of_cycle, y = cumulative_n_includingold, color = "Actual Historical"), 
            linewidth = 1.2) +
  
  # 2-Week Average Projection (Dashed line)
  geom_line(data = proj_df, 
            aes(x = week_of_cycle, y = proj_2wk, color = "2-Week Avg Projection"), 
            linetype = "dashed", linewidth = 1) +
  
  # 4-Week Average Projection (Dashed line)
  geom_line(data = proj_df, 
            aes(x = week_of_cycle, y = proj_4wk, color = "4-Week Avg Projection"), 
            linetype = "dashed", linewidth = 1) +
  
  # 8-Week Average Projection (Dashed line)
  geom_line(data = proj_df, 
            aes(x = week_of_cycle, y = proj_8wk, color = "8-Week Avg Projection"), 
            linetype = "dashed", linewidth = 1) +
  
  # Customize colors and legend labels
  scale_color_manual(
    name = "Enrollment Scenario",
    values = c(
      "Actual Historical" = "black",
      "2-Week Avg Projection" = "dodgerblue",
      "4-Week Avg Projection" = "darkorange",
      "8-Week Avg Projection" = "forestgreen"
    )
  ) +
  
  # Labels and formatting
  labs(
    title = "Cumulative Enrollment Forecast (Projections to Week 52)",
    x = "Week of Cycle",
    y = "Cumulative Enrollment"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )




####Slowed growth
# 2. Calculate the average weekly growth for the last 2, 4, and 8 weeks
avg_2_wk <- mean(tail(df$n, 2))
avg_4_wk <- mean(tail(df$n, 4))
avg_8_wk <- mean(tail(df$n, 8))

# 3. Identify the starting point for projections (Week 38)
last_week <- max(df$week_of_cycle)
last_cum <- max(df$cumulative_n_includingold)
decay_rate <- 0.05 # 5% decrease per week

# 4. Create the projection dataset (Weeks 38 to 52)
proj_df <- data.frame(
  week_of_cycle = last_week:52
) %>%
  mutate(
    weeks_ahead = week_of_cycle - last_week, 
    
    # Calculate the marginalized new enrollees per week (applying the 5% decay formula)
    # The 'ifelse' ensures we add 0 for week 38 to properly anchor the line
    new_enrollees_2wk = ifelse(weeks_ahead == 0, 0, avg_2_wk * ((1 - decay_rate) ^ weeks_ahead)),
    new_enrollees_4wk = ifelse(weeks_ahead == 0, 0, avg_4_wk * ((1 - decay_rate) ^ weeks_ahead)),
    new_enrollees_8wk = ifelse(weeks_ahead == 0, 0, avg_8_wk * ((1 - decay_rate) ^ weeks_ahead)),
    
    # Sum up the new enrollees cumulatively starting from the last known actual total
    proj_2wk = last_cum + cumsum(new_enrollees_2wk),
    proj_4wk = last_cum + cumsum(new_enrollees_4wk),
    proj_8wk = last_cum + cumsum(new_enrollees_8wk)
  )

# 5. Plot the data using ggplot2
ggplot() +
  # Actual historical data (Solid line)
  geom_line(data = df, 
            aes(x = week_of_cycle, y = cumulative_n_includingold, color = "Actual Historical"), 
            linewidth = 1.2) +
  
  # 2-Week Average Projection (Dashed line)
  geom_line(data = proj_df, 
            aes(x = week_of_cycle, y = proj_2wk, color = "2-Week Avg (5% Decay)"), 
            linetype = "dashed", linewidth = 1) +
  
  # 4-Week Average Projection (Dashed line)
  geom_line(data = proj_df, 
            aes(x = week_of_cycle, y = proj_4wk, color = "4-Week Avg (5% Decay)"), 
            linetype = "dashed", linewidth = 1) +
  
  # 8-Week Average Projection (Dashed line)
  geom_line(data = proj_df, 
            aes(x = week_of_cycle, y = proj_8wk, color = "8-Week Avg (5% Decay)"), 
            linetype = "dashed", linewidth = 1) +
  
  # Customize colors and legend labels
  scale_color_manual(
    name = "Enrollment Scenario",
    values = c(
      "Actual Historical" = "black",
      "2-Week Avg (5% Decay)" = "dodgerblue",
      "4-Week Avg (5% Decay)" = "darkorange",
      "8-Week Avg (5% Decay)" = "forestgreen"
    )
  ) +
  
  # Labels and formatting
  labs(
    title = "Pessimistic Enrollment Forecast (5% Weekly Decline)",
    x = "Week of Cycle",
    y = "Cumulative Enrollment"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )
