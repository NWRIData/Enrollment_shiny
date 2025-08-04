library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
require(lubridate)

#load old data
olddata <- read.csv("data/olddata_test.csv")
#load most recent data
recentdata<-read.csv("data/enrollments_20250728.csv",
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
  group_by(week_of_cycle) %>%
  count(name = "n") %>%
  ungroup() %>%
  complete(week_of_cycle = 1:max_week_old, fill = list(n = 0)) %>%
  arrange(week_of_cycle) %>%
  mutate(
    cumulative_applicants = cumsum(n)
  ) %>%
  mutate(Year = "Previous Year")


curreentdataweek <- app_data_enrolled %>%
  filter(cycle_id == "Y5") %>%
  group_by(week_of_cycle) %>%
  count(name = "n") %>%
  ungroup() %>%
  complete(week_of_cycle = 1:max_week_curr, fill = list(n = 0)) %>%
  arrange(week_of_cycle) %>%
  mutate(
    cumulative_applicants = cumsum(n)
  ) %>%
  mutate(Year = "Current Year")

totaldata = rbind(olddataweek,curreentdataweek)

diff_total<-totaldata %>%
  filter(week_of_cycle == max_week_curr) %>%
  pivot_wider(id_cols = week_of_cycle, names_from = Year, values_from = c(n, cumulative_applicants))

diff_week<-diff_total$`n_Current Year`-diff_total$`n_Previous Year`
diff_total<-diff_total$`cumulative_applicants_Current Year`-diff_total$`cumulative_applicants_Previous Year`

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

district_df<-unique(district_data$DistrictName)

currentdate<-Sys.time()
updatetime<-paste("Last Updated at", currentdate)
ui <- fluidPage(
  titlePanel(paste("Enrollment Progress NWRI", updatetime)),
  tabsetPanel(
    tabPanel("Total Enrollment",
             fluidRow(
               column(width = 12,
                      textOutput("enrollment_summary"),
                      h4("Total Enrollment Over Time"),
                      plotOutput("graphtotal", height = "400px")
               )
             )
    ),
    tabPanel("District Enrollment",
             sidebarLayout(
               sidebarPanel(width = 3,
                            selectInput("District", "Select District",
                                        choices = district_df,
                                        selected = district_df[1])
               ),
               mainPanel(width = 9,
                         h4("Enrollment Over Time by District"),
                         plotOutput("fc_plot", height = "400px")
               )
             )
    )
  )
)




server <- function(input, output){
  output$enrollment_summary <- renderText({
    enrollment_week <- diff_week  # Replace with actual variable or calculation
    enrollment_cumulative <- diff_total  # Same here
    
    direction <- if (enrollment_cumulative >= 0) "more" else "fewer"
    cumulative_abs <- abs(enrollment_cumulative)
    
    paste0(
      "This week, we have ", enrollment_week, 
      " enrollees compared to the same day last year, and we also have ", 
      cumulative_abs, " ", direction, 
      " enrollees since the beginning of this year."
    )
  })
  
  graph_total = reactive({totaldata})
  # Render the plot
  output$graphtotal <- renderPlot({
    ggplot(data = NULL) +
      geom_area(data = graph_total(), aes(x = week_of_cycle,
                                            y = cumulative_applicants,
                                            fill = Year)) +
      labs(title = paste("Enrollment Over Time"),
           x = "Date", y = "Enrollment") +
      theme_minimal()
  })
  
  # Reactive dataset based on selected district
  filtered_data <- reactive({
    district_data[district_data$DistrictName == input$District, ]
  })
  
  # Render the plot
  output$fc_plot <- renderPlot({
    ggplot(data = NULL) +
      geom_area(data = filtered_data(), aes(x = week_of_cycle,
                                            y = cumulative_applicants,
                                            fill = Year)) +
      labs(title = paste("Enrollment Over Time -", input$District),
           x = "Date", y = "Enrollment") +
      theme_minimal()
  })
}

shinyApp(ui, server)

