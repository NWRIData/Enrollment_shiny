library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(bslib)


# Function to get latest RDS based on filename date
get_latest_rds <- function(dir_path, verbose = TRUE) {
  rds_files <- list.files(path = dir_path, pattern = "\\.rds$", full.names = TRUE)
  
  if (length(rds_files) == 0) {
    if (verbose) cat("No .rds files found in", dir_path, "\n")
    return(NULL)
  }
  
  # Extract date from filename using regex
  file_dates <- sub(".*?(\\d{4}-\\d{2}-\\d{1,2})\\.rds$", "\\1", basename(rds_files))
  
  # Convert to Date
  file_dates <- as.Date(file_dates, format = "%Y-%m-%d")
  
  latest_file <- rds_files[which.max(file_dates)]
  
  if (verbose) cat("Loaded file from:", latest_file, "\n")
  
  readRDS(latest_file)
}

# Load data
district_data   <- get_latest_rds("data/district_df")
totaldata       <- get_latest_rds("data/total_data")
diff_total_data <- get_latest_rds("data/diff_total")
lost_kids_count <- get_latest_rds("data/lostkids/count")
gradelevels<-get_latest_rds("data/grade_levels")

print("Grade levels data preview:")
print(head(gradelevels))
print(dim(gradelevels)) 

total_new_enroll<-totaldata %>%
  filter(Year == "Current Year") %>%
  pull(cumulative_applicants) %>%
  sum()

# Calculate differences
diff_week  <- diff_total_data$`n_Current Year` - diff_total_data$`n_Previous Year`
diff_total <- diff_total_data$`cumulative_applicants_Current Year` - 
  diff_total_data$`cumulative_applicants_Previous Year`

district_df <- unique(district_data$DistrictName)

currentdate <- Sys.Date()
updatetime  <- paste("Last Updated at", currentdate)

# Theme
my_theme <- bs_theme(
  version = 5,
  bootswatch = "zephyr",
  primary = "#0066cc",
  base_font = font_google("Roboto"),
  heading_font = font_google("Montserrat")
)

# UI
ui <- fluidPage(
  theme = my_theme,
  titlePanel(paste("NWRI Enrollment Progress", updatetime)),
  navset_pill_list(
    widths = c(2, 10),  # left nav 3 columns, content 9 columns
    nav_panel("Current & Total Enrollment",
              div(style = "text-align: center;",
                  h4("Total Enrollment Over Time 2025-26"),
                  h4("Current Enrollment Over Time 2025-26")
              ),
              uiOutput("enrollment_summary"),
              plotOutput("graphtotal", height = "400px"),
              plotOutput("grade", height = "400px")
              
    ),
    nav_panel("District Enrollment",
              sidebarLayout(
                sidebarPanel(width = 2,
                             selectInput("District", "Select District",
                                         choices = district_df,
                                         selected = district_df[1])
                ),
                mainPanel(width = 9,
                          h4("Enrollment Over Time by District"),
                          plotOutput("fc_plot", height = "400px")
                )
              )
    ),
    nav_panel("Lost Kids",
              uiOutput("lost_kids_summary")
    )
  )
)

# Server
server <- function(input, output){
  
  # Enrollment summary UI
  output$enrollment_summary <- renderUI({
    enrollment_cumulative <- diff_total
    directioncum <- if (enrollment_cumulative >= 0) "ahead" else "behind"
    cumulative_abs_cum <- abs(enrollment_cumulative)
    color <- if (enrollment_cumulative >= 0) "green" else "red"
    
    HTML(
      paste0(
        "<div style='font-size: 20px;'>We are currently ",
        "<span style='color:", color, "; font-weight: bold;'>",
        cumulative_abs_cum, " enrollees ", directioncum,
        "</span> compared to the same time last year.</div>"
      )
    )
  })
  
  # Lost kids summary UI
  output$lost_kids_summary <- renderUI({
    lost_count <- as.numeric(lost_kids_count)
    div(
      style = "text-align:center; margin-top: 50px;",
      HTML(paste0(
        "<div style='font-size: 60px; font-weight: bold; color: #d9534f;'>",
        format(lost_count, big.mark = ","),
        "</div>",
        "<div style='font-size: 20px; color: #555;'>Lost Kids</div>"
      ))
    )
  })
  
  # Data for total enrollment plot
  graph_total <- reactive({ totaldata })
  total_new_enroll <- reactive({
    totaldata %>%
      filter(Year == "Current Year") %>%
      pull(n) %>%
      sum()
  })
  
  # Total enrollment plot
  output$graphtotal <- renderPlot({
    ggplot(data = NULL) +
      geom_area(data = graph_total(), aes(x = week_of_cycle,
                                          y = cumulative_applicants,
                                          fill = Year)) +
      geom_line(data = graph_total(), aes(x = week_of_cycle,
                                          y = cumulative_applicants,
                                          fill = Year)) +
      annotate("text",
               x = Inf, y = Inf,
               label = paste0("New enrolled students: ", format(total_new_enroll(), big.mark = ",")),
               hjust = 1.3, vjust = 10, # adjust positioning relative to plot edges
               size = 9, color = "black") +
      labs(title = "Enrollment Over Time",
           x = "Weeks", y = "Enrollment") +
      theme_minimal() +
      theme(axis.title = element_text(size = 16))
  })
  
  #  grade plot
  grade_total <- reactive({ gradelevels })
  output$grade <-renderPlot({
    ggplot(data = NULL) +
      geom_line(data = grade_total(), aes(x=week_of_cycle,
                                        y= cumulative_applicants,
                                        color = Grade)) +
      
      facet_wrap(~Year, nrow = 2,scales = "free_y") +
      theme_light() +
      labs(title = "Enrollment Over Time",
           x = "Weeks", y = "Enrollment") +
      theme_minimal() +
      theme(axis.title = element_text(size = 16))
  })
  
  # Filtered dataset for district plot
  filtered_data <- reactive({
    district_data[district_data$DistrictName == input$District, ]
  })
  
  # District plot
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

# Run the app
shinyApp(ui, server)
