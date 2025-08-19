library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(bslib)
library(viridis)
library(scales)

# Function to get latest RDS based on filename date
get_latest_rds <- function(dir_path, verbose = TRUE) {
  rds_files <- list.files(path = dir_path, pattern = "\\.rds$", full.names = TRUE)
  
  if (length(rds_files) == 0) {
    if (verbose) cat("No .rds files found in", dir_path, "\n")
    return(NULL)
  }
  
  file_dates <- sub(".*?(\\d{4}-\\d{2}-\\d{1,2})\\.rds$", "\\1", basename(rds_files))
  file_dates <- as.Date(file_dates, format = "%Y-%m-%d")
  
  latest_file <- rds_files[which.max(file_dates)]
  
  if (verbose) cat("Loaded file from:", latest_file, "\n")
  readRDS(latest_file)
}

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
    widths = c(2, 10),
    id = "nav",   # important: gives us input$nav to know which tab is open
    nav_panel("Current & Total Enrollment",
              uiOutput("enrollment_summary"),
              div(style = "text-align: center;",
                  h4("Enrollment Projections 2025-26")),
              plotOutput("graphtotalold", height = "400px"),
              div(style = "text-align: center;",
                  h4("Current New Enrollments 2025-26")),
              plotOutput("graphtotal", height = "400px")
    ),
    nav_panel("Enrollment by grade",
              plotOutput("grade", height = "600px")
    ),
    nav_panel("District Enrollment",
              sidebarLayout(
                sidebarPanel(width = 2,
                             uiOutput("district_selector")),
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
server <- function(input, output, session){
  # Create reactiveVals to load data only when needed
  district_data   <- reactiveVal(NULL)
  totaldata       <- reactiveVal(NULL)
  diff_total_data <- reactiveVal(NULL)
  lost_kids_count <- reactiveVal(NULL)
  gradelevels     <- reactiveVal(NULL)
  pm_window       <- reactiveVal(NULL)
  
  # Load data lazily when tab is opened
  observeEvent(input$nav, {
    if (input$nav == "Current & Total Enrollment" && is.null(totaldata())) {
      totaldata(get_latest_rds("data/total_data"))
      diff_total_data(get_latest_rds("data/diff_total"))
      pm_window(get_latest_rds("data/PM_window_info"))
    }
    if (input$nav == "Enrollment by grade" && is.null(gradelevels())) {
      gradelevels(get_latest_rds("data/grade_levels"))
    }
    if (input$nav == "District Enrollment" && is.null(district_data())) {
      district_data(get_latest_rds("data/district_df"))
    }
    if (input$nav == "Lost Kids" && is.null(lost_kids_count())) {
      lost_kids_count(get_latest_rds("data/lostkids/count"))
    }
  })
  
  # Enrollment summary
  output$enrollment_summary <- renderUI({
    req(diff_total_data())
    diff_total <- diff_total_data()$`cumulative_applicants_Current Year` -
      diff_total_data()$`cumulative_applicants_Previous Year`
    directioncum <- if (diff_total >= 0) "ahead" else "behind"
    color <- if (diff_total >= 0) "green" else "red"
    
    HTML(paste0(
      "<div style='font-size: 20px;'>We are currently ",
      "<span style='color:", color, "; font-weight: bold;'>",
      abs(diff_total), " enrollees ", directioncum,
      "</span> compared to the same time last year.</div>"
    ))
  })
  
  # Lost kids summary
  output$lost_kids_summary <- renderUI({
    req(lost_kids_count())
    lost_count <- as.numeric(lost_kids_count())
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
  
  # Total enrollment plots
  graph_total <- reactive({
    req(totaldata())
    totaldata() %>% filter(! Year == "Goal")
  })
  
  total_new_enroll <- reactive({
    req(totaldata())
    totaldata() %>%
      filter(Year == "Current Year") %>%
      pull(n) %>%
      sum()
  })
  
  output$graphtotalold <- renderPlot({
    req(totaldata())
    ggplot(data = NULL) +
      geom_rect(aes(xmin = 8,xmax = 14, ymin = -Inf, ymax = Inf),
                fill = "grey", alpha = 0.2) +
      annotate("text", x = 11, y = 400000, label = "PM1", size = 9) +
      geom_rect(aes(xmin = 25,xmax = 31, ymin = -Inf, ymax = Inf),
                fill = "grey", alpha = 0.2) +
      annotate("text", x = 28, y = 400000, label = "PM2", size = 9) +
      geom_rect(aes(xmin = 44,xmax = 49, ymin = -Inf, ymax = Inf),
                fill = "grey", alpha = 0.2) +
      annotate("text", x = 46.5, y = 400000, label = "PM3", size = 9) +
      geom_line(data = totaldata(),
                aes(x = week_of_cycle, y = cumulative_n_includingold,
                    color = Year, lty = Year), size = 1.5) +
      labs(title = "Enrollment Over Time", x = "Weeks", y = "Enrollment") +
      scale_y_continuous(labels = scales::label_number(suffix = "K", scale = 1e-3)) +
      theme_minimal() +
      theme(axis.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.title = element_blank())
  })
  
  output$graphtotal <- renderPlot({
    req(graph_total())
    ggplot(data = NULL) +
      geom_area(data = graph_total(),
                aes(x = week_of_cycle, y = cumulative_applicants, fill = Year)) +
      geom_line(data = graph_total(),
                aes(x = week_of_cycle, y = cumulative_applicants, fill = Year)) +
      annotate("text", x = Inf, y = Inf,
               label = paste0("New enrolled students: ",
                              format(total_new_enroll(), big.mark = ",")),
               hjust = 1.3, vjust = 10, size = 9, color = "black") +
      labs(title = "Enrollment Over Time", x = "Weeks", y = "Enrollment") +
      theme_minimal() +
      theme(axis.title = element_text(size = 16))
  })
  
  # Grade plot
  output$grade <- renderPlot({
    req(gradelevels())
    ggplot(data = gradelevels()) +
      geom_line(aes(x = week_of_cycle, y = cumulative_applicants, color = Grade),
                size = 1.5) +
      facet_wrap(~Year, nrow = 2, scales = "free_y") +
      labs(title = "Enrollment Over Time", x = "Weeks", y = "Enrollment") +
      theme_minimal() +
      theme(axis.title = element_text(size = 16)) +
      scale_color_viridis_d(option = "H")
  })
  
  # District selector (dynamic after data loads)
  output$district_selector <- renderUI({
    req(district_data())
    selectInput("District", "Select District",
                choices = unique(district_data()$DistrictName),
                selected = unique(district_data()$DistrictName)[1])
  })
  
  filtered_data <- reactive({
    req(district_data(), input$District)
    district_data()[district_data()$DistrictName == input$District, ]
  })
  
  output$fc_plot <- renderPlot({
    req(filtered_data())
    ggplot(data = filtered_data()) +
      geom_area(aes(x = week_of_cycle, y = cumulative_applicants, fill = Year)) +
      labs(title = paste("Enrollment Over Time -", input$District),
           x = "Date", y = "Enrollment") +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui, server)
