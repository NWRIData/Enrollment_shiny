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
  bootswatch = "minty",
  primary = "#0072e3",
  base_font = font_google("Poppins"),
  heading_font = font_google("Poppins")
) |> 
  bs_add_rules("
    .navbar {
      background-color: #004b9b !important; /* navbar fill color */
    }
    .navbar-brand {
      color: white !important;
      font-weight: 700;
      letter-spacing: 0.5px;
    }
    .navbar-nav .nav-link {
      color: white !important;
      font-weight: 500;
    }
    .navbar-nav .nav-link.active {
      background-color: rgba(255,255,255,0.15);
      border-radius: 0.5rem;
    }
  ")
ui <- page_sidebar(
  title = paste("NWRI Enrollment Progress", Sys.Date()),
  theme = my_theme,
  collapsible = TRUE,
  
  # Sidebar navigation as clickable menu
  sidebar = sidebar(
    navset_tab(
      id = "nav",
      nav_panel("Current & Total Enrollment", value = "current_total"),
      nav_panel("Enrollment by grade", value = "by_grade"),
      nav_panel("District Enrollment", value = "district"),
      nav_panel("Lost Kids", value = "lost_kids")
    )
  ),
  
  # Main content dynamically rendered
  uiOutput("tab_content")
)

server <- function(input, output, session){
  
  # Dynamic tab content
  output$tab_content <- renderUI({
    switch(input$nav,
           
           "current_total" = tagList(
             uiOutput("enrollment_summary"),
             div(style = "text-align: center;",
                 h4("Enrollment Projections 2025-26")),
             plotOutput("graphtotalold", height = "400px"),
             div(style = "text-align: center;",
                 h4("Current New Enrollments 2025-26")),
             plotOutput("graphtotal", height = "400px")
           ),
           
           "by_grade" = plotOutput("grade", height = "600px"),
           
           "district" = tagList(
             h4("Enrollment Over Time by District"),
             uiOutput("district_selector"),
             plotOutput("fc_plot", height = "400px")
           ),
           
           "lost_kids" = uiOutput("lost_kids_summary")
    )
  })
  
  # Reactive values for lazy-loading
  district_data   <- reactiveVal(NULL)
  totaldata       <- reactiveVal(NULL)
  diff_total_data <- reactiveVal(NULL)
  lost_kids_count <- reactiveVal(NULL)
  gradelevels     <- reactiveVal(NULL)
  pm_window       <- reactiveVal(NULL)
  
  # Load data lazily when a tab is opened
  observeEvent(input$nav, {
    if (input$nav == "current_total" && is.null(totaldata())) {
      totaldata(get_latest_rds("data/total_data"))
      diff_total_data(get_latest_rds("data/diff_total"))
      pm_window(get_latest_rds("data/PM_window_info"))
    }
    if (input$nav == "by_grade" && is.null(gradelevels())) {
      gradelevels(get_latest_rds("data/grade_levels"))
    }
    if (input$nav == "district" && is.null(district_data())) {
      district_data(get_latest_rds("data/district_df"))
    }
    if (input$nav == "lost_kids" && is.null(lost_kids_count())) {
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
                    color = Year), size = 1.5) +
      geom_point(data = totaldata(),
                 aes(x = week_of_cycle, y = cumulative_n_includingold,
                     fill = Year), size = 3, pch = 21, color = "white") +
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
      geom_point(aes(x = week_of_cycle, y = cumulative_applicants,
                     fill = Grade), size = 3, pch = 21, color = "white") +
      facet_wrap(~Year, nrow = 2, scales = "free_y") +
      labs(title = "Enrollment Over Time", x = "Weeks", y = "Enrollment") +
      theme_minimal() +
      theme(axis.title = element_text(size = 16)) +
      scale_color_viridis_d(option = "H") +
      scale_fill_viridis_d(option = "H")
  })
  
  # District selector (dynamic)
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

shinyApp(ui, server)