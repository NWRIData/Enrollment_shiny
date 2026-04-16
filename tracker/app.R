library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(bslib)
library(viridis)
library(scales)
library(shinycssloaders)

# Smart fetch function that works backward from today's date to find the latest file
fetch_latest_github_rds <- function(sub_folder, file_prefix, max_days_back = 14) {
  base_url <- "https://raw.githubusercontent.com/NWRIData/Enrollment_shiny/main/tracker/data/"
  
  for (i in 0:max_days_back) {
    test_date <- Sys.Date() - i
    target_path <- paste0(sub_folder, "/", file_prefix, test_date, ".rds")
    full_url <- paste0(base_url, target_path)
    
    tryCatch({
      suppressWarnings({
        my_data <- readRDS(url(full_url))
      })
      cat("Successfully loaded:", full_url, "\n")
      return(my_data)
    }, error = function(e) {
      # Loop continues if file not found
    })
  }
  
  cat("Could not find any files for", file_prefix, "in the last", max_days_back, "days.\n")
  return(NULL)
}

currentdate <- Sys.Date()
updatetime  <- paste("Last Updated at", currentdate)

# Theme
my_theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  primary = "#002657",
  base_font = font_google("Poppins"),
  heading_font = font_google("Poppins")
) |> 
  bs_add_rules("
    .navbar {
      background-color: #002657 !important; /* navbar fill color */
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
  title = paste("New Worlds Reading Dashboard"),
  theme = my_theme,
  collapsible = TRUE,
  # Add custom CSS
  tags$head(
    tags$style(HTML("
      /* Change active tab text color */
      .nav-link.active {
        color: #FA4616 !important;
        font-weight: bold;
      }
      
      /* Optional: subtle hover color for consistency */
      .nav-link:hover {
        color: #FA4616 !important;
      }
    "))
  ),
  
  # Sidebar navigation as clickable menu
  sidebar = sidebar(
    navset_tab( footer = HTML(
      paste0(
        "<span style='font-size: 0.85em; font-style: italic;'>Last updated at: ",
        Sys.Date(),
        "</span>"
      )
    ),
    id = "nav",
    nav_panel("Current & Total Enrollment", value = "current_total"),
    nav_panel("District Enrollment", value = "district"),
    nav_panel("Enrollment by grade", value = "by_grade"),
    nav_panel("Lost Kids", value = "lost_kids")
    )
  ),
  # Main content dynamically rendered
  uiOutput("tab_content")
)

server <- function(input, output, session){
  
  # Dynamic tab content
  output$tab_content <- renderUI({
    if (input$nav == "current_total") {
      tagList(
        uiOutput("enrollment_summary") %>% withSpinner(type = 8, size = 0.5),
        card(
          full_screen = TRUE,
          card_header("Enrollment Details"),
          navset_tab(
            id = "enrollment_tabs",
            # --- ENROLLMENT PROJECTION TAB ---
            nav_panel("Enrollment Projection",
                      layout_sidebar(
                        sidebar = sidebar(
                          title = "Forecast Settings",
                          radioButtons("proj_scenario", "Scenario:",
                                       choices = c("Business as Usual" = "bau",
                                                   "Decay Model (Pessimistic)" = "decay")),
                          conditionalPanel(
                            condition = "input.proj_scenario == 'decay'",
                            sliderInput("decay_rate", "Weekly Decay Rate (%):",
                                        min = 1, max = 20, value = 5, step = 1),
                            div(
                              style = "font-size: 0.85em; color: #555; margin-top: 10px; line-height: 1.4;",
                              strong("What does this mean? "), 
                              "This model assumes our recent enrollment momentum will gradually cool off over time.",
                              br(), br(),
                              "For example, a ", strong("5% decay"), " means if we enrolled 1,000 new students this week, we expect to enroll 950 next week, 902 the week after, and so on. A higher percentage means a faster drop-off."
                            )
                          )
                        ),
                        div(style = "text-align:center;",
                            h4("Enrollment Projections 2025-26")),
                        withSpinner(plotOutput("graphtotalold", height = "400px",
                                               brush = brushOpts(id = "plot_brush", resetOnNew = FALSE),
                                               click = "plot_click")),
                        
                        uiOutput("proj_final_boxes"),
                        br(),
                        
                        tableOutput("selected_points") 
                      )
            ),
            # -----------------------------------------
            nav_panel("Current New Enrollments",
                      div(style = "text-align:center;",
                          h4("Current New Enrollments 2025-26")),
                      withSpinner(plotOutput("graphtotal", height = "400px"))
            )
          )
        )
      )
    } else if (input$nav == "district") {
      tagList(
        h4("Enrollment Over Time by District"),
        uiOutput("district_selector"),
        withSpinner(plotOutput("fc_plot", height = "400px"))
      )
    } else if (input$nav == "by_grade") {
      withSpinner(plotOutput("grade", height = "600px"))
    } else if (input$nav == "lost_kids") {
      uiOutput("lost_kids_summary") %>% withSpinner(type = 8, size = 0.5)
    }
  })
  
  # Reactive values for lazy-loading
  district_data   <- reactiveVal(NULL)
  totaldata       <- reactiveVal(NULL)
  diff_total_data <- reactiveVal(NULL)
  lost_kids_count <- reactiveVal(NULL)
  gradelevels     <- reactiveVal(NULL)
  pm_window       <- reactiveVal(NULL)
  selected_points <- reactiveVal(NULL)
  
  # Load data lazily from GitHub when a tab is opened
  observeEvent(input$nav, {
    if (input$nav == "current_total" && is.null(totaldata())) {
      totaldata(fetch_latest_github_rds("total_data", "totaldata"))
      diff_total_data(fetch_latest_github_rds("diff_total", "diff_total"))
      pm_window(fetch_latest_github_rds("PM_window_info", "pm_window_info"))
    }
    if (input$nav == "by_grade" && is.null(gradelevels())) {
      gradelevels(fetch_latest_github_rds("grade_levels", "grade_levels"))
    }
    if (input$nav == "district" && is.null(district_data())) {
      district_data(fetch_latest_github_rds("district_df", "district_df"))
    }
    if (input$nav == "lost_kids" && is.null(lost_kids_count())) {
      lost_kids_count(fetch_latest_github_rds("lostkids/count", "lost_kids_count"))
    }
  })
  
  output$enrollment_summary <- renderUI({
    req(diff_total_data())
    diff_total <- diff_total_data()$`cumulative_applicants_Current Year` -
      diff_total_data()$`cumulative_applicants_Previous Year`
    directioncum <- if (diff_total >= 0) "ahead" else "behind"
    color <- if (diff_total >= 0) "green" else "red"
    
    HTML(paste0(
      "<div style='font-size: 20px;'>We are currently ",
      "<span style='color:", color, "; font-weight: bold;'>",
      formatC(abs(diff_total), format = "f", big.mark = ",", digits = 0), 
      " enrollees ", directioncum,
      "</span> compared to the same time last year.</div>"
    ))
  })
  
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
  
  proj_df <- reactive({
    req(totaldata(), input$proj_scenario)
    
    d_curr <- totaldata() %>% 
      filter(Year == "Current Year") %>% 
      arrange(week_of_cycle)
    
    req(nrow(d_curr) > 0)
    
    last_week <- max(d_curr$week_of_cycle, na.rm = TRUE)
    last_cum <- max(d_curr$cumulative_n_includingold, na.rm = TRUE)
    end_week <- max(totaldata()$week_of_cycle, na.rm = TRUE)
    
    avg_2_wk <- mean(tail(d_curr$n, 2), na.rm = TRUE)
    avg_4_wk <- mean(tail(d_curr$n, 4), na.rm = TRUE)
    avg_8_wk <- mean(tail(d_curr$n, 8), na.rm = TRUE)
    
    decay <- if (input$proj_scenario == "decay") (req(input$decay_rate) / 100) else 0
    
    data.frame(week_of_cycle = last_week:end_week) %>%
      mutate(
        weeks_ahead = week_of_cycle - last_week,
        n_2wk = ifelse(weeks_ahead == 0, 0, avg_2_wk * ((1 - decay) ^ weeks_ahead)),
        n_4wk = ifelse(weeks_ahead == 0, 0, avg_4_wk * ((1 - decay) ^ weeks_ahead)),
        n_8wk = ifelse(weeks_ahead == 0, 0, avg_8_wk * ((1 - decay) ^ weeks_ahead)),
        proj_2wk = last_cum + cumsum(n_2wk),
        proj_4wk = last_cum + cumsum(n_4wk),
        proj_8wk = last_cum + cumsum(n_8wk)
      )
  })
  
  observeEvent(input$plot_brush, {
    brush <- input$plot_brush
    req(brush)
    d <- totaldata()
    sel <- brushedPoints(d, brush, xvar = "week_of_cycle", yvar = "cumulative_n_includingold")
    
    if ("start_date_cycle" %in% names(sel)) {
      sel$start_date_cycle <- as.Date(sel$start_date_cycle, origin = "1970-01-01")
    }
    
    if (nrow(sel) > 0) {
      selected_points(sel)
    } else {
      selected_points(NULL)
    }
  })
  
  observeEvent(input$plot_click, {
    selected_points(NULL)
  })
  
  output$graphtotalold <- renderPlot({
    req(totaldata(), proj_df())
    d <- totaldata()
    p_df <- proj_df()
    
    base_plot <- ggplot() +
      geom_rect(aes(xmin = 8,xmax = 14, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.2) +
      annotate("text", x = 11, y = 400000, label = "PM1", size = 9) +
      geom_rect(aes(xmin = 25,xmax = 31, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.2) +
      annotate("text", x = 28, y = 400000, label = "PM2", size = 9) +
      geom_rect(aes(xmin = 44,xmax = 49, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.2) +
      annotate("text", x = 46.5, y = 400000, label = "PM3", size = 9) +
      
      geom_line(data = d, aes(x = week_of_cycle, y = cumulative_n_includingold, color = Year), size = 1.5) +
      geom_point(data = d, aes(x = week_of_cycle, y = cumulative_n_includingold, fill = Year), size = 3, pch = 21, color = "white") +
      
      geom_line(data = p_df, aes(x = week_of_cycle, y = proj_2wk, linetype = "2-Wk Avg"), color = "#FF3366", size = 1.2) +
      geom_line(data = p_df, aes(x = week_of_cycle, y = proj_4wk, linetype = "4-Wk Avg"), color = "#00C4CC", size = 1.2) +
      geom_line(data = p_df, aes(x = week_of_cycle, y = proj_8wk, linetype = "8-Wk Avg"), color = "#6633FF", size = 1.2) +
      
      labs(title = "Enrollment Over Time", x = "Weeks", y = "Enrollment") +
      scale_colour_manual(values = c("Current Year" = "#1B9E77", "Previous Year" = "#D95F02", "Goal" = "#7570B3")) +
      scale_fill_manual(values = c("Current Year" = "#1B9E77", "Previous Year" = "#D95F02", "Goal" = "#7570B3")) +
      scale_linetype_manual(name = "Projections", values = c("2-Wk Avg" = "dashed", "4-Wk Avg" = "dashed", "8-Wk Avg" = "dashed")) +
      theme_minimal() +
      theme(axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 16),
            legend.title = element_blank())
    
    if (!is.null(selected_points())) {
      np <- selected_points()
      base_plot <- base_plot +
        geom_point(data = np, aes(x = week_of_cycle, y = cumulative_n_includingold), color = "orange", size = 4) +
        geom_text(data = np, aes(x = week_of_cycle, y = cumulative_n_includingold, label = scales::comma(cumulative_n_includingold)),
                  vjust = -1, color = "orange", size = 5, fontface = "bold")
    }
    
    base_plot
  })
  
  output$proj_final_boxes <- renderUI({
    req(proj_df())
    final_data <- proj_df() %>% filter(week_of_cycle == max(week_of_cycle, na.rm = TRUE))
    
    val_2wk <- format(round(final_data$proj_2wk), big.mark = ",")
    val_4wk <- format(round(final_data$proj_4wk), big.mark = ",")
    val_8wk <- format(round(final_data$proj_8wk), big.mark = ",")
    
    HTML(paste0(
      "<div style='display: flex; justify-content: space-around; margin-top: 15px; margin-bottom: 15px;'>",
      "<div style='background-color: #FF3366; color: white; padding: 15px; border-radius: 8px; text-align: center; width: 30%; box-shadow: 0 4px 6px rgba(0,0,0,0.1);'>",
      "<h6 style='margin-bottom: 5px; opacity: 0.9;'>Final Projected (2-Wk Avg)</h6>",
      "<h3 style='font-weight: bold; margin: 0;'>", val_2wk, "</h3></div>",
      "<div style='background-color: #00C4CC; color: white; padding: 15px; border-radius: 8px; text-align: center; width: 30%; box-shadow: 0 4px 6px rgba(0,0,0,0.1);'>",
      "<h6 style='margin-bottom: 5px; opacity: 0.9;'>Final Projected (4-Wk Avg)</h6>",
      "<h3 style='font-weight: bold; margin: 0;'>", val_4wk, "</h3></div>",
      "<div style='background-color: #6633FF; color: white; padding: 15px; border-radius: 8px; text-align: center; width: 30%; box-shadow: 0 4px 6px rgba(0,0,0,0.1);'>",
      "<h6 style='margin-bottom: 5px; opacity: 0.9;'>Final Projected (8-Wk Avg)</h6>",
      "<h3 style='font-weight: bold; margin: 0;'>", val_8wk, "</h3></div></div>"
    ))
  })
  
  output$selected_points <- renderTable({
    req(selected_points(), totaldata())
    selected_points() %>%
      mutate(start_date_cycle=format(as.Date(start_date_cycle, "%Y-%m-%d"))) |>
      relocate(start_date_cycle, .before = week_of_cycle) |>
      relocate(Year, .before = start_date_cycle) |>
      mutate(
        `Weekly Enrollment` = formatC(round(n), format = "d", big.mark = ","),
        `Cumulative enrollments` = formatC(round(cumulative_n_includingold), format = "d", big.mark = ",")
      ) |>
      rename(Week = week_of_cycle, Date = start_date_cycle) |>
      select(Year, Date, Week, `Weekly Enrollment`, `Cumulative enrollments`)
  })
  
  output$graphtotal <- renderPlot({
    req(graph_total())
    ggplot(data = NULL) +
      geom_rect(aes(xmin = 8,xmax = 14, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.2) +
      annotate("text", x = 11, y = 90000, label = "PM1", size = 9) +
      geom_rect(aes(xmin = 25,xmax = 31, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.2) +
      annotate("text", x = 28, y = 90000, label = "PM2", size = 9) +
      geom_rect(aes(xmin = 44,xmax = 49, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.2) +
      annotate("text", x = 46.5, y = 90000, label = "PM3", size = 9) +
      geom_area(data = graph_total(), aes(x = week_of_cycle, y = cumulative_applicants, fill = Year), position = "identity", alpha = 0.8) +
      annotate("text", x = Inf, y = Inf, label = paste0("New enrolled students: ", format(total_new_enroll(), big.mark = ",")),
               hjust = 1.3, vjust = 14, size = 9, color = "black") +
      labs(title = "Enrollment Over Time", x = "Weeks", y = "Enrollment") +
      theme_minimal() +
      theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))
  })
  
  output$grade <- renderPlot({
    req(gradelevels())
    ggplot(data = gradelevels()) +
      geom_line(aes(x = week_of_cycle, y = cumulative_applicants, color = Grade), size = 1.5) +
      geom_point(aes(x = week_of_cycle, y = cumulative_applicants, fill = Grade), size = 3, pch = 21, color = "white") +
      facet_wrap(~Year, nrow = 2, scales = "free_y") +
      labs(title = "Enrollment Over Time", x = "Weeks", y = "Enrollment") +
      theme_minimal() + 
      theme(axis.title = element_text(size = 16)) +
      scale_color_viridis_d(option = "H") +
      scale_fill_viridis_d(option = "H")
  })
  
  output$district_selector <- renderUI({
    req(district_data())
    selectInput("District", "Select District", choices = unique(district_data()$DistrictName), selected = unique(district_data()$DistrictName)[1])
  })
  
  filtered_data <- reactive({
    req(district_data(), input$District)
    district_data()[district_data()$DistrictName == input$District, ]
  })
  
  output$fc_plot <- renderPlot({
    req(filtered_data())
    ggplot(data = filtered_data()) +
      geom_area(aes(x = week_of_cycle, y = cumulative_applicants, fill = Year)) +
      labs(title = paste("Enrollment Over Time -", input$District), x = "Date", y = "Enrollment") +
      theme_minimal()
  })
}

shinyApp(ui, server)