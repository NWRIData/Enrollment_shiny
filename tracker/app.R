library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)


get_latest_rds <- function(dir_path, verbose = TRUE) {
  rds_files <- list.files(path = dir_path, pattern = "\\.rds$", full.names = TRUE)
  
  if (length(rds_files) == 0) {
    if (verbose) cat("No .rds files found in", dir_path, "\n")
    return(NULL)
  }
  
  file_info <- file.info(rds_files)
  latest_file <- rownames(file_info)[which.max(file_info$mtime)]
  
  if (verbose) cat("Loaded file from:", latest_file, "\n")
  
  readRDS(latest_file)
}
district_data <- get_latest_rds("data/district_df")
totaldata     <- get_latest_rds("data/total_data")
diff_total    <- get_latest_rds("data/diff_total")


diff_week<-diff_total$`n_Current Year`-diff_total$`n_Previous Year`
diff_total<-diff_total$`cumulative_applicants_Current Year`-diff_total$`cumulative_applicants_Previous Year`


district_df<-unique(district_data$DistrictName)

currentdate<-Sys.Date()
updatetime<-paste("Last Updated at", currentdate)


ui <- fluidPage(
  titlePanel(paste("Enrollment Progress NWRI", updatetime)),
  tabsetPanel(
    tabPanel("Total Enrollment",
             fluidRow(
               column(width = 12,
                      uiOutput("enrollment_summary"),
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
  output$enrollment_summary <- renderUI({
    enrollment_cumulative <- diff_total  # Still assuming this is precomputed
    
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
  
  graph_total = reactive({totaldata})
  
  # Render the plot
  output$graphtotal <- renderPlot({
    ggplot(data = NULL) +
      geom_area(data = graph_total(), aes(x = week_of_cycle,
                                            y = cumulative_applicants,
                                            fill = Year)) +
      geom_line(data = graph_total(), aes(x = week_of_cycle,
                                          y = cumulative_applicants,
                                          fill = Year)) +
      labs(title = paste("Enrollment Over Time"),
           x = "Weeks", y = "Enrollment") +
      theme_minimal() +
      theme(axis.title = element_text(size = 16))
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

