library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(shinydashboard)
library(plotly)
library(shinyWidgets)
library(gridExtra)
library(grid)
library(ggplot2)




# Function to load data
load_data <- function() {
  read_csv("2025Season_CSV.csv") %>%
    mutate(
      TaggedPitchType = ifelse(TaggedPitchType %in% c("FourSeamFastBall"), "Fastball", TaggedPitchType),
      Results = case_when(
        PitchCall =="BallCalled" ~ 1,
        ExitSpeed >= 90 ~ 1,
        ExitSpeed < 75 ~ 0, 
        PitchCall %in% c("StrikeCalled", "FoulBall", "StrikeSwinging", "Foul") ~ 0
      ),
      OutcomeCategory = case_when(
          PitchCall == "StrikeCalled" ~ "Called Strike",
          PitchCall %in% c("StrikeSwinging", "Foul") ~ "Good Outcomes",  # Use %in% for multiple values
          ExitSpeed <=  75~ "Weak Contact",
          TRUE ~ NA_character_ 
        ),
      Date = as.Date(Date, format = "%m/%d/%Y"),
      Count = paste(Balls, Strikes, sep = "-"), # Combine Balls and Strikes into a Count column like "0-0"
      # Separate count groupings
      TwoStrikes = case_when(
        Count %in% c("0-2", "1-2", "2-2", "3-2") ~ "Yes",
        TRUE ~ "No"
      ),
      PitchersAhead = case_when(
        Count %in% c("0-1", "0-2", "1-2", "2-2") ~ "Yes",
        TRUE ~ "No"
      ),
      HittersAhead = case_when(
        Count %in% c("2-0", "3-0", "2-1", "3-1", "1-0") ~ "Yes",
        TRUE ~ "No"
      ),
      Even = case_when(
        Count %in% c("0-0", "1-1", "3-2") ~ "Yes",
        TRUE ~ "No"
      )
    ) %>%
    filter(
      TaggedPitchType != "Undefined",
      TaggedPitchType != "Other"
    ) %>%
    select(
      Date, PitchNo,BatterSide,PitcherThrows, Pitcher, Balls, Strikes, Count, TwoStrikes, HittersAhead, PitchersAhead, Even, TaggedPitchType, PlayResult, SpinRate, RelSpeed, HorzBreak, InducedVertBreak,
      RelSide, RelHeight, ExitSpeed, Angle, Distance, Batter, PitchCall, Extension, PlateLocSide, PlateLocHeight, Bearing,Distance,Results, OutcomeCategory
    )
}
load_stuff_data <- function() {
  read_csv("poststuffplus.csv") %>%
    mutate(
      TaggedPitchType = ifelse(TaggedPitchType %in% c("FourSeamFastBall"), "Fastball", TaggedPitchType),
      Date = as.Date(Date, format = "%m/%d/%Y"),
      Count = paste(Balls, Strikes, sep = "-"), # Combine Balls and Strikes into a Count column like "0-0"
      # Separate count groupings
      TwoStrikes = case_when(
        Count %in% c("0-2", "1-2", "2-2", "3-2") ~ "Yes",
        TRUE ~ "No"
      ),
      PitchersAhead = case_when(
        Count %in% c("0-1", "0-2", "1-2", "2-2") ~ "Yes",
        TRUE ~ "No"
      ),
      HittersAhead = case_when(
        Count %in% c("2-0", "3-0", "2-1", "3-1", "1-0") ~ "Yes",
        TRUE ~ "No"
      ),
      Even = case_when(
        Count %in% c("0-0", "1-1", "3-2") ~ "Yes",
        TRUE ~ "No"
      )
    ) %>%
    filter(
      TaggedPitchType != "Undefined",
      TaggedPitchType != "Other"
    ) %>%
    select(
      Date,PitchNo,Balls,Strikes, Pitcher, PitcherTeam, Batter,TaggedPitchType, PitchCall, Stuff,RelSpeed, SpinRate, RelHeight, RelSide, Extension, InducedVertBreak, HorzBreak
    )
}

# Define GCU Hitters and Pitchers
GCU_Hitters <-c("Josh Wakefield", "Cooper Neville", "Zach Yorke", "Eli Paton", "Eddy Pelc", "Cannon Peery", "Emilio Barreras", "Michael Diaz", "Troy Sanders", "Carson Ohland", "Christian Gross", "Scuba Smolinski", "Kade Huff", "Gunnar Penzkover", "Marcus Galvan", "Billy Scaldeferri", "Cael Boever", "Luke Moeller", "Brody Sexton", "Austin Matranga","Jake Sanko","Marcus Galvan")
GCU_Pitchers <-c("Isaac Lyon","Garrett Ahern","Chance Key","Walter Quinn","Connor Mattison","Billy Gregory", "Gunnar Penzkover", "Barrett Skaugrud", "Ross Clark", "Elijah Higginbottom", "Jace Smith", "Cam Cunnings", "Devon Laguinto","Gray Bailey","Grant Richardson","Ben Smith", "Josh Wakefield", "Justin Hanson", "Zach Hauser", "Jace Behnke", "Cayden Collins","Dillon Orr", "Alec Ammerman")

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  fluidRow(
    column(2,
           tags$img(src = "gcubaseball.jpg", height = "75px", width = "200px")
    ),
    column(10, 
           h2("GCU Baseball Analytics Dashboard", style = "color: #532F9B; font-weight: bold; text-align: center;")
    )
    
  ),
  
  # Custom CSS for the nav bar, tab panel, and logo
  tags$head(
    tags$style(HTML("
      .navbar-default {
        background-color: #532F9B; /* Custom purple color for navbar */
        border-color: #532F9B;
      }
      .navbar-default .navbar-brand, 
      .navbar-default .navbar-nav > li > a {
        color: white; /* White text on navbar */
      }
      .nav-tabs > li > a {
        color: white; /* White text for tabs */
        background-color: #6A1B9A; /* Purple background for tabs */
      }
      .nav-tabs > li.active > a, 
      .nav-tabs > li.active > a:hover, 
      .nav-tabs > li.active > a:focus {
        background-color: white; /* White background for active tab */
        color: #4A148C; /* Purple text for active tab */
        border-color: #4A148C #4A148C white; /* Borders for the active tab */
      }
      .tab-content {
        padding: 10px;
      }
      .navbar-right {
        position: absolute;
        right: 10px;
        top: 10px;
      }
    "))
  ),
  
  # Load necessary libraries
  tags$head(
    tags$style(HTML("
    body, .container-fluid, .container {
      margin: 0;
      padding: 0;
      width: 100%;
      height: 100%;
    }
    .navbar {
      margin-bottom: 0;
    }
    .row {
      margin: 0;
      padding: 0;
    }
    .main-panel {
      margin-top: 0 !important;
      padding-top: 0 !important;
    }
    #zonePlot {
      margin-top: 0 !important;
      padding-top: 0 !important;
    }
    .grob {
      margin-top: 0;
      padding-top: 0;
    }
    #grid-container {
      padding: 0 !important;
      margin: 0 !important;
      display: flex;
      align-items: flex-start;
    }
  "))
  ),
  
  navbarPage(
    theme = "cerulean",
    title = "Options",
    tabPanel("Pitcher",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("pitcherDropdown", "Select Pitcher", choices = GCU_Pitchers, multiple = FALSE),
                 selectInput("graphType", "Select Graph", choices = c("Data Table", "Stuff Plus","Zone LHH vs RHH", "Heat Map","Velocity", "Pitch Movement", "Spin Rate","Pitch Release" ), selected = "Data Table"),
                 selectizeInput("pitchTypeDropdown", 
                                "Select Pitch Type", 
                                choices = c("All", "Fastball", "Changeup", "Cutter", "Slider", "Curveball"),  # Specific pitch types
                                multiple = TRUE, 
                                selected = "All"),  # Default to "All"
                 selectizeInput("countDropdown", 
                                "Select Count", 
                                choices = c("All", "Pitchers Ahead", "Hitters Ahead","2 Strikes","Even", "0-0", "1-0", "2-0", "3-0", "0-1", "1-1", "2-1", "3-1", "0-2", "1-2", "2-2", "3-2"),  
                                multiple = TRUE, 
                                selected = "All"),
                 selectizeInput("pitchCallDropdown", 
                                "Select Result", 
                                choices = c("All", "BallCalled","InPlay","FoulBall","StrikeCalled","StrikeSwinging","Foul" ),  # Exclude NA values
                                multiple = TRUE, 
                                selected = "All"), # Default to
                 dateRangeInput("dateRange", "Select Date Range", 
                                start = Sys.Date() - 200,  # Default start date is 30 days ago
                                end = Sys.Date()),         # Default end date is today
                 actionButton("reload_data", "Reload Data"),
                 width = 3  # Adjust sidebar width to 3 columns
               ),
               mainPanel(
                 width = 9,  # Adjust the main panel width to 9 columns
                 # Existing conditional panels for other plots
                 conditionalPanel(
                   condition = "input.graphType == 'Data Table'",
                   DTOutput("pitcher_data_table")
                 ),
                 conditionalPanel(
                   condition = "input.graphType == 'Stuff Plus'",
                   plotOutput("stuffPlot", height = "700px", width ="1100px"),
                   DTOutput("stuff_data_table") 
                 ),
                 conditionalPanel(
                   condition = "input.graphType == 'Velocity'",
                   plotOutput("relSpeedPlot", height = "600px"),
                   DTOutput("max_velocity_table")
                 ),
                 conditionalPanel(
                   condition = "input.graphType == 'Zone LHH vs RHH'",
                   plotOutput("zonePlot", height = "600px", width = "1100px")
                 ),
                 conditionalPanel(
                   condition = "input.graphType == 'Heat Map'",
                   fluidRow(
                     column(6, plotOutput("pitcherHeatmap", height = "600px")),
                     column(6, plotOutput("pitcherHeatmap2", height = "600px"))
                            )
                   ),
                 conditionalPanel(
                   condition = "input.graphType == 'Pitch Movement'",
                   plotOutput("pitch_movement_plot", height = "700px", width ="1100px"),
                   DTOutput("pitch_movement_table") 
                 ),
                 conditionalPanel(
                   condition = "input.graphType == 'Spin Rate'",
                   plotOutput("spinRatePlot", height = "600px")
                 ),
                 conditionalPanel(
                   condition = "input.graphType == 'Pitch Release'",
                   plotOutput("pitch_release_plot", height = "600px", width = "1100px")
                 )
               )
             )
    ),
    #@HF
    tabPanel("Hitters",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("hitterDropdown", "Select Hitter", choices = GCU_Hitters, multiple = FALSE),
                 selectInput("graphType", "Select Graph", choices = c("Spray Charts", "Heat Map", "Zone LHH vs RHH", "Data Table"), selected = "Spray Plot"),
                 selectizeInput("pitchTypeDropdown", 
                                "Select Pitch Type", 
                                choices = c("All", "Fastball", "Changeup", "Splitter", "Slider", "Curveball"),  # Specific pitch types
                                multiple = TRUE, 
                                selected = "All"),  # Default to "All"c
                 selectizeInput("countDropdown", 
                                "Select Count", 
                                choices = c("All", "Pitchers Ahead", "Hitters Ahead","2 Strikes","Even", "0-0", "1-0", "2-0", "3-0", "0-1", "1-1", "2-1", "3-1", "0-2", "1-2", "2-2", "3-2"),  
                                selected = "All"),
                 selectizeInput("pitchCallDropdown", 
                                "Select Result", 
                                choices = c("All", "BallCalled","InPlay","FoulBall","StrikeCalled","StrikeSwinging","Foul" ),  # Exclude NA values
                                multiple = TRUE, 
                                selected = "All"), # Default to "
                 dateRangeInput("dateRange", "Select Date Range", 
                                start = Sys.Date() - 200,  # Default start date is 30 days ago
                                end = Sys.Date()),         # Default end date is today
                 actionButton("reload_data", "Reload Data"),
                 width = 3  # Adjust sidebar width to 3 columns
               ),
               mainPanel(
                 width = 9,  # Adjust the main panel width to 9 columns
                 # Existing conditional panels for other plots
                 conditionalPanel(
                   condition = "input.graphType == 'Spray Charts'",
                   plotOutput("sprayPlot",  height = "600px", width ="1100px"),
                   DTOutput("maxEVtable")),
  
                 conditionalPanel(
                   condition = "input.graphType == 'Heat Map'",
                   fluidRow(
                     column(6, plotOutput("HitHeatmap", height = "600px"))
                   )
                 ),
                 conditionalPanel(
                   condition = "input.graphType == 'Zone LHH vs RHH'",
                   plotOutput("zonePlotH", height = "600px", width = "1100px")
                 ),
                 conditionalPanel(
                   condition = "input.graphType == 'Data Table'",
                   DTOutput("hitter_data_table")
                 )
               )
             )
    ),
    tabPanel("PDF Viewer",
             sidebarLayout(
               sidebarPanel(
                 selectInput("folder", "Choose a Subfolder:", choices = NULL, selected = NULL),
                 # Display list of PDF files in the selected folder
                 selectInput("pdf_file", "Choose a PDF file:", choices = NULL, selected = NULL),
                 actionButton("open_pdf", "Open PDF")
               ),
               mainPanel(
                 # Create a UI output to display the selected PDF
                 uiOutput("pdf_viewer"),
                 # Optional: Display the list of PDFs in a data table
                 DTOutput("pdf_table")
               )
             )
    ),   tabPanel("About",
                  h2("Purpose of the App:"),
                  
                  h2("Trackman Resources:")
    )
  )
)



# @S
server <- function(input, output, session) {
  
  # Reactive value to store the data
  rv <- reactiveValues(meta_data = load_data())
  
  # Reload the data when the button is clicked
  observeEvent(input$reload_data, {
    rv$meta_data <- load_data()
  })
  #@PR
  # Reactive expression to filter data based on selected filters
  selected_pitcher_data <- reactive({
    # Start with the full dataset
    data <- rv$meta_data
    
    # Filter by selected pitcher
    # Filter by selected pitcher
    if (!is.null(input$pitcherDropdown) && input$pitcherDropdown != "") {
      data <- data %>% filter(Pitcher == input$pitcherDropdown)
    }
    
    # Filter by selected date range
    if (!is.null(input$dateRange)) {
      data <- data %>%
        filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
    }
    
    # Filter by selected pitch types
    if (!is.null(input$pitchTypeDropdown) && !"All" %in% input$pitchTypeDropdown) {
      data <- data %>%
        filter(TaggedPitchType %in% input$pitchTypeDropdown)
    }
    
    # Filter by selected pitch calls
    if (!is.null(input$pitchCallDropdown) && !"All" %in% input$pitchCallDropdown) {
      data <- data %>%
        filter(PitchCall %in% input$pitchCallDropdown)
    }
    
    # Filter by selected count group or specific count
    if (!is.null(input$countDropdown) && input$countDropdown != "All") {
      if (input$countDropdown == "2 Strikes") {
        data <- data %>% filter(TwoStrikes == "Yes")
      } else if (input$countDropdown == "Hitters Ahead") {
        data <- data %>% filter(HittersAhead == "Yes")
      } else if (input$countDropdown == "Pitchers Ahead") {
        data <- data %>% filter(PitchersAhead == "Yes")
      } else if (input$countDropdown == "Even") {
        data <- data %>% filter(Even == "Yes")
      } else {
        data <- data %>% filter(Count == input$countDropdown)
      }
    }
    
    # Return the filtered data
    return(data)
  })
  rvs <- reactiveValues(meta_data = load_stuff_data())
  
  # Reload the data when the button is clicked
  observeEvent(input$reload_data, {
    rvs$meta_data <- load_stuff_data()
  })
  #@PR
  # Reactive expression to filter data based on selected filters
  selected_stuff_data <- reactive({
    # Start with the full dataset
    data <- rvs$meta_data
    
    # Filter by selected pitcher
    # Filter by selected pitcher
    if (!is.null(input$pitcherDropdown) && input$pitcherDropdown != "") {
      data <- data %>% filter(Pitcher == input$pitcherDropdown)
    }
    
    # Filter by selected date range
    if (!is.null(input$dateRange)) {
      data <- data %>%
        filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
    }
    
    # Filter by selected pitch types
    if (!is.null(input$pitchTypeDropdown) && !"All" %in% input$pitchTypeDropdown) {
      data <- data %>%
        filter(TaggedPitchType %in% input$pitchTypeDropdown)
    }
    
    # Filter by selected pitch calls
    if (!is.null(input$pitchCallDropdown) && !"All" %in% input$pitchCallDropdown) {
      data <- data %>%
        filter(PitchCall %in% input$pitchCallDropdown)
    }
    
    # Filter by selected count group or specific count
    if (!is.null(input$countDropdown) && input$countDropdown != "All") {
      if (input$countDropdown == "2 Strikes") {
        data <- data %>% filter(TwoStrikes == "Yes")
      } else if (input$countDropdown == "Hitters Ahead") {
        data <- data %>% filter(HittersAhead == "Yes")
      } else if (input$countDropdown == "Pitchers Ahead") {
        data <- data %>% filter(PitchersAhead == "Yes")
      } else if (input$countDropdown == "Even") {
        data <- data %>% filter(Even == "Yes")
      } else {
        data <- data %>% filter(Count == input$countDropdown)
      }
    }
    
    # Return the filtered data
    return(data)
  })
  
  
  #@HR
  
  
  # Reactive expression to filter data based on selected hitter and date range
  selected_hitter_data <- reactive({
    data <- rv$meta_data
    
    # Filter by selected hitter
    if (!is.null(input$hitterDropdown) && input$hitterDropdown != "") {
      data <- data %>%
        filter(Batter == input$hitterDropdown)
    }
    
    # Filter by selected date range
    if (!is.null(input$dateRange)) {
      data <- data %>%
        filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
    }
    
    # Filter by selected pitch types
    if (!is.null(input$pitchTypeDropdown) && !"All" %in% input$pitchTypeDropdown) {
      data <- data %>%
        filter(TaggedPitchType %in% input$pitchTypeDropdown)
    }
    if (!is.null(input$pitchCallDropdown) && !"All" %in% input$pitchCallDropdown) {
      data <- data %>%
        filter(PitchCall %in% input$pitchCallDropdown)
    }
    
    # Filter by selected count group or specific count
    if (!is.null(input$countDropdown) && input$countDropdown != "All") {
      if (input$countDropdown == "2 Strikes") {
        data <- data %>% filter(TwoStrikes == "Yes")
      } else if (input$countDropdown == "Hitters Ahead") {
        data <- data %>% filter(HittersAhead == "Yes")
      } else if (input$countDropdown == "Pitchers Ahead") {
        data <- data %>% filter(PitchersAhead == "Yes")
      } else if (input$countDropdown == "Even") {
        data <- data %>% filter(Even == "Yes")
      } else {
        data <- data %>% filter(Count == input$countDropdown)
      }
    }
    
    # Return the filtered data
    return(data)
  })  
  #@PP 
  output$spinRatePlot <- renderPlot({
    selected_pitcher <- selected_pitcher_data()  # Now includes date range filtering
    pitch_colors <- c("Fastball" = "#F21500", 
                      "Slider" = "#EDC10C", 
                      "Changeup" = "#49CB21", 
                      "Splitter" = "#49CB21", 
                      "Curveball" = "#4BD2FF", 
                      "Cutter" = "#743F00")    
    pitch_type_colors <- scale_color_manual(values = pitch_colors)
    
    # Calculate average spin rates by pitch type
    avg_spin_rates <- selected_pitcher %>%
      group_by(TaggedPitchType) %>%
      summarize(AvgSpinRate = mean(SpinRate, na.rm = TRUE))
    
    # Plot the average spin rates
    ggplot(avg_spin_rates, aes(x = TaggedPitchType, y = AvgSpinRate)) +
      geom_bar(stat = "identity", position = "dodge", fill = "darkblue", width = 0.5) +
      geom_text(aes(label = round(AvgSpinRate)),
                position = position_dodge(width = 0.5),
                vjust = -0.5, hjust = 0.5,
                size = 8, color = "black", fontface = "bold") +
      labs(
        title = paste("Average Spin Rate by Pitch Type for", input$pitcherDropdown),
        x = "Pitch Type",
        y = "Spin Rate (RPM)"
      ) +
      pitch_type_colors+
      theme_minimal() +
      theme(
        plot.title = element_text(size = 24, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black")
      ) +
      coord_cartesian(ylim = c(0, max(avg_spin_rates$AvgSpinRate) * 1.1))
  }, width = 1100, height = 700)
  # Velocity Plot
  # Velocity Plot
  output$relSpeedPlot <- renderPlot({
    selected_pitcher <- selected_pitcher_data()
    
    # Define pitch colors and ensure they match the TaggedPitchType levels in the data
    pitch_colors <- c("Fastball" = "#F21500", 
                      "Slider" = "#EDC10C", 
                      "Changeup" = "#49CB21", 
                      "Splitter" = "#49CB21", 
                      "Curveball" = "#4BD2FF", 
                      "Cutter" = "#743F00")    
    # Only retain colors for existing pitch types in the data
    valid_pitch_types <- intersect(names(pitch_colors), unique(selected_pitcher$TaggedPitchType))
    pitch_colors <- pitch_colors[valid_pitch_types]
    
    # Scale colors with valid levels
    pitch_type_colors <- scale_color_manual(values = pitch_colors)
    
    # Ensure data is sorted by Date and PitchNo, then calculate running velocity for each pitch type
    selected_pitcher <- selected_pitcher %>%
      arrange(TaggedPitchType, Date, PitchNo) %>%
      group_by(TaggedPitchType) %>%
      mutate(RunningVelocity = cumsum(RelSpeed) / row_number(),
             PitchSeq = row_number()) %>%

    # Plot with a line for each pitch type
    ggplot(selected_pitcher, aes(x = PitchSeq, y = RunningVelocity, color = TaggedPitchType)) +
      geom_line(size = 1.2) +
      geom_point() +
      ylim(60, 100) +
      scale_x_continuous(breaks = seq(min(selected_pitcher$PitchSeq), max(selected_pitcher$PitchSeq), by = 1))+
      labs(
        title = paste("Running Velocity by Pitch Type for", input$pitcherDropdown),
        x = "Pitch Number",
        y = "Running Velocity"
      ) +
      pitch_type_colors +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 24, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # Max Velocity Table
  output$max_velocity_table <- renderDT({
    selected_pitcher <- selected_pitcher_data()
    
    max_velocity <- selected_pitcher %>%
      group_by(TaggedPitchType) %>%
      summarize(MaxVelocity = max(RelSpeed, na.rm = TRUE))
    
    datatable(max_velocity)
  })
  output$pitch_movement_plot <- renderPlot({
    selected_pitcher <- selected_pitcher_data()
    pitch_colors <- c("Fastball" = "#F21500", 
                      "Slider" = "#EDC10C", 
                      "Changeup" = "#49CB21", 
                      "Splitter" = "#49CB21", 
                      "Curveball" = "#4BD2FF", 
                      "Cutter" = "#743F00")    
    pitch_type_colors <- scale_color_manual(values = pitch_colors)
    
    ggplot(selected_pitcher, aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
      geom_point(size = 4, alpha = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at x = 0
      labs(
        title = paste("Pitch Movement for", input$pitcherDropdown),
        x = "Horizontal Break",
        y = "Vertical Break"
      ) + pitch_type_colors+
      theme_minimal() +
      theme(
        plot.title = element_text(size = 24, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16, face = "bold")
      ) +
      scale_x_continuous(breaks = seq(-30, 30, by = 10)) +  # Add x-axis ticks every 10 units
      scale_y_continuous(breaks = seq(-30, 30, by = 10)) +  # Add y-axis ticks every 10 units
      coord_cartesian(xlim = c(-30, 30), ylim = c(-30, 30))  # Force axis limits to stay fixed
  }, width = 700, height = 700)
  output$pitch_movement_table <- renderDT({
    selected_pitcher <- selected_pitcher_data()
    
    # Calculate averages for each pitch type and round to 2 decimal places
    movement_data <- selected_pitcher %>%
      group_by(TaggedPitchType) %>%
      summarize(
        `Avg Velo` = round(mean(RelSpeed, na.rm = TRUE), 2),
        `Avg Spin Rate` = round(mean(SpinRate, na.rm = TRUE), 2),
        `Avg IVB` = round(mean(InducedVertBreak, na.rm = TRUE), 2),  # Induced Vertical Break
        `Avg HB` = round(mean(HorzBreak, na.rm = TRUE), 2),          # Horizontal Break
        `Avg RelHeight` = round(mean(RelHeight, na.rm = TRUE), 2),
        `Avg RelSide` = round(mean(RelSide, na.rm = TRUE), 2),
        `Avg Extension` = round(mean(Extension, na.rm = TRUE), 2)
      )
    
    # Render the table using datatable from DT package
    datatable(
      movement_data,
      options = list(
        pageLength = 10,  # Show 10 rows per page
        autoWidth = TRUE,  # Adjust column widths automatically
        scrollX = TRUE     # Enable horizontal scrolling if needed
      ),
      rownames = FALSE  # Remove row names
    )
  })
  output$pitch_release_plot <- renderPlot({
    selected_pitcher <- selected_pitcher_data()
    pitch_colors <- c("Fastball" = "#F21500", 
                      "Slider" = "#EDC10C", 
                      "Changeup" = "#49CB21", 
                      "Splitter" = "#49CB21", 
                      "Curveball" = "#4BD2FF", 
                      "Cutter" = "#743F00")
  pitch_type_colors <- scale_color_manual(values = pitch_colors)
    
    ggplot(selected_pitcher, aes(x = RelSide, y = RelHeight, color = TaggedPitchType)) +
      geom_point(size = 4, alpha = 0.7) +
      labs(
        title = paste("Pitch Release for", input$pitcherDropdown),
        x = "Release Side (ft)",
        y = "Release Height (ft)"
      ) + pitch_type_colors+ xlim(-4, 4) + ylim(0, 7) +
      
      theme_minimal() +
      theme(
        plot.title = element_text(size = 24, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16, face = "bold")
      )
  }, width = 1100, height = 600)
  # Data Table for the selected pitcher
  output$pitcher_data_table <- renderDT({
    selected_pitcher <- selected_pitcher_data() 
    TableData <- selected_pitcher %>% select(Date, PitchNo, Batter, Pitcher, Count, TaggedPitchType, PitchCall, PlayResult,  RelSpeed,SpinRate, InducedVertBreak,  HorzBreak,
                                             RelSide, RelHeight, Extension, ExitSpeed, Angle, Distance) %>% mutate_if(is.numeric, ~ round(., 2)) 
    
    # Render the table using datatable from DT package
    datatable(
      TableData,
      options = list(
        pageLength = 20,  # Show 10 rows per page
        autoWidth = TRUE,  # Adjust column widths automatically
        scrollX = TRUE     # Enable horizontal scrolling if needed
      ),
      rownames = FALSE  # Remove row names
    )
  })
  
  lopes_folder <- "www/PostGame Reports/Lopes"
  
  # Get list of subfolders inside the Lopes folder
  if (dir.exists(lopes_folder)) {
    subfolders <- list.dirs(lopes_folder, recursive = FALSE, full.names = FALSE)
    # Update the select input choices with the list of subfolders
    updateSelectInput(session, "folder", choices = subfolders)
  } else {
    print("Lopes folder does not exist. Check folder path.")
  }
  
  # Update PDF file list based on the selected folder
  observeEvent(input$folder, {
    req(input$folder)  # Check if a folder is selected
    
    # Define the selected subfolder path
    selected_folder_path <- file.path(lopes_folder, input$folder)
    
    # List PDF files in the selected folder
    pdf_files <- list.files(selected_folder_path, pattern = "\\.pdf$", full.names = FALSE)
    pdf_files <- rev(pdf_files)
    # Update the PDF file dropdown with the list of PDFs in the selected folder
    updateSelectInput(session, "pdf_file", choices = pdf_files)
  })
  
  # Display the selected PDF using an iframe
  output$pdf_viewer <- renderUI({
    req(input$pdf_file)  # Check if a file is selected
    
    # Construct relative path for iframe src
    file_path <- file.path("www", "PostGame Reports", "Lopes", input$folder, input$pdf_file)
    
    # Debug: Print file path to verify correct reference
    print(paste("Attempting to display PDF from:", file_path))
    
    # Use forward slashes for iframe source path
    tags$iframe(style = "height:600px; width:100%", src = file_path)
  })
  output$zonePlot <- renderPlot({
    selected_pitcher <- selected_pitcher_data()  # Use filtered data based on user input
    pitch_colors <- c("Fastball" = "#F21500", 
                      "Slider" = "#EDC10C", 
                      "Changeup" = "#49CB21", 
                      "Splitter" = "#49CB21", 
                      "Curveball" = "#4BD2FF", 
                      "Cutter" = "#743F00")    
    pitch_type_colors <- scale_color_manual(values = pitch_colors)
    
    # Filter for Left-Handed Hitters
    FilterzoneL <- selected_pitcher %>% filter(BatterSide %in% c("Left"))
    
    # Plot for Left-Handed Hitters (LHH)
    ZoneL <- ggplot(FilterzoneL, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
      geom_point(size = 3.5, alpha = 0.7) +
      annotate("rect", xmin = -0.8, xmax = 0.8, ymin = 1.4, ymax = 3.5, fill = NA, color = "black") +
      xlim(-1.4, 1.4) + ylim(1, 4) +
      labs(
        title = "VS LHH",
        subtitle = "Pitcher's Perspective",
        x = "Horizontal Pitch Location",
        y = "Vertical Pitch Location"
      ) +
      pitch_type_colors +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14),
        plot.subtitle = element_text(size = 16),
        legend.position = "top",
        plot.margin = unit(c(1, 0, 0, 0), "cm"),  # Add a small top margin
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)
      ) +
      coord_fixed(ratio = 1.5)
    
    # Filter for Right-Handed Hitters
    FilterzoneR <- selected_pitcher %>% filter(BatterSide %in% c("Right"))
    
    # Plot for Right-Handed Hitters (RHH)
    ZoneR <- ggplot(FilterzoneR, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
      geom_point(size = 3.5, alpha = 0.7) +
      annotate("rect", xmin = -0.8, xmax = 0.8, ymin = 1.4, ymax = 3.5, fill = NA, color = "black") +
      xlim(-1.4, 1.4) + ylim(1, 4) +
      labs(
        title = "VS RHH",
        subtitle = "Pitcher's Perspective",
        x = "Horizontal Pitch Location",
        y = "Vertical Pitch Location"
      ) +
      pitch_type_colors +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14),
        plot.subtitle = element_text(size = 16),
        legend.position = "top",
        plot.margin = unit(c(1, 0, 0, 0), "cm"),  # Add a small top margin
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)
      ) +
      coord_fixed(ratio = 1.5)
    
    # Convert ggplot objects to grobs
    ZoneL_grob <- ggplotGrob(ZoneL)
    ZoneR_grob <- ggplotGrob(ZoneR)
    
    # Arrange the grobs side by side
    grid.arrange(
      ZoneL_grob, ZoneR_grob,
      ncol = 2
    )
  }, width = 1100, height = 600)  # Adjust the width and height of the output
  output$pdf_viewer <- renderUI({
    req(input$pdf_file)  # Check if a file is selected
    
    # Construct relative path for iframe src
    file_path <- file.path("PostGame Reports/Lopes", input$folder, input$pdf_file)
    # Debug: Print file path to verify correct reference
    print(paste("Attempting to display PDF from:", file_path))
    tags$iframe(style = "height:600px; width:100%", src = file_path)
  })  # Load necessary libraries
  #@HP
  library(gridExtra)
  
  output$sprayPlot <- renderPlot({
    selected_hitter <- selected_hitter_data()  # Use filtered data based on user input
    pitch_colors <- c("Fastball" = "#F21500", 
                      "Slider" = "#EDC10C", 
                      "Changeup" = "#49CB21", 
                      "Splitter" = "#49CB21", 
                      "Curveball" = "#4BD2FF", 
                      "Cutter" = "#743F00")
    pitch_type_colors <- scale_color_manual(values = pitch_colors)
    output$maxEVtable <- renderDT({
      selected_pitcher <- selected_pitcher_data()
      
      max_EV <- selected_hitter %>%
        summarize(MaxEV= max(ExitSpeed, na.rm = TRUE),
                  AVGEV= mean(ExitSpeed, na.rm = TRUE))
      
      datatable(max_EV)
    })


    # Create the plot
    SprayEV <- ggplot(selected_hitter, aes(x = Bearing, y = Distance, color = RelSpeed)) + 
      # drawing an area for the playground
      annotate(geom = "rect", xmin = 45, xmax = -45, 
               ymin = 0, ymax = Inf,
               fill = "grey", alpha = 0.2) + 
      # add marks for the distances
      annotate(geom = "text", 
               x = rep(50, 4), y = c(100, 200, 300, 400), 
               label = c("100", "200", "300", "400")) +
      # add lines for the distance 
      annotate(geom = "segment",
               x = rep(-45,4), xend = rep(45, 4), 
               y = c(100, 200, 300, 400), yend = c(100, 200, 300, 400),
               linetype = "dotted") + 
      # show the results as points
      geom_point(size = 3, alpha = 0.7) + 
      # scale the color from red to blue based on RelSpeed
      scale_color_gradient(low = "blue", high = "red", name = "RelSpeed (MPH)") + 
      # convert to polar
      # convert to polar
      coord_polar(theta = "x", start = pi, clip = "on") + 
      theme_void() +  
      # adjust the axis 
      scale_x_continuous(limits = c(-180, 180),
                         breaks = c(-45, -22.5, 0, 22.5, 45 )) + 
      # show the breaks on the x-axis
      theme(panel.grid.major.x = element_line())
    SprayPitch <- ggplot(selected_hitter, aes(x = Bearing, y = Distance, color = TaggedPitchType)) + 
      # drawing an area for the playground
      annotate(geom = "rect", xmin = 45, xmax = -45, 
               ymin = 0, ymax = Inf,
               fill = "grey", alpha = 0.2) + 
      # add marks for the distances
      annotate(geom = "text", 
               x = rep(50, 4), y = c(100, 200, 300, 400), 
               label = c("100", "200", "300", "400")) +
      # add lines for the distance 
      annotate(geom = "segment",
               x = rep(-45,4), xend = rep(45, 4), 
               y = c(100, 200, 300, 400), yend = c(100, 200, 300, 400),
               linetype = "dotted") + 
      # show the results as points
      geom_point( size = 3) + 
      pitch_type_colors +
      # convert to polar
      coord_polar(theta = "x", start = pi, clip = "on") + 
      theme_void() +  
      # adjust the axis 
      scale_x_continuous(limits = c(-180, 180),
                         breaks = c(-45, -22.5, 0, 22.5, 45 )) + 
      # show the breaks on the x-axis
      theme(panel.grid.major.x = element_line())
    # Duplicate the plot
    
    # Arrange both plots in a grid layout
    grid.arrange(SprayEV, SprayPitch, nrow = 1) # Adjust ncol or nrow for different layouts
    
  }, width = 1100, height = 600)
  
  
  output$zonePlotH <- renderPlot({
    selected_hitter <- selected_hitter_data()  # Use filtered data based on user input
    pitch_colors <- c("Fastball" = "#F21500", 
                      "Slider" = "#EDC10C", 
                      "Changeup" = "#49CB21", 
                      "Splitter" = "#49CB21", 
                      "Curveball" = "#4BD2FF", 
                      "Cutter" = "#743F00")    
    pitch_type_colors <- scale_color_manual(values = pitch_colors)
    
    # Filter for Left-Handed Hitters
    FilterzoneL <- selected_hitter %>% filter(PitcherThrows %in% c("Left"))
    
    # Plot for Left-Handed Hitters (LHH)
    ZoneL <- ggplot(FilterzoneL, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
      geom_point(size = 3.5, alpha = 0.7) +
      annotate("rect", xmin = -0.8, xmax = 0.8, ymin = 1.4, ymax = 3.5, fill = NA, color = "black") +
      xlim(-1.4, 1.4) + ylim(1, 4) +
      labs(
        title = "VS LHH",
        subtitle = "Pitcher's Perspective",
        x = "Horizontal Pitch Location",
        y = "Vertical Pitch Location"
      ) +
      pitch_type_colors +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14),
        plot.subtitle = element_text(size = 16),
        legend.position = "top",
        plot.margin = unit(c(1, 0, 0, 0), "cm"),  # Add a small top margin
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)
      ) +
      coord_fixed(ratio = 1.5)
    
    # Filter for Right-Handed Hitters
    FilterzoneR <- selected_hitter %>% filter(PitcherThrows %in% c("Right"))
    
    # Plot for Right-Handed Hitters (RHH)
    ZoneR <- ggplot(FilterzoneR, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
      geom_point(size = 3.5, alpha = 0.7) +
      annotate("rect", xmin = -0.8, xmax = 0.8, ymin = 1.4, ymax = 3.5, fill = NA, color = "black") +
      xlim(-1.4, 1.4) + ylim(1, 4) +
      labs(
        title = "VS RHH",
        subtitle = "Pitcher's Perspective",
        x = "Horizontal Pitch Location",
        y = "Vertical Pitch Location"
      ) +
      pitch_type_colors +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14),
        plot.subtitle = element_text(size = 16),
        legend.position = "top",
        plot.margin = unit(c(1, 0, 0, 0), "cm"),  # Add a small top margin
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)
      ) +
      coord_fixed(ratio = 1.5)
    
    # Convert ggplot objects to grobs
    ZoneL_grob <- ggplotGrob(ZoneL)
    ZoneR_grob <- ggplotGrob(ZoneR)
    
    # Arrange the grobs side by side
    grid.arrange(
      ZoneL_grob, ZoneR_grob,
      ncol = 2
    )
  }, width = 1100, height = 600)  # Adjust the width and height of the output
  
  output$relSpeedPlot <- renderPlot({
    selected_pitcher <- selected_pitcher_data()
    
    # Define pitch colors and ensure they match the TaggedPitchType levels in the data
    pitch_colors <- c("Fastball" = "#F21500", 
                      "Slider" = "#EDC10C", 
                      "Changeup" = "#49CB21", 
                      "Splitter" = "#49CB21", 
                      "Curveball" = "#4BD2FF", 
                      "Cutter" = "#743F00")    
    # Only retain colors for existing pitch types in the data
    valid_pitch_types <- intersect(names(pitch_colors), unique(selected_pitcher$TaggedPitchType))
    pitch_colors <- pitch_colors[valid_pitch_types]
    
    # Scale colors with valid levels
    pitch_type_colors <- scale_color_manual(values = pitch_colors)
    
    # Ensure data is sorted by Date and PitchNo, then calculate running velocity for each pitch type
    selected_pitcher <- selected_pitcher %>%
      arrange(TaggedPitchType, Date, PitchNo) %>%
      group_by(TaggedPitchType) %>%
      mutate(RunningRelSpeed = cumsum(RelSpeed) / row_number(),
             PitchSeq = row_number()) %>%
      ungroup()
    
    # Plot with a line for each pitch type
    ggplot(selected_pitcher, aes(x = PitchSeq, y = RunningRelSpeed, color = TaggedPitchType, group = TaggedPitchType)) +
      geom_line(size = 1.2) +
      geom_point() +
      ylim(60, 100)+
      scale_x_continuous(breaks = seq(min(selected_pitcher$PitchSeq), max(selected_pitcher$PitchSeq), by = 1))+
      labs(
        title = paste("Velo by Pitch Type for", input$pitcherDropdown),
        x = "Pitch Number",
        y = "Velo"
      ) +
      pitch_type_colors +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 24, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  output$hitter_data_table <- renderDT({
    selected_hitter <- selected_hitter_data() 
    TableData <- selected_hitter %>% select(Date, PitchNo, Batter, Pitcher, Count, TaggedPitchType, PitchCall, PlayResult,  RelSpeed, ExitSpeed, Angle, Distance) %>% mutate_if(is.numeric, ~ round(., 2)) 
    datatable(
      TableData,
      options = list(
        pageLength = 20,  # Show 10 rows per page
        autoWidth = TRUE,  # Adjust column widths automatically
        scrollX = TRUE     # Enable horizontal scrolling if needed
      ),
      rownames = FALSE  # Remove row names
    )
  })
  output$HitHeatmap <- renderPlot({
    selected_hitter <- selected_hitter_data()
    
    # Regular HitHeatmap
    HitHeatmap <- ggplot(selected_hitter, aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = FALSE) +
      scale_fill_gradientn(colours = c("blue", "white", "red")) +
      annotate("rect", xmin = -1, xmax = 1,
               ymin = 1.6, ymax = 3.4,
               fill = NA, color = "black", alpha = 0.1) +
      ylim(1, 4) + xlim(-1.8, 1.8) +
      theme_bw() + xlab("Horizontal Pitch Location") + ylab("Vertical Pitch Location") +
      ggtitle("Pitch Location Heat Map", subtitle = "Pitcher's Perspective") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5))
    print(HitHeatmap)
  })

  output$pitcherHeatmap <- renderPlot({
    selected_pitcher <- selected_pitcher_data()
    
    # Regular pitcherHeatmap
    pitcherHeatmap <- ggplot(selected_pitcher, aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = FALSE) +
      scale_fill_gradientn(colours = c("white", "white", "#D8BFD8", "#BA55D3", "#6A0DAD")) +
      annotate("rect", xmin = -1, xmax = 1,
               ymin = 1.6, ymax = 3.4,
               fill = NA, color = "black", alpha = 0.1) +
      ylim(1, 4) + xlim(-1.8, 1.8) +
      theme_bw() + xlab("Horizontal Pitch Location") + ylab("Vertical Pitch Location") +
      ggtitle("Pitch Location Heat Map", subtitle = "Pitcher's Perspective") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5))+
      guides(fill = guide_legend(title = "Density"))
    print(pitcherHeatmap)
  })
  
  # Add parentheses to call the function
    output$pitcherHeatmap2 <- renderPlot({
      selected_pitcher <- selected_pitcher_data()  # Retrieve the data
      
      # Define pitch colors for different pitch types
      pitch_colors <- c("Fastball" = "#F21500", 
                        "Slider" = "#EDC10C", 
                        "Changeup" = "#49CB21", 
                        "Splitter" = "#49CB21", 
                        "Curveball" = "#4BD2FF", 
                        "Cutter" = "#743F00")
      
      # Ensure the `TaggedPitchType` column exists in the dataset
      if (!"TaggedPitchType" %in% colnames(selected_pitcher)) {
        stop("The column `TaggedPitchType` is missing in the dataset.")
      }
      
      # Filter pitch colors to include only valid types present in the dataset
      valid_pitch_types <- intersect(names(pitch_colors), unique(selected_pitcher$TaggedPitchType))
      pitch_colors <- pitch_colors[valid_pitch_types]
      
      # Create the heatmap
      PitchOutcomes <- ggplot(selected_pitcher, aes(x = PlateLocSide, y = PlateLocHeight, z = Results)) +
        stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = FALSE) +
        geom_point(aes(color = TaggedPitchType), size = 2, alpha = 0.7) +  # Map color to TaggedPitchType
        scale_fill_gradientn(colours = c("white", "white", "#D8BFD8", "#BA55D3", "#6A0DAD")) +
        scale_color_manual(values = pitch_colors) +  # Apply filtered pitch colors
        annotate("rect", xmin = -0.8, xmax = 0.8, ymin = 1.4, ymax = 3.5, color = "black", fill = NA) +
        xlim(-1.4, 1.4) + ylim(1, 4) +
        xlab("Horizontal Pitch Location") +
        ylab("Vertical Pitch Location") +
        coord_equal() +
        ggtitle("Strategic Locations Heatmap", subtitle = "Calculated Density of Pitch Outcomes") +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5), 
              plot.subtitle = element_text(hjust = 0.5)) +
        guides(fill = guide_legend(title = "Density"), color = guide_legend(title = "Pitch Type"))
      
      print(PitchOutcomes)  # Render the plot
})
  
  output$stuffPlot <- renderPlot({
    selected_stuff <- selected_stuff_data()
    
    # Define pitch colors and ensure they match the TaggedPitchType levels in the data
    pitch_colors <- c("Fastball" = "#F21500", 
                      "Slider" = "#EDC10C", 
                      "Changeup" = "#49CB21", 
                      "Splitter" = "#49CB21", 
                      "Curveball" = "#4BD2FF", 
                      "Cutter" = "#743F00")    
    # Only retain colors for existing pitch types in the data
    valid_pitch_types <- intersect(names(pitch_colors), unique(selected_stuff$TaggedPitchType))
    pitch_colors <- pitch_colors[valid_pitch_types]
    
    # Scale colors with valid levels
    pitch_type_colors <- scale_color_manual(values = pitch_colors)
    
    # Ensure data is sorted by Date and PitchNo, then calculate running velocity for each pitch type
    selected_stuff <- selected_stuff %>%
      arrange(TaggedPitchType, Date, PitchNo) %>%
      group_by(TaggedPitchType) %>%
      mutate(RunningStuff = cumsum(Stuff) / row_number(),
             PitchSeq = row_number()) %>%
      ungroup()
    
    # Plot with a line for each pitch type
    ggplot(selected_stuff, aes(x = PitchSeq, y = Stuff, color = TaggedPitchType, group = TaggedPitchType)) +
      geom_line(size = 1.2) +
      geom_point() +
      ylim(50, 180)+
      scale_x_continuous(breaks = seq(min(selected_stuff$PitchSeq), max(selected_stuff$PitchSeq), by = 1))+
      labs(
        title = paste("Stuff + by Pitch Type for", input$pitcherDropdown),
        x = "Pitch Number",
        y = "Stuff +"
      ) +
      pitch_type_colors +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 24, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$stuff_data_table <- renderDT({
    selected_stuff <- selected_stuff_data() 
    TableData <- selected_stuff %>% select(Date,PitchNo, Pitcher, PitcherTeam, Batter,TaggedPitchType, PitchCall, Stuff,RelSpeed, SpinRate, InducedVertBreak, HorzBreak, RelHeight, RelSide, Extension ) %>% mutate_if(is.numeric, ~ round(., 2)) 
    
    # Render the table using datatable from DT package
    datatable(
      TableData,
      options = list(
        pageLength = 20,  # Show 10 rows per page
        autoWidth = TRUE,  # Adjust column widths automatically
        scrollX = TRUE     # Enable horizontal scrolling if needed
      ),
      rownames = FALSE  # Remove row names
    )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


