# =====================================================
# Assignment 10
# Course: Exploratory Data Analysis for R Programming
# Student: Arni Mualumbsa
# Professor: Dr. Ge
# Date: November 5, 2026
#
# Topic: Fatal Traffic Crash Analysis in the USA (2024)
#
# Purpose:
# Explore the geographical distribution of fatal
# traffic crashes and the factors associated with
# traffic fatalities across U.S. states.
#
# AI Assistance:
# ChatGPT (OpenAI) assisted me with the development,
# organization, and debugging of the R Shiny application.
# =====================================================




# USA 2024 Fatal Crash Geography | Arny Mwalumba

library(shiny)
library(dplyr)
library(ggplot2)
library(scales)

# =====================================================
# 1. MAPS AND PICTURES
# =====================================================

prepare_folder <- function(folder, zip_names) {
  
  if (dir.exists(folder)) return(normalizePath(folder))
  
  archive <- zip_names[file.exists(zip_names)][1]
  
  if (!is.na(archive)) unzip(archive, exdir = ".")
  
  if (dir.exists(folder)) normalizePath(folder) else NULL
}

maps_dir <- prepare_folder(
  "Interactive_maps",
  c("Interactive_maps.zip", "Interactive_maps(1).zip")
)

pic_dir <- prepare_folder("pic", "pic.zip")

if (!is.null(maps_dir)) addResourcePath("maps", maps_dir)
if (!is.null(pic_dir)) addResourcePath("pictures", pic_dir)

map_files <- if (is.null(maps_dir)) character(0) else
  sort(list.files(
    maps_dir,
    pattern = "\\.(html|png|jpg|jpeg)$",
    ignore.case = TRUE,
    recursive = TRUE
  ))

pic_files <- if (is.null(pic_dir)) character(0) else
  sort(list.files(
    pic_dir,
    pattern = "\\.(png|jpg|jpeg)$",
    ignore.case = TRUE,
    recursive = TRUE
  ))

# =====================================================
# 2. STATE DATA
# =====================================================

# Load the full dataset, not data_1.csv

if (file.exists("data_accident.csv")) {
  
  crashes <- read.csv("data_accident.csv")
  
} else if (exists("data_accident", envir = .GlobalEnv)) {
  
  crashes <- get("data_accident", envir = .GlobalEnv)
  
} else {
  
  stop("Save data_accident as data_accident.csv beside app.R")
}

required <- c(
  "STATE", "ST_CASE", "STATENAME", "ROUTENAME", "FATALS"
)

missing <- setdiff(required, names(crashes))

if (length(missing) > 0) {
  stop(paste(
    "Wrong dataset. Missing:",
    paste(missing, collapse = ", "),
    "- Export the full data_accident dataset."
  ))
}

crashes <- crashes %>%
  distinct(STATE, ST_CASE, .keep_all = TRUE) %>%
  mutate(
    STATENAME = trimws(as.character(STATENAME)),
    ROUTENAME = trimws(as.character(ROUTENAME)),
    FATALS = as.numeric(FATALS)
  ) %>%
  filter(
    !is.na(STATENAME),
    STATENAME != "",
    STATENAME != "Puerto Rico",
    !is.na(ROUTENAME),
    ROUTENAME != "",
    !is.na(FATALS),
    FATALS >= 1
  )

states <- c("All", sort(unique(crashes$STATENAME)))

# =====================================================
# 3. USER INTERFACE
# =====================================================

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {background:#fafafa;}
      .well {background:white;}
      iframe {border:0;}
    "))
  ),
  
  titlePanel(
    "Factors Explaining Fatal Crashes in the USA (2024)"
  ),
  
  p("USA 2024 Fatal Crash Geography | Arny Mwalumba"),
  
  fluidRow(
    
    # Navigation on the left
    column(
      3,
      wellPanel(
        h4("Navigation"),
        
        selectInput(
          "section",
          "Choose a section",
          choices = c(
            "1. Interactive Maps" = "maps",
            "2. Pictures" = "pics",
            "3. States Analysis" = "states"
          )
        ),
        
        uiOutput("controls")
      )
    ),
    
    # Graphs on the right
    column(
      9,
      uiOutput("display")
    )
  )
)

# =====================================================
# 4. SERVER
# =====================================================

server <- function(input, output, session) {
  
  # Navigation
  output$controls <- renderUI({
    
    if (input$section == "maps") {
      
      tagList(
        selectInput(
          "map_choice",
          "Select map or graph",
          choices = map_files
        ),
        
        fluidRow(
          column(6, actionButton("map_prev", "Previous",
                                 width = "100%")),
          column(6, actionButton("map_next", "Next",
                                 width = "100%"))
        )
      )
      
    } else if (input$section == "pics") {
      
      tagList(
        selectInput(
          "pic_choice",
          "Select picture",
          choices = pic_files
        ),
        
        fluidRow(
          column(6, actionButton("pic_prev", "Previous",
                                 width = "100%")),
          column(6, actionButton("pic_next", "Next",
                                 width = "100%"))
        )
      )
      
    } else {
      
      tagList(
        selectInput(
          "state_choice",
          "Select State",
          choices = states,
          selected = "All"
        ),
        
        fluidRow(
          column(6, actionButton("state_prev", "Previous",
                                 width = "100%")),
          column(6, actionButton("state_next", "Next",
                                 width = "100%"))
        ),
        
        tags$hr(),
        p("Blue: 1 Fatality"),
        p("Red: More Than 1 Fatality")
      )
    }
  })
  
  # Previous / Next buttons
  move_choice <- function(current, choices, step, id) {
    
    if (length(choices) == 0) return()
    
    position <- match(current, choices)
    
    if (is.na(position)) position <- 1
    
    position <- ((position - 1 + step) %%
                   length(choices)) + 1
    
    updateSelectInput(
      session, id,
      selected = choices[position]
    )
  }
  
  observeEvent(input$map_prev, {
    move_choice(input$map_choice, map_files, -1, "map_choice")
  })
  
  observeEvent(input$map_next, {
    move_choice(input$map_choice, map_files, 1, "map_choice")
  })
  
  observeEvent(input$pic_prev, {
    move_choice(input$pic_choice, pic_files, -1, "pic_choice")
  })
  
  observeEvent(input$pic_next, {
    move_choice(input$pic_choice, pic_files, 1, "pic_choice")
  })
  
  observeEvent(input$state_prev, {
    move_choice(input$state_choice, states, -1, "state_choice")
  })
  
  observeEvent(input$state_next, {
    move_choice(input$state_choice, states, 1, "state_choice")
  })
  
  # =====================================================
  # 5. DISPLAY MAPS, PICTURES, STATES
  # =====================================================
  
  output$display <- renderUI({
    
    if (input$section == "maps") {
      
      req(input$map_choice)
      
      file <- input$map_choice
      
      url <- paste0(
        "maps/",
        URLencode(file, reserved = FALSE)
      )
      
      tagList(
        h3(tools::file_path_sans_ext(basename(file))),
        
        if (grepl("\\.html$", file, ignore.case = TRUE)) {
          
          tags$iframe(
            src = url,
            width = "100%",
            height = "750px"
          )
          
        } else {
          
          tags$img(
            src = url,
            style = "width:100%;height:auto;"
          )
        }
      )
      
    } else if (input$section == "pics") {
      
      req(input$pic_choice)
      
      tagList(
        h3(tools::file_path_sans_ext(
          basename(input$pic_choice)
        )),
        
        tags$img(
          src = paste0(
            "pictures/",
            URLencode(input$pic_choice, reserved = FALSE)
          ),
          style = "width:100%;height:auto;"
        )
      )
      
    } else {
      
      req(input$state_choice)
      
      tagList(
        h3(paste("State:", input$state_choice)),
        
        plotOutput(
          "state_plot",
          height = "700px"
        )
      )
    }
  })
  
  # =====================================================
  # 6. STATES ANALYSIS
  # =====================================================
  
  output$state_plot <- renderPlot({
    
    req(input$section == "states")
    req(input$state_choice)
    
    # All states or individual state
    state_data <- crashes
    
    if (input$state_choice != "All") {
      state_data <- state_data %>%
        filter(STATENAME == input$state_choice)
    }
    
    # Fatalities by route
    route_fatalities <- state_data %>%
      mutate(
        Fatality_Group = ifelse(
          FATALS == 1,
          "1 Fatality",
          "More Than 1 Fatality"
        )
      ) %>%
      group_by(ROUTENAME, Fatality_Group) %>%
      summarise(
        Total_Deaths = sum(FATALS),
        .groups = "drop"
      ) %>%
      group_by(ROUTENAME) %>%
      mutate(
        Route_Total = sum(Total_Deaths),
        Segment_Percentage =
          100 * Total_Deaths / Route_Total
      ) %>%
      ungroup()
    
    validate(
      need(
        nrow(route_fatalities) > 0,
        "No route data available for this state."
      )
    )
    
    # Totals and percentages
    route_totals <- route_fatalities %>%
      distinct(ROUTENAME, Route_Total) %>%
      arrange(desc(Route_Total)) %>%
      mutate(
        Overall_Percentage =
          100 * Route_Total / sum(Route_Total)
      )
    
    # Order routes
    route_order <- route_totals$ROUTENAME
    
    route_fatalities <- route_fatalities %>%
      mutate(
        ROUTENAME = factor(
          ROUTENAME,
          levels = route_order
        ),
        
        Fatality_Group = factor(
          Fatality_Group,
          levels = c(
            "More Than 1 Fatality",
            "1 Fatality"
          )
        )
      )
    
    route_totals <- route_totals %>%
      mutate(
        ROUTENAME = factor(
          ROUTENAME,
          levels = route_order
        )
      )
    
    # Barplot
    ggplot(
      route_fatalities,
      aes(
        x = ROUTENAME,
        y = Total_Deaths,
        fill = Fatality_Group
      )
    ) +
      
      geom_col(
        width = 0.8,
        color = "white",
        linewidth = 0.4
      ) +
      
      geom_text(
        aes(
          label = ifelse(
            Segment_Percentage >= 4,
            paste0(
              round(Segment_Percentage, 1), "%"
            ),
            ""
          )
        ),
        position = position_stack(vjust = 0.5),
        color = "white",
        fontface = "bold",
        size = 3.3
      ) +
      
      geom_text(
        data = route_totals,
        aes(
          x = ROUTENAME,
          y = Route_Total,
          label = paste0(
            round(Overall_Percentage, 1), "%"
          )
        ),
        inherit.aes = FALSE,
        vjust = -0.5,
        size = 3.5,
        fontface = "bold"
      ) +
      
      scale_fill_manual(
        values = c(
          "1 Fatality" = "#2471A3",
          "More Than 1 Fatality" = "#E74C3C"
        )
      ) +
      
      scale_y_continuous(
        labels = comma,
        expand = expansion(mult = c(0, 0.12))
      ) +
      
      labs(
        title = paste(
          "Traffic Fatalities by Route Type and Fatality Severity:",
          input$state_choice
        ),
        subtitle = "United States, 2024",
        x = "Route Type",
        y = "Number of Fatalities",
        fill = "Fatality Group"
      ) +
      
      theme_minimal(base_size = 12) +
      
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        panel.grid.major.x = element_blank(),
        legend.position = "right",
        plot.title = element_text(face = "bold")
      )
  })
}

# =====================================================
# 7. RUN APPLICATION
# =====================================================

shinyApp(ui = ui, server = server)
