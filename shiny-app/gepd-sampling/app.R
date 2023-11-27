
## Libraries
library(shiny)
library(tidyverse)
library(readxl)
library(writexl)
library(leaflet)
library(httr)

### Creating the shiny app

#####  User Interface
ui <-  fluidPage(
  titlePanel("GEPD Sampling Tool"),
  sidebarLayout(
    sidebarPanel(
      tags$p(style = "text-align: center; font-weight: bold;", 
             "READ BEFORE SAMPLING"),
      tags$p(style = "text-align: center;", 
             "Please, download the sample template file below, which contains the columns that must be present in the file you will be uploading."),
      div(style = "text-align: center;",
          downloadButton("download_template",
                     "Download Template")),
      
      # Horizontal separator - uploading file
      tags$hr(style = "border-top: 3px solid #ccc; width: 90%;"),
      tags$p(style = "text-align: center; font-weight: bold;", 
             "PROCEED TO SAMPLING"),
      tags$p(style = "text-align: center;", 
             "Now that you have adjusted your file to the template foramt, you may proceed with sampling"),
      fileInput("file1", "Choose CSV or Excel File",
                accept = c(".csv", ".xlsx")),
      
      # Horizontal separator - setting the sampling main characteristics
      tags$hr(style = "border-top: 3px solid #ccc; width: 90%;"),
      tags$p(style = "text-align: center; font-weight: bold;", 
             "SAMPLING PARAMETERS"),
      numericInput("sample_size",
                   "Select Sample Size:", value = 100, min = 100, max = 350),
      selectInput("strata1", "Select Stratification Option 1",
                  choices = NULL),  # Dynamically populated based on the uploaded file
      selectInput("strata2", "Select Stratification Option 2",
                  choices = NULL),  # Dynamically populated based on the uploaded file
      div(style = "text-align: center;",
          actionButton("run_sample",
                       "Run Sample")),
      tags$p(style = "text-align: center; font-weight: bold;",
             "Download your results here:"),
      downloadButton("download_data",
                     "Download Sampled File")
    ),
    mainPanel(
      tags$h3("Map with the Schools", style = "text-align: center;"),
      leafletOutput("map", width = "100%", height = "600px")
    )
  )
)

#####  User Interface

server = function(input, output, session) {
  # Reactive value for the uploaded data
  uploaded_data <- reactiveVal()
  sampled_data <- reactiveVal()
  
  # Download handler for the template file from GitHub URL
  output$download_template <- downloadHandler(
    filename = function() {
      "SB4_School_Information.xlsx"
    },
    content = function(file) {
      url <- "https://github.com/ArthurCheib/sampling-mvp/raw/main/shiny-app/gepd-sampling/materials/SB4_School_Information.xlsx"
      GET(url, write_disk(file, overwrite = TRUE))
    }
  )
  
  # Observe when the file is uploaded and read the data
  observeEvent(input$file1, {
    file <- input$file1
    ext <- tools::file_ext(file$name)
    if (ext == "csv") {
      data <- read.csv(file$datapath, stringsAsFactors = FALSE)
    } else if (ext == "xlsx") {
      data <- read_excel(file$datapath)
    }
    ## Store the uploaded data
    uploaded_data(data)
    
    # Update choices for stratification columns when a file is uploaded
    updateSelectInput(session, "strata1", choices = names(data))
    updateSelectInput(session, "strata2", choices = c("", names(data)))
    
  })
  
  # Observe when the 'Run Sample' button is clicked
  observeEvent(input$run_sample, {
    req(uploaded_data())  # Ensure data is uploaded
    data <- uploaded_data()
    
    # Stratification logic
    if (input$strata1 != "" && input$strata2 != "") {
      # Stratify by two columns
      data <- stratify_sample(data, c(input$strata1, input$strata2), input$sample_size)
    } else if (input$strata1 != "") {
      # Stratify by one column
      data <- stratify_sample(data, input$strata1, input$sample_size)
    } else {
      # Simple random sample if no stratification column is selected
      actual_sample_size <- min(input$sample_size, nrow(data))
      data$selected <- rep(FALSE, nrow(data))
      selected_rows <- sample(nrow(data), actual_sample_size, replace = FALSE)
      data$selected[selected_rows] <- TRUE
    }
    ## Return of this function
    sampled_data(data)
  })
  
  # Map rendering
  default_map <- leaflet() %>% 
    addTiles() %>% 
    setView(lng = -51.9253, lat = -14.2350, zoom = 4) 
  
  output$map <- renderLeaflet({
    
    data <- sampled_data()
    
    if (is.null(data)) {
      
      return(default_map) }
    
    else {
      # Assuming 'lat' and 'long' are the names of the columns
      map <- leaflet(data) %>% addTiles()
      
    }
    # Check if 'school_name' col exists + use it popup, otherwise use 'school_code'
    if ("School Name" %in% names(data)) {
      popup_info <- paste(data$`School Name`, data$`School ID`, sep=" - ")
    } else {
      # Convert school_code to character if not already
      popup_info <- as.character(data$`School ID`)
    }
    # Add markers to the map
    map <- map %>% addCircleMarkers(lng = ~longitude, lat = ~latitude,
                                    color = ~ifelse(selected, "red", "blue"),
                                    popup = ~popup_info)
    return(map)
  }
  )
  
  # Download handler for the sampled file
  output$download_data <- downloadHandler(
    filename = function() {
      paste("sampled_data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      data <- sampled_data()
      if (!is.null(data)) {
        write_xlsx(data, file)
      }
    }
  )
}

# Function for stratified sampling with proportional allocation
stratify_sample <- function(data, strata_cols, total_sample_size) {
  # Ensure strata_cols is a character vector
  strata_cols <- as.character(strata_cols)
  
  # Calculate the size of each stratum and its proportion
  strata_sizes <- data %>%
    group_by(across(all_of(strata_cols))) %>%
    summarise(count = n(), .groups = 'drop') %>%
    mutate(prop = count / sum(count))
  
  # Function to calculate sample size for each stratum
  calc_stratum_sample_size <- function(stratum_size, total_population_size, total_sample_size) {
    round(stratum_size / total_population_size * total_sample_size)
  }
  
  # Calculate sample sizes for each stratum
  strata_sizes$sample_size <- pmin(map2_dbl(strata_sizes$count, strata_sizes$prop, ~calc_stratum_sample_size(.x, sum(strata_sizes$count), total_sample_size)), strata_sizes$count)
  
  # Initialize an empty dataframe to store sampled data
  sampled_data <- data[FALSE, ]
  
  # Iterate over each stratum and sample
  for (i in seq_len(nrow(strata_sizes))) {
    stratum_data <- data
    for (col in strata_cols) {
      stratum_data <- stratum_data[stratum_data[[col]] == strata_sizes[[col]][i], ]
    }
    sample_size <- min(nrow(stratum_data), strata_sizes$sample_size[i])
    
    # Sample from the stratum
    if (nrow(stratum_data) > 0 && sample_size > 0) {
      stratum_sample <- stratum_data[sample(nrow(stratum_data), size = sample_size, replace = FALSE), ]
      sampled_data <- rbind(sampled_data, stratum_sample)
    }
  }
  sampled_data$selected <- TRUE
  return(sampled_data)
}



## Running the app
shinyApp(ui = ui, server = server)
