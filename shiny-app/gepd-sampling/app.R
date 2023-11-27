
## Libraries
library(shiny)
library(readxl)
library(writexl)
library(leaflet)

### Creating the shiny app

#####  User Interface
ui <-  fluidPage(
  titlePanel("GEPD Sampling Tool"),
  sidebarLayout(
    sidebarPanel(
      tags$p(style = "text-align: center; font-weight: bold;", 
             "READ BEFORE SAMPLING"),
      tags$p(style = "text-align: center;", 
             "Please, download the sample template file below, which contains the fcolumns that must be present in the file you will be uploading."),
      div(style = "text-align: center;",
          downloadButton("download_template",
                     "Download Template")),
      fileInput("file1", "Choose CSV or Excel File",
                accept = c(".csv", ".xlsx")),
      numericInput("sample_size",
                   "Select Sample Size", value = 100, min = 100, max = 350),
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
  
  # Observe when the file is uploaded and read the data
  observeEvent(input$file1, {
    file <- input$file1
    ext <- tools::file_ext(file$name)
    if (ext == "csv") {
      data <- read.csv(file$datapath, stringsAsFactors = FALSE)
    } else if (ext == "xlsx") {
      data <- read_excel(file$datapath)
    }
    uploaded_data(data)
  })
  
  # Observe when the 'Run Sample' button is clicked
  observeEvent(input$run_sample, {
    # Ensure the sample size does not exceed the number of rows in the data
    req(uploaded_data())  # Ensure data is uploaded
    data <- uploaded_data()
    actual_sample_size <- min(input$sample_size, nrow(data))
    data$selected <- rep(FALSE, nrow(data))  # Default all to FALSE
    selected_rows <- sample(nrow(data), actual_sample_size, replace = FALSE)
    data$selected[selected_rows] <- TRUE
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
    if ("school_name" %in% names(data)) {
      popup_info <- paste(data$school_name, data$school_code, sep=" - ")
    } else {
      # Convert school_code to character if not already
      popup_info <- as.character(data$school_code)
    }
    # Add markers to the map
    map <- map %>% addCircleMarkers(lng = ~long, lat = ~lat,
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

## Running the app
shinyApp(ui = ui, server = server)
