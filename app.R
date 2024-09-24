library(shiny)
library(httr)
library(jsonlite)
library(DT)
library(stringr)
library(tidyverse)
library(bslib)
library(janitor)
library(shinyjs)
library(shinycssloaders)

# Internal API key (replace with your actual API key)
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")

ui <- page_fluid(
  theme = bs_theme(version = 5),
  useShinyjs(),  # Add this to use shinyjs
  br(),
  titlePanel("AI Dataset Generator"),
  sidebarLayout(
    sidebarPanel(
      textInput("description", "Describe the dataset you want", 
                placeholder = "e.g., health data for a family of 4"),
      actionButton("generate", "Generate Dataset"),
      hidden(downloadButton("download", "Download CSV")),  # Hide download button initially
      br(), br(),
      uiOutput("summary"),
      hr(),
      tags$small("Note: Generated data may not be accurate or suitable for real-world use. The maximum number of records is limited to 25.")
    ),
    mainPanel(
      navset_tab(
        nav_panel("Data Table", 
                  br(),
                DTOutput("dataset")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactiveVal(NULL)
  summary_text <- reactiveVal("")
  
  preprocess_csv <- function(csv_string) {
    # Extract only the CSV part
    csv_pattern <- "(?s)(.+?\\n(?:[^,\n]+(?:,[^,\n]+)*\n){2,})"
    csv_match <- str_extract(csv_string, csv_pattern)
    
    if (is.na(csv_match)) {
      stop("No valid CSV data found in the response")
    }
    
    lines <- str_split(csv_match, "\n")[[1]]
    lines <- lines[lines != ""]  # Remove empty lines
    
    # Get the number of columns from the header
    header <- str_split(lines[1], ",")[[1]]
    num_cols <- length(header)
    
    # Ensure all rows have the same number of columns
    processed_lines <- sapply(lines[-1], function(line) {  # Skip header
      cols <- str_split(line, ",")[[1]]
      if (length(cols) < num_cols) {
        cols <- c(cols, rep("", num_cols - length(cols)))
      } else if (length(cols) > num_cols) {
        cols <- cols[1:num_cols]
      }
      cols
    })
    
    # Create a tibble
    tibble(!!!setNames(as.list(as.data.frame(t(processed_lines))), header))
  }
  
  generate_summary <- function(df) {
    prompt <- paste("Summarize the following dataset:\n\n",
                    "Dimensions: ", nrow(df), "rows and", ncol(df), "columns\n\n",
                    "Variables:\n", paste(names(df), collapse=", "), "\n\n",
                    "Please provide a brief summary of the dataset dimensions and variable definitions. Keep it concise, about 3-4 sentences.")
    
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", OPENAI_API_KEY)),
      content_type_json(),
      body = toJSON(list(
        model = "gpt-3.5-turbo-0125",
        messages = list(
          list(role = "system", content = "You are a helpful assistant that summarizes datasets."),
          list(role = "user", content = prompt)
        )
      ), auto_unbox = TRUE),
      encode = "json"
    )
    
    if (status_code(response) == 200) {
      content <- content(response)
      summary <- content$choices[[1]]$message$content
      return(summary)
    } else {
      return("Error generating summary. Please try again later.")
    }
  }
  
  observeEvent(input$generate, {
    req(input$description)
    showPageSpinner()
    
    prompt <- paste("Generate a fake dataset with at least two variables as a CSV string based on this description:",
                    input$description, "Include a header row. Limit to 25 rows of data. Ensure all rows have the same number of columns. Do not include any additional text or explanations.")
    
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", OPENAI_API_KEY)),
      content_type_json(),
      body = toJSON(list(
        model = "gpt-3.5-turbo-0125",
        messages = list(
          list(role = "system", content = "You are a helpful assistant that generates fake datasets."),
          list(role = "user", content = prompt)
        )
      ), auto_unbox = TRUE),
      encode = "json"
    )
    
    if (status_code(response) == 200) {
      content <- content(response)
      csv_string <- content$choices[[1]]$message$content
      
      tryCatch({
        # Preprocess the CSV string and create a tibble
        df <- preprocess_csv(csv_string) %>% clean_names() %>% 
          mutate(across(everything(), ~ ifelse(suppressWarnings(!is.na(as.numeric(.))), as.numeric(.), as.character(.))))
        dataset(df)
        updateSelectInput(session, "variable", choices = names(df))
        
        # Generate and set summary
        summary <- generate_summary(df)
        summary_text(summary)
        
        # Show download button
        hidePageSpinner()
        shinyjs::show("download")
        
        
        
      }, error = function(e) {
        showNotification(paste("Error parsing CSV:", e$message), type = "error")
      })
    } else {
      showNotification("Error generating dataset. Please try again later.", type = "error")
    }
    
    # Hide loading spinner
    shinyjs::hide("loading-spinner")
  })
  
  output$dataset <- renderDT({
    req(dataset())
    datatable(dataset(), rownames = FALSE, options = list(pageLength = 10))
  })
  
  
  output$download <- downloadHandler(
    filename = function() {
      "generated_dataset.csv"
    },
    content = function(file) {
      req(dataset())
      write.csv(dataset(), file, row.names = FALSE)
    }
  )
  
  output$summary <- renderUI({
    req(summary_text())
    div(
      h4("Dataset Summary"),
      p(summary_text()),
      style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;"
    )
  })
}

shinyApp(ui, server)
