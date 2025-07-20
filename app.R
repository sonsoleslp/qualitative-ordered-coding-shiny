library(shiny)
library(readxl)
library(writexl)
library(RColorBrewer)
library(markdown)

# Define your label list
label_list <- c("Problem Context: Programming Concepts",
                "Problem Context: Programming Constructs",
                "Problem Context: Domain-specific Knowledge",
                "Problem Specification: Rephrasing the Problem Statement",
                "Problem Specification: Questioning Details about the Requirements",
                "Problem Specification: Decomposing the Problem into Smaller Parts",
                "Problem Specification: Thinking of a Corresponding Output for a Certain Input",
                "Iterative Conversation: Based on Different Levels of Abstraction",
                "Iterative Conversation: Based on Specificity of Prompts",
                "Evaluate and Adapt Solution: Solution Explanation",
                "Evaluate and Adapt Solution: Evaluation Using Test Cases",
                "Evaluate and Adapt Solution: Problem Decomposition",
                "Evaluate and Adapt Solution: Inspect and Adapt",
                "SRL: Planning",
                "SRL: Process Monitoring",
                "SRL: Comprehension Monitoring",
                "SRL: Reflection on Cognition",
                "SRL: Motivation and Affect")
# label_colors <- brewer.pal(n = length(label_list), name = "Set2")
label_colors <- brewer.pal(n = 5, name = "Set2")
label_colors <- c(rep(label_colors[1],3),rep(label_colors[2],4),rep(label_colors[3],2),rep(label_colors[4],4),rep(label_colors[5],5))
names(label_colors) <- label_list

ui <- fluidPage(
  titlePanel("🔖 Multi-Click Excel Labeler"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel File", accept = ".xlsx"),
      uiOutput("column_selector"),
      textOutput("progress"),
      actionButton("prev", "⬅️ Previous"),
      actionButton("nextt", "Next ➡️"),
      actionButton("reset", "Reset Label", class = "btn btn-danger mt-2"),
      hr(),
      h4("Click to label (adds to list):"),
      uiOutput("label_buttons"),
      br(), br(),
      downloadButton("download", "📅 Download Labeled Excel")
    ),
    
    mainPanel(
      h4("Current Row Content"),
      uiOutput("row_content"),
      h4("Current Labels"),
      uiOutput("label_display")
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    data = NULL,
    column = NULL,
    index = 1,
    label_seq = character(),
    user_changed_column = FALSE
  )
  
  observeEvent(input$file, {
    req(input$file)
    rv$data <- read_excel(input$file$datapath)
    rv$index <- 1
    rv$label_seq <- character()
  })
  
  output$column_selector <- renderUI({
    req(rv$data)
    selectInput("column", "Select column to classify",
                choices = names(rv$data)[!grepl("_label$", names(rv$data))],
                selected = isolate(input$column))
  })
  
  observeEvent(input$column, {
    rv$column <- input$column
    label_col <- paste0(rv$column, "_label")
    if (!(label_col %in% names(rv$data))) {
      rv$data[[label_col]] <- ""
    }
    current_value <- rv$data[[label_col]][rv$index]
    rv$label_seq <- if (!is.na(current_value) && nzchar(current_value)) strsplit(current_value, ";")[[1]] else character()
  }, ignoreInit = TRUE)
  
  observeEvent(input$nextt, {
    req(rv$data)
    if (rv$index < nrow(rv$data)) {
      rv$index <- rv$index + 1
      current_value <- rv$data[[paste0(rv$column, "_label")]][rv$index]
      rv$label_seq <- if (!is.na(current_value) && nzchar(current_value)) strsplit(current_value, ";")[[1]] else character()
    }
  })
  
  observeEvent(input$prev, {
    req(rv$data)
    if (rv$index > 1) {
      rv$index <- rv$index - 1
      current_value <- rv$data[[paste0(rv$column, "_label")]][rv$index]
      rv$label_seq <- if (!is.na(current_value) && nzchar(current_value)) strsplit(current_value, ";")[[1]] else character()
    }
  })
  
  observeEvent(input$reset, {
    req(rv$data, rv$column)
    rv$label_seq <- character()
    rv$data[[paste0(rv$column, "_label")]][rv$index] <- ""
  })
  
  
  output$label_buttons <- renderUI({
    tagList(
      lapply(seq_along(label_list), function(i) {
        label <- label_list[i]
        id <- paste0("label_", i)
        actionButton(
          inputId = id,
          label = label,
          style = paste0("background-color:", label_colors[[label]], "; color: black; margin: 2px; font-size: 10px; padding: 4px 6px;")
        )
      })
    )
  })
  
  for (i in seq_along(label_list)) {
    local({
      label <- label_list[i]
      input_id <- paste0("label_", i)
      
      observeEvent(input[[input_id]], {
        isolate({
          rv$label_seq <- c(rv$label_seq, label)
          rv$data[[paste0(rv$column, "_label")]][rv$index] <- paste(rv$label_seq, collapse = ";")
        })
      })
    })
  }
  
  
  output$row_content <- renderUI({
    req(rv$data, rv$column)
    text <- rv$data[[rv$column]][rv$index]
    HTML(markdown::markdownToHTML(text = text, fragment.only = TRUE))
  })
  
  output$label_display <- renderUI({
    if (length(rv$label_seq) == 0) {
      HTML("<em>(No labels yet)</em>")
    } else {
      tags$div(
        lapply(rv$label_seq, function(label) {
          span(
            label,
            class = "badge me-1",
            style = paste0("background-color:", label_colors[[label]], "; color: black;")
          )
        })
      )
    }
  })
  
  output$progress <- renderText({
    req(rv$data, rv$column)
    col <- paste0(rv$column, "_label")
    done <- sum(rv$data[[col]] != "")
    total <- nrow(rv$data)
    paste("Progress:", done, "of", total, "rows labeled")
  })
  
  output$download <- downloadHandler(
    filename = function() {
      "labeled_output.xlsx"
    },
    content = function(file) {
      writexl::write_xlsx(rv$data, file)
    }
  )
}

shinyApp(ui, server)
