
library(shiny)
library(dplyr)

#load dataset
data <- read.csv("data/data_formatted.csv")


# UI
ui <- fluidPage(
  titlePanel("Species Identification Quiz"),
  sidebarLayout(
    sidebarPanel(
      h4("Quiz Settings"),
      radioButtons("source", "Select SBS choice:", choices = c("200 only" = "200", "200 & 400" = "all")),
      selectInput("family", "Choose a Family:", choices = NULL),
      numericInput("num_questions", "Number of Questions:", value = 10, min = 5, max = 200),
      actionButton("start", "Start Quiz"),
      actionButton("restart", "Restart Quiz")
    ),
    mainPanel(
      uiOutput("quiz_ui"),
      textOutput("score"),
      conditionalPanel(
        condition = "output.quiz_over == true",
        h3("Wrongs were:"),
        tableOutput("incorrect_answers")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  quiz_data <- reactiveVal(NULL)
  current_question <- reactiveVal(1)
  score <- reactiveVal(0)
  incorrect_list <- reactiveVal(data.frame(Genus = character(), Species = character(), Family = character()))
  quiz_over <- reactiveVal(FALSE)
  
  # Update family choices based on selection
  observeEvent(input$source, {
    filtered_data <- if (input$source == "200") {
      data %>% filter(source == 200)
    } else {
      data
    }
    updateSelectInput(session, "family", choices = c("All", unique(filtered_data$family)))
  })
  
  # Function to start or restart quiz
  generate_quiz <- function() {
    filtered_data <- data %>%
      filter(source == 200 | (input$source == "all")) %>%
      filter(if (input$family != "All") family == input$family else TRUE)
    
    # Stratified sampling to balance species representation
    set.seed(Sys.time())
    quiz_samples <- filtered_data %>%
      group_by(scientific_name_2) %>%
      sample_n(size = min(ceiling(input$num_questions / n_distinct(filtered_data$scientific_name_2)), n()), replace = FALSE) %>%
      ungroup() %>%
      sample_n(input$num_questions)
    
    quiz_data(quiz_samples)
    current_question(1)
    score(0)
    incorrect_list(data.frame(Genus = character(), Species = character(), Family = character()))
    quiz_over(FALSE)
  }
  
  observeEvent(input$start, { generate_quiz() })
  observeEvent(input$restart, { generate_quiz() })
  
  # Display Quiz UI
  output$quiz_ui <- renderUI({
    req(quiz_data())
    q <- quiz_data()[current_question(), ]
    
    tagList(
      h3(paste("Question", current_question(), "of", nrow(quiz_data()))),
      fluidRow(
        column(6,
               textInput("genus_input", "Genus:"),
               textInput("species_input", "Species:"),
               textInput("family_input", "Family:"),
               actionButton("submit", "Submit")
        ),
        column(6,
               tags$div(
                 style = "text-align: center;",
                 tags$img(src = q$image_url, width = "90%", height = "auto", style = "border: 2px solid #ccc; cursor: zoom-in;",
                          onclick = "this.style.cursor='zoom-out'; this.style.width='100%'; this.style.height='auto';")
               )
        )
      )
    )
  })
  
  # Check Answer
  observeEvent(input$submit, {
    req(quiz_data())
    q <- quiz_data()[current_question(), ]
    
    correct_genus <- tolower(q$genus)
    correct_species <- tolower(q$species)
    correct_family <- tolower(q$family)
    
    user_genus <- tolower(trimws(input$genus_input))
    user_species <- tolower(trimws(input$species_input))
    user_family <- tolower(trimws(input$family_input))
    
    genus_correct <- user_genus == correct_genus
    species_correct <- user_species == correct_species
    family_correct <- user_family == correct_family
    
    points <- sum(genus_correct, species_correct, family_correct)
    score(score() + points)
    
    feedback <- paste(
      if (genus_correct) "Genus: ✅" else paste0("Genus: ❌ (Correct: ", q$genus, ")"),
      if (species_correct) "Species: ✅" else paste0("Species: ❌ (Correct: ", q$species, ")"),
      if (family_correct) "Family: ✅" else paste0("Family: ❌ (Correct: ", q$family, ")"),
      sep = " | "
    )
    
    showNotification(feedback, type = ifelse(points == 3, "message", "error"))
    
    # If answer is wrong, add it to the incorrect answers list
    if (!genus_correct | !species_correct | !family_correct) {
      incorrect_list(rbind(incorrect_list(), data.frame(Genus = q$genus, Species = q$species, Family = q$family)))
    }
    
    # Move to next question, unless it's the last one
    if (current_question() < nrow(quiz_data())) {
      current_question(current_question() + 1)
    } else {
      quiz_over(TRUE)
      showNotification(paste("Quiz Over! Final Score:", score(), "/", nrow(quiz_data()) * 3), type = "message")
    }
  })
  
  # Display Score
  output$score <- renderText({
    paste("Score:", score(), "/", nrow(quiz_data()) * 3)
  })
  
  # Display Incorrect Answers Table
  output$incorrect_answers <- renderTable({
    req(quiz_over())  # Ensure table appears only at the end
    incorrect_list()
  })
  
  output$quiz_over <- reactive({ quiz_over() })
  outputOptions(output, "quiz_over", suspendWhenHidden = FALSE)
}

# Run App
shinyApp(ui, server)
