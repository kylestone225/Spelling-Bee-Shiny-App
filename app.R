library(tidyverse)
library(stringr)
library(stringi)
library(shiny)
library(shinycssloaders)


all_words <- as.data.frame(read.csv("words (2).csv"))

all_words <- all_words |>
  rename(words = "X2")

all_four <- all_words |>
  filter(str_length(words) >= 4)

all_four <- all_four |>
  filter(grepl("[[:punct:]]", all_four$words) != "TRUE" &
           grepl("[0-9]", all_four$words) != "TRUE")

all_four$words <- tolower(all_four$words)

bee_letters <- letters[c(-1, -5, -9, -15, -17, -21, -24, -26)]

bee_vowels <- c("a", "e", "i", "o", "u")


ui <- fluidPage(
  
  titlePanel("Welcome To The Spelling Bee!"),
  tags$b("Here Are Today's Letters:"),
  tags$b(textOutput("bee")),
  textOutput("letter"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      textInput("guess", "What's your guess?"),
      
      actionButton("submit", "SUBMIT"),
      
      textOutput("answer")
    ),
    mainPanel(
      
      withSpinner(
        textOutput("total")
      ),
      
      textOutput("score"),
      
      plotOutput("plot", height = "100px"),
      
      tags$b(textOutput("rank")),
    ),
  ),
  tableOutput("guesses")
)



server <- function(input, output, session) {
  
  b_letters <- c(sample(bee_letters, 4, replace = FALSE), sample(bee_vowels, 3, replace = FALSE))
  
  
  output$choose <- renderText({
    "Here are the game letters:"
  })
  
  output$bee <- renderText(
    b_letters
  )
  
  pattern <- paste0("^[",paste(b_letters, collapse = ""),"]+$")         
  
  total_words <- all_four |>
    filter(str_detect(words, b_letters[7]) &
            str_detect(words, pattern))

  
  total_points <- sum(str_length(total_words$words))
  
  output$total <- renderText({paste0("There Are ", nrow(total_words),
                                     " Total Words In Today's Puzzle, and There Are ",
                                     total_points, " Total Points") 
  })
  
  output$letter <- renderText({
    paste0("You Must Use The ", toupper(b_letters[7]))
  })
  
  button <- eventReactive(input$submit, {
    tolower(input$guess)
  })
  
  observeEvent(input$submit, {
    updateTextInput(session, "guess", value = "")
  })
  
  values <- reactiveValues()
  
  values$df <- data.frame(Guesses = numeric(0))
  
  correct <- eventReactive(input$submit, {
    ifelse(str_detect(tolower(input$guess), b_letters[7]) == "TRUE" & 
             all(str_split_1(tolower(input$guess), "") %in% b_letters) == "TRUE" &
             any(all_four$words == tolower(input$guess)) == "TRUE" &
             any(values$df$Guesses == tolower(input$guess)) == "FALSE",
           str_length(input$guess), 0)
  })
  
  newEntry <- observe({
    if(input$submit > 0 & correct() > 0) {
      newline <- isolate(data.frame(Guesses = button(),
                                    Score = correct()))
      isolate(values$df <- rbind(values$df, newline))
    }
  })
  
  score <- reactiveValues()  
  
  score$db <- data.frame(Total_Score = numeric(0))
  
  totalScore <- observe({
    if(input$submit > 0) {
      scoreline <- isolate(data.frame(Total_Score = correct()))
      isolate(score$db <- rbind(score$db, scoreline))
    }
  })
  
  output$answer <- renderText({
    if(str_length(button()) < 4){
      "Too Short"
    }else{
      if(correct() >= 4){
        paste0("Congrats! You Earned ", str_length(button()), " Points!")
        }else{
          "Sorry Charlie"
      }
    }
  })
  
  output$rank <- renderText({
    if(sum(score$db) > 0 & sum(score$db) < total_points/6){
      "Good Start!"
    }else{
      if(sum(score$db) > total_points/6 & sum(score$db) < total_points*.333){
      "Going Strong!"
      }else{
        if(sum(score$db) > total_points*.333 & sum(score$db) < total_points*.5){
        "GREAT!"
        }else{
          if(sum(score$db) > total_points*.5 & sum(score$db) < total_points*.667){
          "EXCELLENT!!" 
          }else{
            if(sum(score$db) > total_points*.667 & sum(score$db) < total_points*.833){
            "INCREDIBLE!!!" 
            }else{
              if(sum(score$db) > total_points*.833 & sum(score$db) < total_points*.999){
              "GENIUS!!! Congratulations You Have Won!"
              }else{
                if(sum(score$db) == total_points){
                "HOLY SHIT A QUEEN BEE?!?!?! I CAN'T FUCKING BELIEVE IT!"
                }else{
                "Good Luck!"
              }
            }
          }
        }
      }
    }
    }
  })
  
  
  
  output$guesses <- renderTable({
    values$df 
  })
  
  output$score <- renderText({
    paste0("You have scored ", sum(score$db), " points, and you have found ", nrow(values$df), " words.")
  })
  
  output$plot <- renderPlot(
    ggplot() +
      geom_col(aes(y = sum(score$db), x = button(), fill = "yellow", color = "black", width = .3), show.legend = FALSE) +
      coord_flip() +
      ylab("Your Score") +  xlab("") +
      scale_y_continuous(limits = c(0,total_points)) +
      scale_fill_identity(guide = "legend") +
      scale_color_identity(guide = "legend") +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) + 
      theme_classic()
  )
}

shinyApp(ui, server)

