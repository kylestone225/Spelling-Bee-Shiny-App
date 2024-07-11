library(tidyverse)
library(stringr)
library(stringi)
library(shiny)


library(tidyverse)
library(stringr)
library(stringi)
library(shiny)

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
      
      textOutput("total"),
      
      textOutput("score"),
      
      plotOutput("plot", height = "100px"),
      
      tags$b(textOutput("rank"))
      
    ),
  ),
  column(width = 3,
         tableOutput("guesses")
  ),
  
  column(width = 3,
         actionButton("cheat", "SHOW ANSWERS")
  ),
  
  column(width = 6,
         tableOutput("allwords")
  )
  
)



server <- function(input, output, session) {
  
  all_four <- as.data.frame(read.csv("words (2).csv")) |>
    rename(words = "X2") |>
    mutate(words = tolower(words)) |>
    filter(grepl("[[:punct:]]", words) != TRUE &
             grepl("[0-9]", words) != TRUE &
             nchar(words) > 3)
  
  bee_letters <- letters[c(-1, -5, -9, -15, -17, -21, -24, -26)]
  
  bee_vowels <- c("a", "e", "i", "o", "u")
  
  b_letters <- c(sample(bee_letters, 4, replace = FALSE), sample(bee_vowels, 3, replace = FALSE))
  
  output$bee <- renderText(
    b_letters
  )
  
  pattern <- paste0("^[",paste(b_letters, collapse = ""),"]+$")         
  
  total_fours <- all_four |>
    filter(nchar(words) == 4 &
             str_detect(words, b_letters[7]) &
             str_detect(words, pattern))
  
  total_others <- all_four |>
    filter(nchar(words) > 4 &
             str_detect(words, b_letters[7]) &
             str_detect(words, pattern))
  
  total_words <- all_four |>
    filter(nchar(words) > 3 &
             str_detect(words, b_letters[7]) &
             str_detect(words, pattern))
  
  
  total_points <- sum(str_length(total_others$words)) + nrow(total_fours)
  
  output$total <- renderText({paste0("There are ", nrow(total_words),
                                     " total words in today's puzzle. There are ",
                                     total_points, " total points") 
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
    if(str_detect(button(), b_letters[7]) == TRUE & 
       all(str_split_1(button(), "") %in% b_letters) == TRUE &
       any(all_four$words == button()) == TRUE &
       any(values$df$Guesses == button()) == FALSE &
       str_length(button()) == 4){
      1
    }else{
      if(str_detect(button(), b_letters[7]) == TRUE & 
         all(str_split_1(button(), "") %in% b_letters) == TRUE &
         any(all_four$words == button()) == TRUE &
         any(values$df$Guesses == button()) == FALSE &
         str_length(button()) > 4){
        str_length(button())
      }else{
        if(str_detect(button(), b_letters[7]) == TRUE & 
           all(str_split_1(button(), "") %in% b_letters) == TRUE &
           any(all_four$words == button()) == TRUE &
           any(values$df$Guesses == button()) == FALSE &
           all(b_letters %in% str_split_1(button(), "")) == TRUE){
          str_length(button())*2
        }else{
          0
        }
      }
    }
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
      if(correct() == 1){
        paste0("Congrats! You Earned 1 Point!")
      }else{
        if(correct() > 4){
          paste0("Congrats! You Earned ", str_length(button()), " Points!")
        }else{
          if(correct() > 0 & all(b_letters %in% str_split_1(input$guess, "")) == TRUE){
            "PANGRAM!!!!"
          }else{
            "Sorry Charlie"
          }
        }
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
                  "HOLY $#@& A QUEEN BEE?!?!?! I CAN'T BELIEVE IT! THE GAME IS NOT WORTHY!!"
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
  
  observeEvent(input$cheat, {
    output$allwords <-  renderTable(total_words)
  })
}

shinyApp(ui, server)
