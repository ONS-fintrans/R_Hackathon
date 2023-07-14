library(dplyr)
library(DT)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(stringr)
library(words)
source("C:/My_RStudio/Workspace/R_hackathon/functions.R")


#Setting words list
word_list <- words::words %>% 
  filter(word_length == 5) %>% 
  select(word)


#Developing user interface - header, sidebar, input box and also buttons
header <- dashboardHeader(title = "Wordle Shiny")

sidebar <- dashboardSidebar(disable = T)

body <- dashboardBody(
  actionButton(inputId = "help", label = "", icon = icon("circle-question"), class = "helpButton",
               style = "position: absolute; right: 40px"),
  
  useShinyjs(),
  div(
    class = "word-button",
    textInput(
      inputId = "word1_guess",
      label = div("Enter your guess:", style="font-size:120%"),
      value = "",
    ) %>%
      tagAppendAttributes(class = "inline-element"),
    actionButton(inputId = "go1", "Lock Guess", class = "lockButton")
  ),
  letter_colour_ui("word1",1), 
  br(),
  letter_colour_ui("word2",2),
  br(),
  letter_colour_ui("word3",3),
  br(),
  letter_colour_ui("word4",4),
  br(),
  letter_colour_ui("word5",5),
  br(),
  letter_colour_ui("word6",6)
)
#Custom .css for adding colours to boxes 
ui <- dashboardPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")), 
  skin = "black",
  header = header,
  sidebar = sidebar,
  body = body
)


# Setting up server / changing pieces
server <- function(input,output,session){
  
  #Close app function to trigger after javascript code (4s delay)
  observeEvent(input$closeApp,{
    if(input$closeApp){
      stopApp()
    }
  })
  
  #Help button
  observeEvent(input$help, {
    showModal(modalDialog(
      HTML('<img src="instructions.png" onmousedown="if (event.preventDefault) event.preventDefault()" />'),
      easyClose = FALSE,
      footer=tagList(
        modalButton(div('Back', style="font-size:120%"))
      )
    ))
  })
  
  #Setting counter to 0
  counter <- reactiveValues(countervalue = 0)
  previous_guesses <- reactiveValues()
  
  #Setting target words
  target <- sample(word_list$word,1)
  target_val <- strsplit(target, "")[[1]]
  
  
  #Hit the "Submit" button of a word guess, all the following happens:
  observeEvent(input$go1, {
    
     guess <- tolower(input$word1_guess)
     
     #Ensuring word is 5 letters long
    if(nchar(guess)!=5){
      showModal(
        modalDialog(
          title = div("Error", style="font-size:160%"),
          div("Guesses must be exactly 5 letters long.", style="font-size:120%"),
          easyClose = FALSE,
          footer=tagList(
            modalButton(div('Back', style="font-size:120%"))
          )
        )
      )
      updateTextInput(session,"word1_guess",value="")
      
      #Ensuring word is in word list
    } else if(!(guess %in% word_list$word)){
      showModal(
        modalDialog(
          title = div("Error", style="font-size:160%"),
          div("Guesses must be a real word.", style="font-size:120%"),
          easyClose = FALSE,
          footer=tagList(
            modalButton(div('Back', style="font-size:120%"))
          )
        )
      )
      updateTextInput(session,"word1_guess",value="")
      
      #Checking if word has been guessed previously
    } else {
        if(guess %in% previous_guesses$guess){
          showModal(
            modalDialog(
              title = div("Error", style="font-size:160%"),
              div("Make sure that guesses are not used multiple times.", style="font-size:120%"),
              easyClose = FALSE,
              footer=tagList(
                modalButton(div('Back', style="font-size:120%"))
              )
            )
          )
          updateTextInput(session,"word1_guess",value="")
        }
        else{
          previous_guesses$guess <- c(isolate(previous_guesses$guess), isolate(guess))
          
          guess_val <- strsplit(input$word1_guess, "")[[1]]
          
      
          word <- word_checker(session, target_val, guess_val)
          
      #Depending on counter value, updating the following boxes
          if (counter$countervalue==0){
            check_update_col(session, input, word, 1)
        
            counter$countervalue <- counter$countervalue+1
        
            guess_achieved(input$word1_guess,target)
         }
      
         else if (counter$countervalue==1){
            check_update_col(session, input, word, 2)
        
            counter$countervalue <- counter$countervalue+1
        
            guess_achieved(input$word1_guess,target)
         }
      
         else if (counter$countervalue==2){
           check_update_col(session, input, word, 3)
        
           counter$countervalue <- counter$countervalue+1
        
            guess_achieved(input$word1_guess,target)
          }
      
          else if (counter$countervalue==3){
           check_update_col(session, input, word, 4)
        
           counter$countervalue <- counter$countervalue+1
        
           guess_achieved(input$word1_guess,target)
          }
      
          else if (counter$countervalue==4){
           check_update_col(session, input, word, 5)
        
           counter$countervalue <- counter$countervalue+1
        
            guess_achieved(input$word1_guess,target)
         }
      
          else if (counter$countervalue==5){
            check_update_col(session, input, word, 6)
        
            counter$countervalue <- counter$countervalue+1

           guess_achieved(input$word1_guess,target)
        
         }}}
    
     #Max guess' reached
    if(counter$countervalue==6){
      max_guesses()
    }
    
    updateTextInput(session,"word1_guess",value="")
    
    
  })
  
  
}

#Run App
shinyApp(ui = ui, server = server)
