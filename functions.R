check_update_col <- function(session, input, word, word_id){
  change_letter_col(session, word[1,1], paste0("l1_w",word_id))
  change_letter_col(session, word[2,1], paste0("l2_w",word_id))
  change_letter_col(session, word[3,1], paste0("l3_w",word_id))
  change_letter_col(session, word[4,1], paste0("l4_w",word_id))
  change_letter_col(session, word[5,1], paste0("l5_w",word_id))
  updateButton(session, paste0("l1_w",word_id), label = toupper(str_sub(input$word1_guess, 1, 1)))
  updateButton(session, paste0("l2_w",word_id), label = toupper(str_sub(input$word1_guess, 2, 2)))
  updateButton(session, paste0("l3_w",word_id), label = toupper(str_sub(input$word1_guess, 3, 3)))
  updateButton(session, paste0("l4_w",word_id), label = toupper(str_sub(input$word1_guess, 4, 4)))
  updateButton(session, paste0("l5_w",word_id), label = toupper(str_sub(input$word1_guess, 5, 5)))
}


change_letter_col <- function(session,value,id){
  if(value>2){
    value <- value %% 3
  }
  
  if(value == 1){
    removeClass(class = "wrongLetter", selector = paste0("#",id))
    removeClass(class = "rightSpot", selector = paste0("#",id))
    addClass(class = "wrongSpot",selector = paste0("#",id))
  }
  else if(value == 2){
    removeClass(class = "wrongSpot", selector = paste0("#",id))
    removeClass(class = "wrongLetter", selector = paste0("#",id))
    addClass(class = "rightSpot",selector = paste0("#",id))
  }
  else {
    removeClass(class = "rightSpot", selector = paste0("#",id))
    removeClass(class = "wrongSpot", selector = paste0("#",id))
    addClass(class = "wrongLetter",selector = paste0("#",id))
  }
}


changelettercolour<-function(session,input,id){
  
  observeEvent(input[[id]],{
    
    value <- input [[id]]
    
    if(value>2){
      value <- value %% 3
    }
    
    if(value == 1){
      removeClass(class = "wrongLetter", selector = paste0("#",id))
      removeClass(class = "wrongSpot", selector = paste0("#",id))
      addClass(class = "wrongSpot",selector = paste0("#",id))
    }
    else if(value == 2){
      removeClass(class = "wrongSpot", selector = paste0("#",id))
      removeClass(class = "wrongLetter", selector = paste0("#",id))
      addClass(class = "rightSpot",selector = paste0("#",id))
    }else {
      removeClass(class = "rightSpot", selector = paste0("#",id))
      removeClass(class = "wrongSpot", selector = paste0("#",id))
      addClass(class = "wrongLetter",selector = paste0("#",id))
    }
    
  })
}


letter_colour_ui<- function(ui,word_id) {
  fluidRow(
    column(
      6, offset = 3,
      splitLayout(
        bsButton(inputId = paste0("l1_w",word_id), label = "", class = "tileButton"),
        bsButton(inputId = paste0("l2_w",word_id), label = "", class = "tileButton"),
        bsButton(inputId = paste0("l3_w",word_id), label = "", class = "tileButton"),
        bsButton(inputId = paste0("l4_w",word_id), label = "", class = "tileButton"),
        bsButton(inputId = paste0("l5_w",word_id), label = "", class = "tileButton")
      )
    )
  )
}



##Continue game functions----
word_checker <- function(session, target_val, guess_val) {
  output <- character(5)
  target_remaining <- target_val
  guess_remaining <- character(5)
  
  #target frequencies
  target_freq <- data.frame(target_val) %>% 
    group_by(target_val) %>% 
    summarize(target_freq = n()) %>% #count the frequency of each letter in the target
    ungroup()
  #frequencies
  freq <- data.frame(guess_val) %>% 
    mutate(position = 1:5) %>% #get position of letter in word
    group_by(guess_val) %>% 
    mutate(guess_freq = n()) %>% #count the frequency of each letter in the guess
    ungroup() %>% 
    left_join(target_freq, by = c("guess_val" = "target_val")) %>% #join frequencies of guess and target together
    mutate(target_freq = ifelse(is.na(target_freq) == TRUE, 0, target_freq)) # replace NA with 0 where target letter does not appear in guess
  #minimum position flag
  freq <- freq %>% group_by(guess_val) %>% 
    mutate(counter = row_number(guess_val)) %>% #get position of specific letter
    ungroup()
  
  
  for (i in 1:5){
    
    if(guess_val[i] == target_val[i]) {
      output[i] <- 2
      target_remaining[i] <- NA
      
    } else { #needs to only highlight 1 
      #guess
      guess_remaining[i] <-  guess_val[i]
      output[i] <- 0
    }
  }
  #Add counts to this for loop  
  for (i in 1:5){
    if((guess_val[i] != target_val[i]) & (guess_remaining[i] %in% target_remaining)){
      #check counts
      if(freq[i,3] <= freq[i,4]){
        #if target counts = guess counts then 
        output[i] <-1
        #if guess count != target count 
      }else if(freq[i,3] > freq[i,4] & freq[i,4] >= freq[i,5]){ #and in min position then 
        #only flag first case
        output[i] <- 1
        #not in
      }  }
  }
  output <- data.frame(output)
  return(output)
}