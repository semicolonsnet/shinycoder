server <- function(input, output, session) {
  
  # Checks tab name, performs quit app function
  observe({
    if(input$tabs == "quitapp"){
      rm(coded_excerpt, active_transcript_text, currentID, currentParticipant, transcriptHTML, envir = .GlobalEnv)
      js$closeWindow()
      stopApp("Remember to save your variables!")
    }
  })
  
  currentIDInput <- reactive({
    switch(input$currentID, currentID <<- input$currentID)
  })
  
  switchActiveTranscript <- reactive({
    currentID <<- currentIDInput()
    currentParticipant <<- filter(transcripts, ID==currentID)$participant
    transcriptHTML <<- filter(transcripts, ID==currentID)$HTMLfile
    currentID
  })
  
  output$active_transcript <- renderUI({
    currentID <<- switchActiveTranscript()
    includeHTML(transcriptHTML)
  })
  
  AddCodetoText <- eventReactive(input$codetext, {
    # Add new coded line to running tibble
    coded_text <<- add_row(coded_text, participant = currentParticipant, textid = currentID, code = input$code_radio, text = input$mydata)
    
    # Determine code's text color
    
    # Use JS to code text
    session$sendCustomMessage("codetext", "")
    
  })
  
  ## Watch Javascript for new codes and update transcript accordingly
  
  observe({
    # thats how you access the variable
    input$active_new_transcript
    # Save full transcript to variable
    if (is.null(input$active_new_transcript) == FALSE) active_transcript_text <<- input$active_new_transcript
    # Write variable to HTML
    if (is.null(input$active_new_transcript) == FALSE) write(active_transcript_text, transcriptHTML)
    
  })
  
  participantAdded <- eventReactive(input$addParticipant, {
    # Add new value, while removing dummy value if exists 
    participants <<- c(participants[which(participants!="No participant added")] , input$writeParticipant)
    # Update radios in transcript tab
    updateRadioButtons(session, "transcriptParticipant", choices=participants)
    # Output list with line breaks
    HTML(paste(participants, '<br/>'))
    })
  
  output$participantAdded <- renderUI({
    participantAdded()
  })
  
  output$ListTranscripts <- renderTable({
    # Checks for file, writes ID
    req(input$importedFile)
    
    writeID <- tools::file_path_sans_ext(input$importedFile$name)
    
    # Only proceed if this transcript (writeID) isn't already in the table
    if (nrow(filter(transcripts, ID==writeID)) == 0){ 
      # Writes Markdown to HTML file
      write(markdown::markdownToHTML(input$importedFile$datapath, stylesheet = 'md-style.md'), paste(writeID, ".html", sep=""))
      # Adds to transcripts tibble
      transcripts <<- add_row(transcripts, ID = writeID, participant = input$transcriptParticipant, HTMLfile=paste(writeID, ".html", sep=""))
      # Update dropdown on Code Transcripts tab
      updateSelectInput(session, "currentID",  choices=c(transcripts$ID))
    }
    # Return new transcripts table
    transcripts
    
  })
  
  output$exportcsv <- downloadHandler(
    filename = "coded-text.csv",
    content = function(file) {
      write.csv(coded_text, file, row.names = FALSE)
    }
  )
  
  output$coded_text <- renderTable({ AddCodetoText() })
  
  output$coded_text_table <- renderTable(coded_text)
  
}