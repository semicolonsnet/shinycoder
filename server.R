server <- function(input, output, session) {
  
  ######################
  # Quit App Functions #
  ######################
  
  quitApp <- function(){
    suppressWarnings(rm(active_transcript_text,
      currentParticipant,
      transcriptHTML,
      envir = .GlobalEnv))
    stopApp("Remember to save your variables!")
  }
  
  session$onSessionEnded(quitApp)
  
  observe({
    if (input$tabs == "quitapp") {
      js$closeWindow()
      quitApp()
    }
  })
  
  ##########################
  #  Switch Participant ID #
  ##########################
  
  currentIDInput <- reactive({
    switch(input$currentID, currentID <<- input$currentID)
  })
  
  ######################
  #  Switch Transcript #
  ###################### 
  
  switchActiveTranscript <- reactive({
    currentID <<- currentIDInput()
    currentParticipant <<-
      filter(transcripts, ID == currentID)$participant
    transcriptHTML <<- filter(transcripts, ID == currentID)$HTMLfile
    currentID
  })
  
  ##############################
  #  Render Current Transcript #
  ##############################
  
  output$active_transcript_text <- renderUI({
    currentID <<- switchActiveTranscript()
    includeHTML(transcriptHTML)
  })
  
  ######################
  # Add a Code to Text #
  ######################
  
  AddCodetoText <- eventReactive(input$codetext, {
    # Find current selected code
    selCode <- unlist(get_selected(codes))
   
    # Add new coded line to running tibble
    coded_text <<-
      add_row(
        coded_text,
        participant = currentParticipant,
        textid = currentID,
        code = selCode,
        text = input$mydata
      )
    
    classID <- "red"
    
    # Send class to JS instance
    session$sendCustomMessage("codeClass", classID)
    
    # Use JS to code text
    session$sendCustomMessage("codeText", selCode)
    
  })
  
  ####################################################################
  # Watch Javascript for new codes and update transcript accordingly #
  ####################################################################
  
  observe({
    # Access transcript
    input$active_new_transcript
    
    # Save full transcript to variable
    
    if (is.null(input$active_new_transcript) == FALSE)
      active_transcript_text <<- input$active_new_transcript
    
    # Write variable to HTML
    
    if (is.null(input$active_new_transcript) == FALSE)
      write(active_transcript_text, transcriptHTML)
  })
  
  ##########################
  # Add Participant Action #
  ##########################
  
  participantAdded <- eventReactive(input$addParticipant, {
    # Add new value, while removing dummy value if exists
    participants <<-
      c(participants[which(participants != "No participant added")] , input$writeParticipant)
    # Update radios in transcript tab
    updateRadioButtons(session, "transcriptParticipant", choices = participants)
    # Output list with line breaks
    HTML(paste(participants, '<br/>'))
  })
  
  output$participantAdded <- renderUI({
    participantAdded()
  })
  
  ################################################################
  # After Transcript is Added, Writes to File and Refreshes List #
  ################################################################
  
  output$ListTranscripts <- renderTable({
    # Checks for file, writes ID
    req(input$importedFile)
    writeID <- tools::file_path_sans_ext(input$importedFile$name)
    # Only proceed if this transcript (writeID) isn't already in the table
    if (nrow(filter(transcripts, ID == writeID)) == 0) {
      # Writes Markdown to HTML file
      write(
        markdown::markdownToHTML(input$importedFile$datapath, stylesheet = 'md-style.md'),
        paste(writeID, ".html", sep = "")
      )
      # Adds to transcripts tibble
      transcripts <<-
        add_row(
          transcripts,
          ID = writeID,
          participant = input$transcriptParticipant,
          HTMLfile = paste(writeID, ".html", sep = "")
        )
      # Update dropdown on Code Transcripts tab
      updateSelectInput(session, "currentID",  choices = c(transcripts$ID))
    }
    # Return new transcripts table
    transcripts
  })
  
  ######################
  # Display Coded Text #
  ######################
  
  getData <- reactive({
    as.tibble(coded_text)
  })
  
  output$coded_text <- renderTable({
    AddCodetoText()
  })
  
  output$coded_text_table <- renderTable(coded_text)
  
  ########################
  # Code List Management #
  ########################
  
  
  # Watches for codes variable to determine whether to show an empty tree
  
  output$codeList <- renderTree({
    codes
  })
  
  
  output$emptyTree <- renderEmptyTree()
  
  observeEvent(input$codeList, { 
    codes <<- input$codeList
  })
  
  observeEvent(input$refreshCode, {
    # Update Tree Display
    updateTree(session, "codeList", codes)
  })
  
  observeEvent(input$addCode, {
    # Initialize new code
    codes[[input$addCodeID]] <<- structure(0,  stclass="red")

    # Update Tree Display
    updateTree(session, "codeList", codes)
  })
  
  observeEvent(input$deleteCode, {
    # Helper delete functions
    # recursive function to remove name from all levels of list
    stripname <- function(x, name) {
      thisdepth <- depth(x)
      if (thisdepth == 0) {
        return(x)
      } else if (length(nameIndex <- which(names(x) == name))) {
        x <- x[-nameIndex]
      }
      return(lapply(x, stripname, name))
    }
    
    # function to find depth of a list element
    # see http://stackoverflow.com/questions/13432863/determine-level-of-nesting-in-r
    depth <- function(this, thisdepth=0){
      if (!is.list(this)) {
        return(thisdepth)
      } else{
        return(max(unlist(lapply(this,depth,thisdepth=thisdepth+1))))    
      }
    }
    
    # Find current selected code
    selCode <- unlist(get_selected(codes, format = "classid"))
    codes <<- stripname(codes, selCode)
    
    # Update Tree Display
    updateTree(session, "codeList", codes)
    
    # Remove Code from HTML
    session$sendCustomMessage("removeCode", selCode)
    
    # Write new HTML to file
    if (is.null(input$active_new_transcript) == FALSE)
      write(active_transcript_text, transcriptHTML)
    
  })
  
  ##################################
  # Export Coded Text Table to CSV #
  ##################################
  
  output$exportcsv <- downloadHandler(
    filename = function() {
      paste("coded-text", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(getData(), file, row.names = FALSE)
    }
  )
}