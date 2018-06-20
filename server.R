server <- function(input, output, session) {

  ######################
  # Quit App Functions #
  ######################

  quitApp <- function() {
    suppressWarnings(rm(active_transcript_text,
      currentParticipant,
      transcriptHTML,
      envir = .GlobalEnv
    ))
    stopApp("Remember to save your variables!")
  }

  session$onSessionEnded(quitApp)

  observe({
    if (input$tabs == "quitapp") {
      js$closeWindow()
      quitApp()
    }
  })

  #########################
  # Switch Participant ID #
  #########################

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

  applyCode <- eventReactive(input$applyCode, {
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

    # Update coded text table
    output$coded_text_table <- renderDataTable(coded_text)

    classID <- codeClasses[[selCode]]

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
    if (is.null(input$active_new_transcript) == FALSE) {
      active_transcript_text <<- input$active_new_transcript
    }

    # Write variable to HTML
    if (is.null(input$active_new_transcript) == FALSE) {
      write(active_transcript_text, transcriptHTML)
    }
  })

  ##########################
  # Add Participant Action #
  ##########################

  participantAdded <- eventReactive(input$addParticipant, {
    # Add new value, while removing dummy value if exists
    participants <<-
      c(participants[which(participants != "No participant added")], input$writeParticipant)
    # Update radios in transcript tab
    updateRadioButtons(session, "transcriptParticipant", choices = participants)
    # Return empty line
    HTML("")
  })

  output$participantAdded <- renderUI({
    participantAdded()
  })

  ################################################################
  # After Transcript is Added, Writes to File and Refreshes List #
  ################################################################

  output$ListTranscripts <- renderTable({
    # If no file is being added, just display the table
    if (is.null(input$importedFile)) {
      transcripts
      # If a file is being added, add it to the table then update the display
    } else {
      writeID <- tools::file_path_sans_ext(input$importedFile$name)
      # Only proceed if this transcript (writeID) isn't already in the table
      if (nrow(filter(transcripts, ID == writeID)) == 0) {
        # Writes Markdown to HTML file
        write(
          markdown::markdownToHTML(input$importedFile$datapath, stylesheet = "md-style.md"),
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
        updateSelectInput(session, "currentID", choices = c(transcripts$ID))
      }
      # Return new transcripts table
      transcripts
    }
  })

  ######################
  # Display Coded Text #
  ######################

  getData <- reactive({
    as.tibble(coded_text)
  })

  output$coded_text <- renderTable({
    applyCode()
  })

  output$coded_text_table <- renderDataTable(coded_text)

  ########################
  # Code List Management #
  ########################

  output$codeList <- renderTree({
    codes
  })

  # Allows user to interact with selected code
  observeEvent(input$codeList, {
    codes <<- input$codeList
  })

  observeEvent(input$refreshCode, {
    # Update Tree Display
    updateTree(session, "codeList", codes)
  })

  ############
  # Add Code #
  ############

  rerenderTree <- function(expr, env = parent.frame(), quoted = FALSE) {
    func <- shiny::exprToFunction(expr, env, quoted)
    return(function(shinysession, name, ...) {
      tree <- func()

      shiny::HTML(as.character(listToTags(tree)))
    })
  }

  observeEvent(input$addCode, {
    # Lowercase string to prevent weird deattr-ing errors
    addCodeID <- tolower(input$addCodeID)

    # Initialize new code
    addCodeClass <- input$addCodeClass
    codes[[addCodeID]] <<- structure("", stclass = addCodeClass)
    codeClasses[[addCodeID]] <<- addCodeClass

    # Update Tree Display
    updateTree(session, "codeList", codes)
  })

  ###############
  # Delete Code #
  ###############

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
    depth <- function(this, thisdepth=0) {
      if (!is.list(this)) {
        return(thisdepth)
      } else {
        return(max(unlist(lapply(this, depth, thisdepth = thisdepth + 1))))
      }
    }

    # Find current selected code
    selCode <- unlist(get_selected(codes, format = "classid"))

    # Remove coded segments from coded_text table
    coded_text <<- filter(coded_text, code != selCode)

    output$coded_text_table <- renderDataTable(coded_text)

    # Remove code from codes hierarchy using helper functions
    codes <<- stripname(codes, selCode)

    # Update Tree Display
    updateTree(session, "codeList", codes)

    # Remove Code from HTML
    session$sendCustomMessage("removeCode", selCode)

    # Write new HTML to file
    if (is.null(input$active_new_transcript) == FALSE) {
      write(active_transcript_text, transcriptHTML)
    }
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
