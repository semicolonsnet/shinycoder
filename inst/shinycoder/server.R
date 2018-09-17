server <- function(input, output, session) {

  ######################
  # Quit App Functions #
  ######################

  quitApp <- function() {
    suppressWarnings(rm(active_transcript_text,
      current_participant,
      transcript_html,
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

  ######################
  # Set Save Directory #
  ######################

  volumes <- c(Home = fs::path_home())
  
  shinyDirChoose(input, "SaveDir", roots = volumes, session = session, restrictions = system.file(package = "base"))

  output$directorypath <- renderPrint({
    
  # If new save dir, write to variable
  if (is.null(input$SaveDir) == FALSE) {
    if (identical(parseDirPath(volumes, input$SaveDir),character(0)) == FALSE) {
     savedir <<- str_replace(parseDirPath(volumes, input$SaveDir), fs::path_home(), "~")
    }
  }
    path_expand(savedir)
  })
  
  #########################
  # Switch Participant id #
  #########################

  current_idInput <- reactive({
    switch(input$current_id, current_id <<- input$current_id)
  })

  ######################
  #  Switch Transcript #
  ######################

  switchActiveTranscript <- reactive({
    current_id <<- current_idInput()
    current_participant <<-
      dplyr::filter(transcripts, id == current_id)$participant
    transcript_html <<- dplyr::filter(transcripts, id == current_id)$html_file
    current_id
  })

  ##############################
  #  Render Current Transcript #
  ##############################

  output$active_transcript_text <- renderUI({
    current_id <<- switchActiveTranscript()
    includeHTML(paste(path_expand(savedir),.Platform$file.sep,transcript_html, sep = ""))
  })

  ######################
  # Add a Code to Text #
  ######################

  applyCode <- eventReactive(input$applyCode, {
    # Find current selected code
    sel_code <- unlist(get_selected(codes))

    # Add new coded line to running tibble
    coded_text <<-
      add_row(
        coded_text,
        participant = current_participant,
        textid = current_id,
        code = sel_code,
        text = input$mydata
      )

    # Update coded text table
    output$coded_text_table <- renderDataTable(coded_text)

    classid <- codeClasses[[sel_code]]

    # Send class to JS instance
    session$sendCustomMessage("codeClass", classid)

    # Use JS to code text
    session$sendCustomMessage("codeText", sel_code)
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
      write(active_transcript_text, paste(path_expand(savedir),.Platform$file.sep,transcript_html, sep = ""))
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
    updateRadioButtons(session, "transcript_participant", choices = participants)
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
    if (is.null(input$imported_file)) {
      transcripts
      # If a file is being added, add it to the table then update the display
    } else {
      writeID <- tools::file_path_sans_ext(input$imported_file$name)
      # Only proceed if this transcript (writeID) isn't already in the table
      if (nrow(dplyr::filter(transcripts, id == writeID)) == 0) {
        # Writes Markdown to HTML file
        write(
          markdown::markdownToHTML(input$imported_file$datapath, stylesheet = "md-style.md"),
          paste(path_expand(savedir),.Platform$file.sep,writeID, ".html", sep = "")
        )
        # Adds to transcripts tibble
        transcripts <<-
          add_row(
            transcripts,
            id = writeID,
            participant = input$transcript_participant,
            html_file = paste(writeID, ".html", sep = "") # Just save file relative to directory to increase compatibility
          )
        # Update dropdown on Code Transcripts tab
        updateSelectInput(session, "current_id", choices = c(transcripts$id))
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

  output$code_list <- renderTree({
    codes
  })

  # Allows user to interact with selected code
  observeEvent(input$code_list, {
    codes <<- input$code_list
  })

  observeEvent(input$refreshCode, {
    # Update Tree Display
    updateTree(session, "code_list", codes)
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
    updateTree(session, "code_list", codes)
    
    # Empty Add Code Box
    updateTextInput(session, "addCodeID", value="")
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
    sel_code <- unlist(get_selected(codes, format = "classid"))

    # Remove coded segments from coded_text table
    coded_text <<- dplyr::filter(coded_text, code != sel_code)

    output$coded_text_table <- renderDataTable(coded_text)

    # Remove code from codes hierarchy using helper functions
    codes <<- stripname(codes, sel_code)

    # Update Tree Display
    updateTree(session, "code_list", codes)

    # Remove Code from HTML
    session$sendCustomMessage("removeCode", sel_code)

    # Write new HTML to file
    if (is.null(input$active_new_transcript) == FALSE) {
      write(active_transcript_text, paste(path_expand(savedir),.Platform$file.sep,transcript_html, sep = ""))
    }
  })

  ##################################
  # Export Coded Text Table to CSV #
  ##################################

  output$exportcsv <- downloadHandler(
    filename = function() {
      paste("coded_text", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(getData(), file, row.names = FALSE)
    }
  )
}
