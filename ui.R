library(rmarkdown)
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)

## If main variables don't exist, create them
if (exists("transcripts") == FALSE) {
  transcripts <<-
    tibble(ID = character(),
      participant = character(),
      HTMLfile = character())
  #transcripts <<- add_row(transcripts, ID = "sample", participant = "Sample Participant", HTMLfile = "sample.html")
}

if (exists("coded_text") == FALSE) {
  coded_text <<-
    tibble(
      participant = character(),
      textid = character(),
      code = character(),
      text = character()
    )
}

if (exists("codes") == FALSE) {
  codes <<- list("Choice 1" = 1,
    "Choice 2" = 2,
    "Choice 3" = 3)
}

if (exists("participants") == FALSE) {
  participants <<- character(0)
}

active_transcript_text <<- character(0)

## Load Transcript - Set variables and tibbles
currentParticipant <<-
  filter(transcripts, ID == currentID)$participant
transcriptHTML <<- filter(transcripts, ID == currentID)$HTMLfile

## JS function to close window
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

## JS function for pulling highlighted text
highlight <- '
function getSelectionText() {
var text = "";
if (window.getSelection) {
text = window.getSelection().toString();
} else if (document.selection) {
text = document.selection.createRange().text;
}
return text;
}

document.onmouseup = document.onkeyup = document.onselectionchange = function() {
var selection = getSelectionText();
Shiny.onInputChange("mydata", selection);
};
'

ui <- bootstrapPage(
  theme = shinytheme("lumen"),
  
  # HTML styles - prevent selection in codes column, pad top body for navbar
  tags$head(tags$style(
    HTML(
      "
      .no-select {
      -webkit-user-select: none;
      -khtml-user-drag: none;
      -khtml-user-select: none;
      -moz-user-select: none;
      -moz-user-select: -moz-none;
      -ms-user-select: none;
      user-select: none;
      }
      body {padding-top: 70px;}
      "
    )
    )),
  # Enables Close Window function
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  
  includeScript("www/codetext.js"),
  navbarPage(
    "Shinycoder",
    id = "tabs",
    tabPanel(
      "Participants",
      column(
        4,
        textInput("writeParticipant", label = "Participant Name", value = ""),
        actionButton("addParticipant", "Add Participant")
      ),
      column(4,
        htmlOutput("participantAdded"))
    ),
    tabPanel(
      "Transcripts",
      column(
        4,
        fileInput(
          "importedFile",
          "Choose MD file",
          accept = c("text/plain", "text/markdown", "text/md", ".md", ".txt")
        ),
        radioButtons(
          "transcriptParticipant",
          label = h3("Participants"),
          choices = participants,
          selected = 1
        )
        #"Identify Participant"
        # Server: Send upload to active transcript
      ),
      column(4,
        tableOutput("ListTranscripts"))
    ),
    tabPanel("Codes",
      column(4,
        tags$h1("Add Code")
        # Code Name
        # Code Abbreviation
        # Code Parent
        # Code Color
        # Add code button (with server behavior)
      ),
      column(4,
        tags$h1("Codes")
        # List of Codes, hierarchical, colored appropriately
      )),
      tabPanel(
        "Code Transcript",
        fixedPanel(
          tags$div(
            class = "no-select",
            tags$h1("Coding"),
            actionButton("codetext", "Apply Code"),
            radioButtons(
              "code_radio",
              label = h3("Codes"),
              choices = codes,
              selected = 1
            ),
            tableOutput("coded_text") # Meaningless output to get the events to trigger. Should try to remove at some point.
          ),
          left = 20
        ),
        column(
          width = 10,
          offset = 2,
          selectInput("currentID", "Pick transcript", choices = c(transcripts$ID)),
          tags$script(highlight),
            #fluidRow(
            #  column(8,
            uiOutput("active_transcript_text")
            #  )
        )),
      tabPanel("Review Codes",
          column(
            width = 10,
            offset = 2,
            tags$div(
              class = "no-select",
              tags$h1("Data Table"),
              tableOutput("coded_text_table"),
              downloadButton("exportcsv", "Export CSV")
            )
        )),
        tabPanel("Quit App", "Quit App", value = "quitapp")
        # Navbar display options
        , position = "fixed-top"
      )
    )