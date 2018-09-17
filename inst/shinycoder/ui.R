library(dplyr)
library(fs)
library(jsonlite)
library(rmarkdown)
library(shiny)
library(shinyFiles)
library(shinyjs)
library(shinythemes)
library(tidyverse)

# Install ShinyTree from source

if (!is.element("shinyTree", installed.packages()[,1]))
{
  library(devtools)
  install_github("shinyTree/shinyTree")
}
library(shinyTree)

## If no save directory, set a default
if (exists("savedir") == FALSE) {
  savedir <<- paste(Sys.getenv("HOME"), .Platform$file.sep, sep="")
}

## If main variables don't exist, create them
if (exists("transcripts") == FALSE) {
  transcripts <<-
    tibble(
      id = character(),
      participant = character(),
      html_file = character()
    )
}

if (exists("codes") == FALSE) {
  codes <<- list()
  codeClasses <<- list()
  # Create a dummy code so the tree doesn't fail (can we fix this eventually?)
  codes[["metadata"]] <<- structure(0, stclass = "none")
  codeClasses[["metadata"]] <<- "none"
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

if (exists("participants") == FALSE) {
  participants <<- character(0)
}

active_transcript_text <<- character(0)

#####################
# Read Code Classes #
#####################

CSSfile <- readLines("codeClass.css")
classes <- subset(CSSfile, grepl("\\..*", CSSfile) == TRUE)
classes <- gsub(" \\{", "", classes)
classes <- as.list(gsub("\\.", "", classes))


## Load Transcript - Set variables and tibbles
current_participant <<-
  filter(transcripts, id == current_id)$participant
transcript_html <<- filter(transcripts, id == current_id)$html_file

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

  includeScript("codetext.js"),
  navbarPage(
    "Shinycoder",
    id = "tabs",
    tabPanel(
      "New Text",
      column(
        4,
        tags$h3("Participants"),
        textInput("writeParticipant", label = "Participant Name", value = ""),
        actionButton("addParticipant", "Add Participant"),
        htmlOutput("participantAdded"),
        tags$br(), tags$br(), tags$br(),
        tags$h3("Transcripts"),
        fileInput(
          "imported_file",
          "Choose MD file",
          accept = c("text/plain", "text/markdown", "text/md", ".md", ".txt")
        ),
        radioButtons(
          "transcript_participant", "",
          choices = participants,
          selected = 1
        )
        
      ),
      column(
        8,
        shinyDirButton('SaveDir', label='Save Directory', title='Please select a directory to save files'),
        tableOutput("ListTranscripts"),
        "Save Directory: ",
        verbatimTextOutput("directorypath")
      )
    ),
    tabPanel(
      "Code Transcript",
      sidebarLayout(
        sidebarPanel(
          style = "position: fixed; right=20px;",
          tags$div(
            class = "no-select",

            ####################
            # Add Code Section #
            ####################
            tags$h3("Add Code"),
            textInput("addCodeID", "Code ID", value = ""),
            selectInput("addCodeClass", label = "Code Color", choices = classes, selected = 1),
            actionButton("addCode", "Add Code"),
            actionButton("refreshCode", "Refresh Code"),

            #############
            # Code List #
            #############

            tags$h3("Codes"),
            actionButton("applyCode", "Apply Code"),
            tags$br(), tags$br(),
            includeCSS("codeClass.css"),
            shinyTree("code_list", theme = "proton", dragAndDrop = TRUE),

            ########################
            # Manage Codes Section #
            ########################

            tags$h3("Manage Codes"),
            actionButton("deleteCode", "Delete Code")
          ), tableOutput("coded_text"), # Meaningless output to get the events to trigger. Should try to remove at some point.
          width = 2
        ),

        ###########################
        # Transcript Text Display #
        ###########################
        mainPanel(
          selectInput("current_id", "Pick transcript", choices = c(transcripts$id)),
          tags$script(highlight),
          includeCSS("codeClass.css"),
          uiOutput("active_transcript_text"),
          width = 9
        ),
        position = "right", fluid = FALSE
      )
    ),

    ####################
    # Review Codes Tab #
    ####################

    tabPanel(
      "Review Codes",
      column(
        width = 10,
        tags$div(
          class = "no-select",
          tags$h1("Data Table"),
          dataTableOutput("coded_text_table"),
          downloadButton("exportcsv", "Export CSV")
        )
      )
    ),
    tabPanel("Quit App", "Quit App", value = "quitapp")
    # Navbar display options
    , position = "fixed-top"
  )
)