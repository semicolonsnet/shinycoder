new_shinycoder <- function(path, ...) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  fileConn<-file(paste(path, .Platform$file.sep, "run_shinycoder.R", sep=""))
  writeLines(c("library(shinycoder)","shinycoder()"), fileConn)
  close(fileConn)
  
}