// the event handler listens to shiny for messages send by codetext
Shiny.addCustomMessageHandler("codetext", codeText);

// this function is called by the handler, which passes the message
function codeText(message){
    
    // Get Selection
    sel = window.getSelection();
    if (sel.rangeCount && sel.getRangeAt) {
      range = sel.getRangeAt(0);
    }
    
    // Set design mode to on
    document.designMode = "on";
    if (range) {
      sel.removeAllRanges();
      sel.addRange(range);
    }
    
    // Colorize text
    document.execCommand("ForeColor", false, "red");
    
    // Insert code boundry
    
    document.execCommand("insertText", false, "[code1]" + sel + "[/code1]");
   
    // Set design mode to off
    document.design;
    
    // Get full version of new transcript
    var NewTranscriptID = document.getElementById('active_transcript_text');
    NewTranscript = NewTranscriptID.innerHTML;
    
    // Return transcript to Shiny
    Shiny.onInputChange("active_new_transcript", NewTranscript);
}
