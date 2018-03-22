// the event handler listens to shiny for messages send by codetext
Shiny.addCustomMessageHandler("codetext", codeText);
Shiny.addCustomMessageHandler("removeCode", removeCode);

// this function is called by the handler, which passes the message
function codeText(selCode){
    
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
    document.execCommand("insertText", false, "{"+ selCode + "}"+ sel + "{/" + selCode + "}");
   
    // Set design mode to off
    document.designMode = "off";
    
    // Get full version of new transcript
    var NewTranscriptID = document.getElementById('active_transcript_text');
    NewTranscript = NewTranscriptID.innerHTML;
    
    // Return transcript to Shiny
    Shiny.onInputChange("active_new_transcript", NewTranscript);
}


function removeCode(selCode){
  // '<font color="#ff0000">' +
  
  var begreplace = '<font color="#ff0000">' + '{' + selCode + '}';
  var begreg = new RegExp(begreplace, "g");
  
  var endreplace = '{/'+ selCode + '}' + '</font>';
  var endreg = new RegExp(endreplace, "g");
    
    // Replace tags in active_transcript_text 
    
  document.getElementById('active_transcript_text').innerHTML = document.getElementById('active_transcript_text').innerHTML.replace(begreg, '');
    
  document.getElementById('active_transcript_text').innerHTML = document.getElementById('active_transcript_text').innerHTML.replace(endreg, '');
    
    // Get full version of new transcript
    var NewTranscriptID = document.getElementById('active_transcript_text');
    NewTranscript = NewTranscriptID.innerHTML;
    
    // Return transcript to Shiny
    Shiny.onInputChange("active_new_transcript", NewTranscript);
  
}
