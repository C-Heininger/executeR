#' Execute function for system calls
#' 
#' This function takes a constructed command that is to be run on the command line 
#' and executes that command if a specified output file does not already exist.
#' @param x Constructed command
#' @param outputfile Name for output file of command. Default = NA
#' @param intern To set the intern option of the internal system function. Defaults to FALSE
#' @param quitonError Decide to close R if an error occurs. Defaults to FALSE
#' @keywords command execute system
#' @export
#' @examples 
#' execute(constructed.command, output.file)

execute <- function(x, outputfile = NA, intern = FALSE, quitOnError = FALSE) {
  if(!is.na(outputfile) && file.exists(outputfile)) {
    cat("Output for step exists, skipping this step\n");
    invisible("")
  }
  cat("----", x, "\n"); res <- system(x, intern = intern); cat(">>>>", res[1], "\n")
  if(res[1] >= 1) {
    cat("Error external process did not finish\n\n");
    if(quitOnError) q("no")
  }
}