#' Restart R session question
#'
#'Ask user if they would like to restart the current R session.  Accepts 'Y' or 'N', not case sensitive. Used in error
#'handling when overuse of system memory is identified.
#'
#' @export
#' @return If user enters Y, restart R session. Otherwise, return NULL.
#' @examples
#' askRestartR()

askRestartR <- function(){
  answer = readline(prompt = 'Restart R session to help free memory (Y/N)? ')
  if (toupper(answer) == 'Y'){
    .rs.restartR()
  }else if(toupper(answer) == 'N'){
    return(NULL)
  }else{
    writeLines('Please respond with Y or N')
    askQuestion()
  }
}