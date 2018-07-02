#' debug pipe chain
#'
#' Shows the output step by step. press \emph{s} to skip (i.e. not print, it still evaluates),
#' \emph{q} to quit. Explore environment as in \code{base::debug}, current output is stored in
#' \code{.} variable.
#'
#'@param expr expressions to go through, if not provided content of clipboard will be used
#'@examples
#'\dontrun{
#'library(dplyr)
#'debug_pipe(
#'test <- iris %>%
#'  slice(1:2) %>%
#'  select(1:3) %>%
#'  mutate(x=3)
#')
#'
#'# works in functions too :
#'fun <- function(){
#'  print("hello")
#'  debug_pipe(
#'    test <- iris %>%
#'      slice(1:2) %>%
#'      select(1:3) %>%
#'      mutate(x=3)
#'  )
#'  print("bye")
#'}
#'
#'fun()
#'}
#'@export
pdbg <- function(.expr){
  .pchain <-
    if (missing(.expr)) readClipboard() # windows only , else try clipr::read_clip()
  else deparse(substitute(.expr))

  .lhs    <- if (grepl("^\\s*[[:alnum:]_.]*\\s*<-",.pchain[1])) {
    sub("^\\s*([[:alnum:]_.]*)\\s*<-.*","\\1",.pchain[1])
  } else NA

  .pchain <- sub("[^%]*<-\\s*","",.pchain)        # remove lhs of assignment if exists
  .pchain <- paste(.pchain,collapse = " ")          # collapse
  .pchain <- gsub("\\s+"," ",.pchain)             # multiple spaces to single
  .pchain <- strsplit(.pchain,"\\s*%>%\\s*")[[1]] # split by pipe
  .pchain <- as.list(.pchain)

  for (i in rev(seq_along(.pchain))) {
    # function to count matches
    .f <- function(x) sum(gregexpr(x,.pchain[i],fixed = TRUE)[[1]] != -1)
    # check if unbalanced operators
    .balanced <-
      all(c(.f("{"),.f("("),.f("[")) == c(.f("}"),.f(")"),.f("]"))) &
      !.f("'") %% 2 &
      !.f('"') %% 2

    if (!.balanced) {
      # if unbalanced, combine with previous
      .pchain[[i - 1]] <- paste(.pchain[[i - 1]],"%>%", .pchain[[i]])
      .pchain[[i]] <- NULL
    }
  }

  .calls  <- Reduce(                             # build calls to display
    function(x,y) paste0(x," %>%\n  ",y),
    .pchain, accumulate = TRUE)

  .xinit  <- eval(parse(text = .pchain[1]))
  .values <- Reduce(function(x,y){               # compute all values
    if (inherits(x,"try-error")) NULL
    else try(eval(parse(text = paste("x %>%", y))),silent = TRUE)},
    .pchain[-1], .xinit, accumulate = TRUE)

  message("press enter to show, 's' to skip, 'q' to quit, lhs can be accessed with `.`")
  for (.i in (seq_along(.pchain))) {
    cat("\n",.calls[.i])
    .rdl_ <- readline()
    . <- .values[[.i]]

    # while environment is explored
    while (!.rdl_ %in% c("q","s","")) {
      # if not an assignment, should be printed
      if (!grepl("^\\s*[[:alnum:]_.]*\\s*<-",.rdl_)) .rdl_ <- paste0("print(",.rdl_,")")
      # wrap into `try` to safely fail
      try(eval(parse(text = .rdl_)))
      .rdl_ <- readline()
    }
    if (.rdl_ == "q")  return(invisible(NULL))
    if (.rdl_ != "s") {
      if (inherits(.values[[.i]],"try-error")) {
        # a trick to be able to use stop without showing that
        # debug_pipe failed in the output
        opt <- options(show.error.messages = FALSE)
        on.exit(options(opt))
        message(.values[[.i]])
        stop()
      } else
      {
        print(.)
      }
    }
  }
  if (!is.na(.lhs)) assign(.lhs,tail(.values,1),envir = parent.frame())
  invisible(NULL)
}


pdbg_addin <- function(){
  selection <- rstudioapi::primary_selection(
    rstudioapi::getSourceEditorContext())[["text"]]
  rstudioapi::sendToConsole("",execute = F)
  eval(parse(text=paste0("pdbg(",selection,")")))
}
