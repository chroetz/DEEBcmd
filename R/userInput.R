getUserInputNrs <- function(title, options, multi = FALSE, default = NULL) {
  getUserInput(title, options, multi = multi, default = default, onlyNrs = TRUE)
}

getUserInputYesNo <- function(title, default) {
  input <- getUserInput(title, c("Yes", "No"), multi = FALSE, default = default, onlyNrs = FALSE)
  switch(
    input,
    Yes = TRUE,
    No = FALSE)
}

getUserInput <- function(title, options, multi = FALSE, default = NULL, onlyNrs = FALSE) {
  cat("\n", title, "\n", sep="")

  possibleChoices <- NULL
  possibleChoicesNr <- NULL
  if (multi) {
    allText <- " 0: all"
    if (default == "all") allText <-  crayon::cyan(allText)
    cat(allText, "\n")
    possibleChoices <- c(possibleChoices, "all")
    possibleChoicesNr <- c(possibleChoicesNr, 0)
  }
  if (onlyNrs) {
    cat(" ")
    cat(ifelse(options %in% default, crayon::cyan(options), options), sep = ",")
    cat("\n")
    possibleChoices <- c(possibleChoices, options)
    possibleChoicesNr <- c(possibleChoicesNr, options)
  } else {
    numList <- paste0(" ", seq_along(options), ": ", options)
    cat(ifelse(options %in% default, crayon::cyan(numList), numList), sep = "\n")
    possibleChoices <- c(possibleChoices, options)
    possibleChoicesNr <- c(possibleChoicesNr, seq_along(options))
  }

  if (!is.null(default)) cat(
    "Leave empty for ",
    crayon::cyan("cyan"),
    " (", paste(default, collapse = ", "), ").\n", sep="")
  if (multi) {
    cat("Choose one or more options, e.g., '2', '3:6', '1,4,7':\n")
  } else {
    cat("Choose one option:\n")
  }

  input <- getLine()
  ids <- as.numeric(eval(parse(text = paste("c(", input, ")"))))
  if (!multi && length(ids) > 1) stop("Choose only one element!")
  if (any(!ids %in% possibleChoicesNr)) {
    stop("Choose numbers from ", paste(possibleChoicesNr, collapse=","), "!")
  }
  if (length(ids) == 0) {
    ids <- possibleChoicesNr[possibleChoices %in% default]
  }
  if (multi && 0 %in% ids) {
    chosenIdx <- seq_along(options)
  } else {
    if (onlyNrs) {
      chosenIdx <- which(options %in% ids)
    } else {
      chosenIdx <- ids
    }
  }
  cat("You chose: ", paste(options[chosenIdx], collapse = ","), "\n", sep = "")
  if (is.null(names(options))) {
    chosenElements <- options[chosenIdx]
  } else {
    chosenElements <- names(options)[chosenIdx]
  }
  return(chosenElements)
}

# Get one line of user input regardless of whether running interactively or not (via Rscript).
# (base::readline does not wait for user input when running via Rscript.)
getLine <- function() {
  if (interactive()) {
    # needed for e.g. RStudio and R in jupyter
    return(readline())
  }
  return(readLines(withr::local_connection(file("stdin")), n = 1))
}


getUserInputDeebDb <- function(startDir) {

  currentPath <- normalizePath(startDir, winslash="/")

  for (i in seq_len(1e4)) {
    isCurrentDeebDb <- DEEBpath::isDeebDb(currentPath)
    isParentDeebDb <- DEEBpath::isDeebDb(dirname(currentPath))
    currentDirs <- list.dirs(currentPath, recursive=FALSE)
    isDeebDb <- sapply(currentDirs, DEEBpath::isDeebDb)
    options <- c(
      if (isCurrentDeebDb) paste0("SELECT ."),
      if (isParentDeebDb) paste0("SELECT .."),
      if (any(isDeebDb)) paste0("SELECT ", basename(currentDirs[isDeebDb])),
      paste0("ENTER ", basename(currentDirs)),
      "ENTER ..")
    choice <- getUserInput(paste0(currentPath, " >> Choose:"),  options)
    if (startsWith(choice, "ENTER")) {
      newDir <- substring(choice, 7)
      if (newDir == "..") {
        currentPath <- normalizePath(dirname(currentPath), winslash="/")
      } else {
        currentPath <- file.path(currentPath, newDir)
      }
      next
    }
    if (startsWith(choice, "SELECT")) {
      selectDir <- substring(choice, 8)
      if (selectDir == ".") return(currentPath)
      if (selectDir == "..") return(dirname(currentPath))
      return(file.path(currentPath, selectDir))
    }
    stop("Cannot handle choice: ", choice)
  }

  stop("getUserInputDeebDb loop ended unexpectedly.")
}


getUserInputInteger <- function(title, default) {
  cat(title, "\ndefault:", default, "\n")
  getLine() |> as.integer()
}

