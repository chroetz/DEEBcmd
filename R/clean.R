cleanLogDir <- function(dbPath) {
  logDir <- DEEBpath::getLogDir(dbPath, relative=FALSE)
  if (dir.exists(logDir)) {
    nFiles <- length(list.files(logDir))
    cat("Deleting", nFiles, "files in", logDir, "\n")
    unlink(logDir, recursive = TRUE, force = TRUE)
    cat("Done.\n")
    return(invisible())
  }
  cat("No log files found.\n")
}
