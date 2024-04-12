cleanLogDir <- function(dbPath) {
  logDir <- DEEBpath::getLogDir(dbPath, relative=FALSE)
  if (dir.exists(logDir)) {
    unlink(logDir, recursive = TRUE, force = TRUE)
  }
}
