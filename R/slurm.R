#' @export
startComp <- function(cmdStr, prefix="DEEB", timeInMinutes=NULL, mail=TRUE, startAfterJobIds=NULL) {
  if (isSlurmAvailable()) {
    jobName <- paste0(prefix, "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
    cat("Starting SLURM job", jobName, "\n")
    logDir <- DEEBpath::getLogDir(NULL, relative=TRUE)
    if (!dir.exists(logDir)) dir.create(logDir, recursive=TRUE, showWarnings=FALSE)
    command <- paste0(
      "sbatch ",
      " --qos=short",
      " --job-name=", jobName,
      " --output=",logDir, "/", jobName, "_%j.out",
      " --error=",logDir, "/", jobName, "_%j.err",
      if (mail) " --mail-type=END",
      if (!is.null(timeInMinutes)) " --time=", timeInMinutes,
      if (length(startAfterJobIds) > 0) paste0(" --dependency=afterany:", paste(startAfterJobIds, collapse=":")),
      " --wrap=\"Rscript -e '", gsub("\"", "\\\\\"", cmdStr), "'\"")
    cat(command, "\n")
    output <- system(command, intern = TRUE)
    jobId <- extractJobId(output)
    return(jobId)
  } else {
    cat("Evaluating following R expression:\n", cmdStr, "\n", sep="")
    eval(rlang::parse_expr(cmdStr))
  }
}

isSlurmAvailable <- function() {
  return(suppressWarnings(system2("srun", stdout = FALSE, stderr = FALSE) != 127))
}

extractJobId <- function(x) {
  if (startsWith(x, "Submitted batch job ")) {
    return(as.numeric(substring(x, 21)))
  } else {
    return(NA)
  }
}
