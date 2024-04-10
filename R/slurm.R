#' @export
startComp <- function(cmdStr, prefix="DEEB", timeInMinutes=NULL, mail=TRUE, startAfterJobIds=NULL) {
  if (isSlurmAvailable()) {
    jobName <- paste0(prefix, "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
    cat("Starting SLURM job", jobName, "\n")
    if (!dir.exists("_log")) dir.create("_log")
    command <- paste0(
      "sbatch ",
      " --qos=short",
      " --job-name=", jobName,
      " --output=_log/", jobName, "_%j.out",
      " --error=_log/", jobName, "_%j.err",
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
