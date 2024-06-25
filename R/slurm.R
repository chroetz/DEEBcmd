#' @export
startComp <- function(cmdStr, prefix="DEEB", timeInMinutes=NULL, nCpus = 1, mail=TRUE, startAfterJobIds=NULL, autoId = NULL) {
  cat("startComp():", format(Sys.time()), "\n")
  if (isSlurmAvailable()) {
    jobName <- paste0(prefix, "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
    cat("Starting SLURM job", jobName, "\n")
    logDir <- DEEBpath::getLogDir(NULL, relative=TRUE, autoId=autoId)
    if (!dir.exists(logDir)) dir.create(logDir, recursive=TRUE, showWarnings=FALSE)
    command <- paste0(
      "sbatch ",
      " --qos=short",
      " --job-name=", jobName,
      " --output=",logDir, "/", jobName, "_%j.out",
      " --error=",logDir, "/", jobName, "_%j.err",
      if (mail) " --mail-type=END",
      if (hasValue(timeInMinutes)) " --time=", timeInMinutes,
      if (hasValue(nCpus)) " --cpus-per-task=", nCpus,
      if (length(startAfterJobIds) > 0) paste0(" --dependency=afterany:", paste(startAfterJobIds, collapse=":")),
      " --wrap=\"Rscript -e '", gsub("\"", "\\\\\"", cmdStr), "'\"")
    cat(command, "\n")
    output <- system(command, intern = TRUE)
    cat(output, "\n")
    jobId <- extractJobId(output)
    return(jobId)
  } else {
    cat("No slurm. Evaluating following R expression directly:\n", cmdStr, "\n", sep="")
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
