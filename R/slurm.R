#' @export
startComp <- function(cmdStr, prefix="DEEB", timeInMinutes=NULL, nCpus = 1, mail=TRUE, startAfterJobIds=NULL, autoId = NULL, dbPath = NULL) {
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
    Sys.sleep(1)
    cat(output, "\n")
    if (is.character(output)) output <- paste(output, collapse="\n")
    if (length(output) == 1 && stringr::str_detect(output, stringr::fixed("error", ignore_case = TRUE))) {
      logFailedSubmission(dbPath, autoId, output, command)
      return(NULL)
    }
    jobId <- extractJobId(output)
    return(jobId)
  } else {
    cat("No slurm. Evaluating following R expression directly:\n", cmdStr, "\n", sep="")
    eval(rlang::parse_expr(cmdStr))
  }
}


startSlurmArray <- function(cmdFilePath, n, prefix, timeInMinutes, nCpus, mail, startAfterJobIds, dbPath=NULL, autoId=NULL, pause=60) {
  cmdFilePath <- normalizePath(cmdFilePath, winslash="/", mustWork=TRUE)
  jobName <- paste0(prefix, "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
  cat("Starting SLURM array", jobName, "\n")
  logDir <- DEEBpath::getLogDir(NULL, relative=TRUE, autoId=autoId)
  if (!dir.exists(logDir)) dir.create(logDir, recursive=TRUE, showWarnings=FALSE)
  command <- paste0(
    "sbatch ",
    " --qos=short",
    " --array=1-", n,
    " --job-name=", jobName,
    " --output=",logDir, "/", jobName, "_%A_%a.out",
    " --error=",logDir, "/", jobName, "_%A_%a.err",
    if (mail) " --mail-type=END",
    if (hasValue(timeInMinutes)) " --time=", timeInMinutes,
    if (hasValue(nCpus)) " --cpus-per-task=", nCpus,
    if (length(startAfterJobIds) > 0) paste0(" --dependency=afterany:", paste(startAfterJobIds, collapse=":")),
    " --wrap=\'Rscript -e \"DEEBcmd::startArrayTask(\\\"", cmdFilePath,"\\\", $SLURM_ARRAY_TASK_ID)\"\'")
  cat(command, "\n")
  output <- system(command, intern = TRUE)
  Sys.sleep(pause)
  cat(output, "\n")
  if (is.character(output)) output <- paste(output, collapse="\n")
  if (length(output) == 1 && stringr::str_detect(output, stringr::fixed("error", ignore_case = TRUE))) {
    logFailedSubmission(dbPath, autoId, output, command)
    return(NULL)
  }
  jobId <- extractJobId(output)
  return(jobId)
}


#' @export
startArrayTask <- function(cmdFilePath, arrayTaskNr) {
  cmdTextAll <- readLines(cmdFilePath)
  startLineIdx <- stringr::str_which(cmdTextAll, stringr::fixed(paste0("# START ", arrayTaskNr)))
  endLineIdx <- stringr::str_which(cmdTextAll,  stringr::fixed(paste0("# END ", arrayTaskNr)))
  if (length(startLineIdx) != 1  || length(endLineIdx) != 1) {
    stop(
      "cmdFilePath=", cmdFilePath,
      ",arrayTaskNr=", arrayTaskNr,
      ",length(startLineIdx)=", length(startLineIdx),
      ",length(endLineIdx)=", length(endLineIdx))
  }
  stopifnot(length(startLineIdx) == 1)
  stopifnot(length(endLineIdx) == 1)
  stopifnot(startLineIdx+1 <= endLineIdx-1)
  cmdText <- paste(cmdTextAll[(startLineIdx+1):(endLineIdx-1)], collapse="\n")
  cat("Evaluate the expression:\n")
  cat(cmdText, "\n")
  expr <- rlang::parse_expr(cmdText)
  eval(expr)
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


logFailedSubmission <- function(dbPath, autoId, message, command) {
  filePath <- tempfile(paste0("failedSubmissions_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), "_"), tmpdir=DEEBpath::getLogDir(dbPath), fileext = ".txt")
  writeLines(c(message,"\n",command,"\n",dbPath,"\n",autoId,"\n"), filePath)
}


getNumberOfActiveSlurmJobs <- function() {
  nLines <-
    system("squeue -u cschoetz | wc -l", intern=TRUE) |>
    as.integer()
  stopifnot(length(nLines) == 1 && !is.na(nLines))
  return(nLines - 1)
}
