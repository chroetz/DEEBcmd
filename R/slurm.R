#' @export
startComp <- function(cmdStr, prefix="DEEB") {
  if (isSlurmAvailable()) {
    jobName <- paste0(prefix, "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
    cat("Starting SLURM job", jobName, "\n")
    clcom <- paste0(
      "sbatch ",
      " --qos=short",
      " --job-name=", jobName,
      " --output=", jobName, "_%j.out",
      " --error=", jobName, "_%j.err",
      " --mail-type=END",
      " --wrap=\"Rscript -e '", gsub("\"", "\\\\\"", cmdStr), "'\"")
    cat(clcom, "\n")
    system(clcom)
  } else {
    cat("Evaluating following R expression:\n", cmdStr, "\n", sep="")
    eval(rlang::parse_expr(cmdStr))
  }
}

isSlurmAvailable <- function() {
  return(suppressWarnings(system2("srun", stdout = FALSE, stderr = FALSE) != 127))
}
