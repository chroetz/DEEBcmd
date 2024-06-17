#' @export
startEstimHyper <- function(
  dbPath,
  methodTable,
  truthNrFilter,
  forceOverwrite = FALSE,
  runSummaryAfter = TRUE,
  autoId = NULL,
  runLocal = FALSE,
  parallel = FALSE
) {

  isFirstCall <- DEEBpath::isFirstAutoCall(dbPath, autoId)

  jobCollection <- collectJobs(
    dbPath,
    methodTable,
    truthNrFilter,
    forceOverwrite
  )

  cat("There are", jobCollection$n, "jobs to do;", jobCollection$nSkipped, "others were skipped.\n")

  if (jobCollection$nSkipped + jobCollection$n == 0) {
    cat("No jobs at all. Stopping.\n")
    return(invisible())
  }

  if (hasValue(autoId) && jobCollection$n > 0) {
      autoRound <- DEEBpath::addToPastJobs(dbPath, autoId, jobCollection$jobTable)
    } else {
      autoRound <- NULL
    }

  jobIds <- numeric()

  if (jobCollection$n == 0) {
    cat("Nothing to do.\n")
    if (isFirstCall) {
      cat("But is first call. So may need to check for best.\n")
    } else {
      return(invisible())
    }
  } else {
    if (runLocal) {
      cat("Run jobs local.\n")
      evalExpressionList(jobCollection$jobTable$expression, parallel = parallel)
    } else {
      cat("Try to use slurm to run jobs.\n")
      for (i in seq_len(jobCollection$n)) {
        jobInfo <- jobCollection$jobTable[i, ]
        jobId <- startComp(
          rlang::expr_text(jobInfo$expression),
          prefix = jobInfo$prefix,
          timeInMinutes = if(hasValue(jobInfo$timeInMinutes)) jobInfo$timeInMinutes else 60,
          nCpus = if(hasValue(jobInfo$nCpus)) jobInfo$nCpus else 1,
          mail = FALSE)
        jobIds <- c(jobIds, jobId)
      }
    }
  }

  if (runSummaryAfter) {
    if (hasValue(autoId)) {
      jobIds <- startNewEvalAuto(dbPath, startAfterJobIds = jobIds, autoId = autoId, autoRound = autoRound)
    } else {
      jobIds <- startNewEval(dbPath, startAfterJobIds = jobIds)
    }
  }

  if (hasValue(autoId)) {
    jobIds <- startGenCube(dbPath, jobIds, methodTable, autoId = autoId)

    cmdText <-  rlang::expr_text(rlang::expr(
      DEEBcmd::interactAutoHyper(!!dbPath, autoId = !!autoId, runLocal = !!runLocal, parallel = !!parallel)
    ))
    jobIds <- startComp(
      cmdText,
      prefix = "DEEBcmd-auto",
      timeInMinutes = 60,
      mail = FALSE,
      startAfterJobIds = jobIds)
  }

  return(jobIds)
}



collectJobs <- function(
  dbPath,
  methodTable,
  truthNrFilter,
  forceOverwrite
) {

  nSkipped <- 0

  jobTable <- dplyr::bind_cols(
    dplyr::tibble(
      methodName = character(),
      expansionNr = integer(),
      expression = list(),
      prefix = character(),
      obsNr = integer()
    ),
    methodTable[0,]
  )

  for (i in seq_len(nrow(methodTable))) {

    methodInfo <- methodTable[i, ]
    obsNr <- DEEBpath::getObsNrFromName(dbPath, methodInfo$model, methodInfo$obs)
    hyperParmsList <- DEEBesti::loadAsHyperParmsList(dbPath, methodInfo$methodFile)
    for (expansionNr in seq_along(hyperParmsList$list)) {
      hyperParms <- hyperParmsList$list[[expansionNr]]
      cat(hyperParms$name, ": ", sep="")
      if (forceOverwrite) {
        openTruthNrs <- truthNrFilter
      } else {
        openTruthNrs <- DEEBpath::getOpenTruthNrs(
          dbPath,
          truthNrFilter = truthNrFilter,
          obsNr = obsNr,
          model = methodInfo$model,
          methodName = hyperParms$name)
      }
      if (length(openTruthNrs) == 0) {
        cat("All results seem to exist. Skipping.\n")
        nSkipped <- nSkipped + 1
        next
      }
      cat(length(openTruthNrs), "new openTruthNrs. Adding job to list.\n")
      expr <- rlang::expr(
          DEEBesti::runOne(
            dbPath = !!dbPath,
            truthNrFilter = !!openTruthNrs,
            obsNr = !!obsNr,
            model = !!methodInfo$model,
            method = !!methodInfo$methodFile,
            expansionNr = !!expansionNr)
        )
      prefix <- if (is.null(expansionNr)) {
          paste("DEEBesti", methodInfo$model, methodInfo$methodFile, sep="-")
        } else {
          paste("DEEBesti", methodInfo$model, methodInfo$methodFile, expansionNr, sep="-")
        }
      jobTable <- dplyr::bind_rows(
        jobTable,
        dplyr::bind_cols(
          dplyr::tibble(
            methodName = hyperParms$name,
            expansionNr = expansionNr,
            expression = list(expr),
            prefix = prefix,
            obsNr = obsNr
          ),
          methodInfo))
    }
  }

  return(lst(
    jobTable,
    n = nrow(jobTable),
    nSkipped))
}



evalExpressionList <- function(expressionList, parallel = TRUE, numCores = parallel::detectCores() - 1) {

  if (parallel) {
    numCores <- pmin(numCores, length(expressionList))
    cat("Create cluster of", numCores, "cores to run", length(expressionList), "in parallel.\n")
    cat("The following expressions will be executed on the parallel cluster:\n")
    for (expr in expressionList) cat(rlang::expr_text(expr), "\n")
    cat("Create Cluster.\n")
    cl <- parallel::makeCluster(numCores)
    cat("Start execution of", length(expressionList), "expressions on cluster with", numCores, "cores.\n")
    pt <- proc.time()
    results <- parallel::clusterApplyLB(cl, expressionList, eval)
    cat("Done after", (proc.time()-pt)[3], "s. Stop Cluster.\n")
    parallel::stopCluster(cl)
    return(results)
  }

  cat("Run", length(expressionList), " expressions sequentially.\n")
  results <- lapply(expressionList, \(expr) {
    cat("Run following expression:\n")
    cat(rlang::expr_text(expr), "\n")
    eval(expr)
  })
  cat("Done evalutating expression list.\n")
  return(results)
}
