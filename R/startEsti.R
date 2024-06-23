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

  if (hasValue(autoId)) {
    isFirstCall <- DEEBpath::isFirstAutoCall(dbPath, autoId)
  } else {
    isFirstCall <- FALSE
  }

  pastJobs <- if (hasValue(autoId)) DEEBpath::getPastJobs(dbPath, autoId) else NULL
  jobCollection <- collectJobs(
    dbPath,
    methodTable,
    truthNrFilter,
    forceOverwrite,
    pastJobs
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
      warning("Does not calculate scores of previously run jobs.")
    } else {
      return(invisible())
    }
  } else {
    if (runLocal) {
      cat("Run jobs local.\n")
      evalExpressionList(dbPath, jobCollection$jobTable$expression, parallel = parallel)
    } else {
      cat("Try to use slurm to run jobs.\n")
      for (i in seq_len(jobCollection$n)) {
        jobInfo <- jobCollection$jobTable[i, ]
        jobId <- startComp(
          rlang::expr_text(jobInfo$expression[[1]]),
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
  forceOverwrite,
  pastJobs
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
      if (NROW(pastJobs) > 0 && hyperParms$name %in% pastJobs$methodName) {
        cat("Tried this before (and apparently failed). Skipping.\n")
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
          paste("DEEBesti", methodInfo$model, basename(methodInfo$methodFile), sep="-")
        } else {
          paste("DEEBesti", methodInfo$model, basename(methodInfo$methodFile), expansionNr, sep="-")
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


evalSave <- function(expr) {
  tryCatch(
    eval(expr),
    error = function(cond) cond)
}


evalExpressionList <- function(dbPath, expressionList, parallel = TRUE, numCores = parallel::detectCores() - 1) {

  logDir <- DEEBpath::getLogDir(dbPath)
  dir.create(logDir, showWarnings=FALSE, recursive=TRUE)
  logFilePath <- file.path(logDir, format(Sys.time(), "DEEBcmd_evalExpressionList_%Y-%m-%d-%H-%M-%S.txt"))
  sink(logFilePath, split=TRUE)

  if (parallel) {
    numCores <- pmin(numCores, length(expressionList))
    cat("Create cluster of", numCores, "cores to run", length(expressionList), "in parallel.\n")
    cat("The following expressions will be executed on the parallel cluster:\n")
    for (expr in expressionList) cat(rlang::expr_text(expr), "\n")
    cat("Create Cluster.\n")
    cl <- parallel::makeCluster(numCores)
    cat(format(Sys.time()), "\n")
    cat("Start execution of", length(expressionList), "expressions on cluster with", numCores, "cores.\n")
    pt <- proc.time()
    results <- parallel::clusterApplyLB(cl, expressionList, evalSave)
    cat("Done after", (proc.time()-pt)[3], "s. Stop Cluster.\n")
    parallel::stopCluster(cl)
    errorDetected <- FALSE
    lapply(seq_along(results), \(i) {
      r <- results[[i]]
      if (inherits(r, c("error", "condition"))) {
        cat("ERROR:", r$message, "\n")
        errorDetected <- TRUE
      }
    })
    sink(file = NULL)
    if (errorDetected) stop()
    return(results)
  }

  cat("Run", length(expressionList), " expressions sequentially.\n")
  results <- lapply(expressionList, \(expr) {
    cat(format(Sys.time()), "\n")
    cat("Run following expression:\n")
    cat(rlang::expr_text(expr), "\n")
    res <- evalSave(expr)
    if (inherits(res, c("error", "condition"))) {
      cat("ERROR:", res$message, "\n")
    }
  })
  cat("Done evalutating expression list.\n")
  cat(format(Sys.time()), "\n")
  sink(file = NULL)
  return(results)
}
