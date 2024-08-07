#' @export
startEstimHyper <- function(
  dbPath,
  methodTable,
  truthNrFilter,
  forceOverwrite = FALSE,
  runSummaryAfter = TRUE,
  runLocal = FALSE,
  parallel = FALSE,
  splitByTruthNr = FALSE
) {

  jobCollection <- collectJobs(
    dbPath,
    methodTable,
    truthNrFilter,
    forceOverwrite,
    pastJobs = NULL,
    splitByTruth = splitByTruthNr
  )

  cat("There are", jobCollection$n, "jobs to do;", jobCollection$nSkipped, "others were skipped.\n")

  if (jobCollection$nSkipped + jobCollection$n == 0) {
    cat("No jobs at all. Stopping.\n")
    return(invisible())
  }

  jobIds <- numeric()

  if (jobCollection$n == 0) {
    cat("Nothing to do.\n")
    return(invisible())
  } else {
    if (runLocal) {
      cat("Run jobs local.\n")
      evalExpressionList(dbPath, jobCollection$jobTable$expression, parallel = parallel)
    } else {
      cat("Try to use slurm to run job arrays.\n")
      jobData <-
        jobCollection$jobTable |>
        tidyr::nest(data = c("methodName", "expansionNr", "expression", "prefix"))
      for (i in seq_len(nrow(jobData))) {
        jobDataRow <- jobData[i, ]
        jobId <- evalExpressionListSlurm(
          jobDataRow$data[[1]]$expression,
          dbPath = dbPath,
          autoId = NULL,
          prefix = "startEstimHyper",
          timeInMinutes = jobDataRow$timeInMinutes,
          nCpus = jobDataRow$nCpus
        )
        jobIds <- c(jobIds, jobId)
      }
    }
  }

  if (runSummaryAfter) {
    jobIds <- startNewEval(dbPath, startAfterJobIds = jobIds)
  }

  return(jobIds)
}



#' @export
initOneEstimAutoHyper <- function(
  dbPath,
  runLocal,
  parallel,
  methodInfo
) {

  methodInfo <- tibble::as_tibble(methodInfo)

  autoId <- DEEBpath::initializeAuto(
    dbPath,
    methodInfo,
    runLocal = runLocal,
    parallel = parallel)

  cat("autoId: ", autoId, "\n")

  jobCollection <- collectJobs(
    dbPath,
    methodInfo,
    truthNrFilter = NULL,
    forceOverwrite = FALSE,
    pastJobs = NULL)

  cat("There are", jobCollection$n, "jobs to do;", jobCollection$nSkipped, "others were skipped.\n")

  if (jobCollection$nSkipped + jobCollection$n == 0) {
    cat("No jobs at all. Stopping.\n")
    return(invisible())
  }

  if (jobCollection$n > 0) {
    autoRound <- DEEBpath::addToPastJobs(dbPath, autoId, jobCollection$jobTable)
  } else {
    autoRound <- NULL
  }

  jobIds <- numeric()

  if (jobCollection$n == 0) {
    cat("Nothing to do.\n")
    cat("But is first call. So may need to check for best.\n")
    warning("Does not calculate scores of previously run jobs.", immediate.=TRUE)
  } else {
    if (runLocal) {
      cat("Run jobs local.\n")
      evalExpressionList(dbPath, jobCollection$jobTable$expression, parallel = parallel, autoId=autoId)
    } else {
      cat("Try to use slurm to run jobs.\n")
      jobData <-
        jobCollection$jobTable |>
        tidyr::nest(data = c("methodName", "expansionNr", "expression", "prefix"))
      for (i in seq_len(nrow(jobData))) {
        jobDataRow <- jobData[i, ]
        jobId <- evalExpressionListSlurm(
          jobDataRow$data[[1]]$expression,
          dbPath = dbPath,
          autoId = autoId,
          prefix = "initEstimAutoHyper",
          timeInMinutes = jobDataRow$timeInMinutes,
          nCpus = jobDataRow$nCpus
        )
        jobIds <- c(jobIds, jobId)
      }
    }
  }
  cubeId <- startNewEvalAuto(dbPath, startAfterJobIds = jobIds, autoId = autoId, autoRound = autoRound)
  jobId <- startGenCube(dbPath, cubeId=cubeId, startAfterJobIds=cubeId, methodTable = methodInfo, autoId = autoId)
  cmdText <-  rlang::expr_text(rlang::expr(
    DEEBcmd::continueOneEstimAutoHyper(!!dbPath, autoId = !!autoId, cubeId = !!cubeId)
  ))
  jobId <- startComp(
    cmdText,
    prefix = "DEEBcmd-auto",
    timeInMinutes = 1440,
    mail = FALSE,
    startAfterJobIds = jobId,
    autoId = autoId,
    dbPath = dbPath)

  return(jobId)
}


#' @export
continueOneEstimAutoHyper <- function(dbPath, autoId, cubeId) {

  cat("autoId: ", autoId, "\n")
  cat("cubeId: ", cubeId, "\n")

  methodInfo <- DEEBpath::readAutoInfo(dbPath, autoId)

  methodTableFilePath <- file.path(
    DEEBpath::autoIdDir(dbPath, autoId),
    paste0(cubeId,".csv"))
  if (!file.exists(methodTableFilePath)) {
    stop("continueOneEstimAutoHyper cannot find cube file: ", methodTableFilePath)
  }
  methodTable <- DEEBpath::getMethodTable(dbPath, methodTableFilePath) # Expands Regex!!

  pastJobs <- DEEBpath::getPastJobs(dbPath, autoId)

  jobCollection <- collectJobs(
    dbPath,
    methodTable,
    truthNrFilter = NULL,
    forceOverwrite = FALSE,
    pastJobs = pastJobs
  )

  cat("There are", jobCollection$n, "jobs to do;", jobCollection$nSkipped, "others were skipped.\n")

  if (jobCollection$nSkipped + jobCollection$n == 0) {
    cat("No jobs at all. Stopping.\n")
    return(invisible())
  }

  if (jobCollection$n > 0) {
    autoRound <- DEEBpath::addToPastJobs(dbPath, autoId, jobCollection$jobTable)
  } else {
    autoRound <- NULL
  }

  jobIds <- numeric()

  if (jobCollection$n == 0) {
    cat("Nothing to do.\n")
    return(invisible())
  } else {
    if (methodInfo$runLocal) {
      cat("Run jobs local.\n")
      evalExpressionList(dbPath, jobCollection$jobTable$expression, parallel = methodInfo$parallel, autoId=autoId)
    } else {
      cat("Try to use slurm to run jobs.\n")
      jobData <-
        jobCollection$jobTable |>
        tidyr::nest(data = c("methodName", "expansionNr", "expression", "prefix"))
      for (i in seq_len(nrow(jobData))) {
        jobDataRow <- jobData[i, ]
        jobId <- evalExpressionListSlurm(
          jobDataRow$data[[1]]$expression,
          dbPath = dbPath,
          autoId = autoId,
          prefix = "contEstimAutoHyper",
          timeInMinutes = jobDataRow$timeInMinutes,
          nCpus = jobDataRow$nCpus
        )
        jobIds <- c(jobIds, jobId)
      }
    }
  }
  cubeId <- startNewEvalAuto(dbPath, startAfterJobIds = jobIds, autoId = autoId, autoRound = autoRound)
  jobId <- startGenCube(dbPath, cubeId=cubeId, startAfterJobIds=cubeId, methodTable = methodTable, autoId = autoId)
  cmdText <-  rlang::expr_text(rlang::expr(
    DEEBcmd::continueOneEstimAutoHyper(!!dbPath, autoId = !!autoId, cubeId = !!cubeId)
  ))
  jobId <- startComp(
    cmdText,
    prefix = "DEEBcmd-auto",
    timeInMinutes = 1440,
    mail = FALSE,
    startAfterJobIds = jobId,
    autoId = autoId,
    dbPath = dbPath)

  return(jobId)
}



collectJobs <- function(
  dbPath,
  methodTable,
  truthNrFilter,
  forceOverwrite,
  pastJobs,
  splitByTruth = FALSE
) {

  nSkipped <- 0

  methodTable <- tibble::as_tibble(methodTable)

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
      cat(methodInfo$model, "", hyperParms$name, ": ", sep="")
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
      if (splitByTruth) {
        exprs <- lapply(openTruthNrs, \(truthNr) {
          rlang::expr(
            DEEBesti::runOne(
              dbPath = !!dbPath,
              truthNrFilter = !!truthNr,
              obsNr = !!obsNr,
              model = !!methodInfo$model,
              method = !!methodInfo$methodFile,
              expansionNr = !!expansionNr))
        })
      } else {
        exprs <- list(rlang::expr(
            DEEBesti::runOne(
              dbPath = !!dbPath,
              truthNrFilter = !!openTruthNrs,
              obsNr = !!obsNr,
              model = !!methodInfo$model,
              method = !!methodInfo$methodFile,
              expansionNr = !!expansionNr)))
      }
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
            expression = exprs,
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


evalExpressionList <- function(dbPath, expressionList, parallel = TRUE, numCores = parallel::detectCores() - 1, autoId = NULL) {

  logDir <- DEEBpath::getLogDir(dbPath, autoId = autoId)
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

  cat("Run", length(expressionList), "expressions sequentially.\n")
  results <- lapply(seq_along(expressionList), \(i) {
    expr <- expressionList[[i]]
    cat(format(Sys.time()), "\n")
    cat(sprintf("Run following expression (%i/%i):\n", i, length(expressionList)))
    cat(rlang::expr_text(expr), "\n")
    res <- evalSave(expr)
    if (inherits(res, c("error", "condition"))) {
      cat("ERROR:", res$message, "\n")
      print(expr)
      print(res)
      stop()
    }
  })
  cat("Done evalutating expression list.\n")
  cat(format(Sys.time()), "\n")
  sink(file = NULL)
  return(results)
}


startSlurmJobToEvalExpressionList <- function(
  expressionList,
  dbPath,
  autoId = NULL,
  prefix = "DEEB",
  timeInMinutes = NULL,
  nCpus = 1,
  startAfterJobIds=NULL
) {
  cmdDir <- DEEBpath::getCmdDir(dbPath, autoId)
  if (!dir.exists(cmdDir))  dir.create(cmdDir)
  cmdFilePath <- DEEButil::getUniqueFileName(
    prefix = "cmd",
    dirPath = cmdDir,
    identifyingObject = expressionList,
    fileExtension = ".txt",
    fullPath = TRUE)
  exprTexts <- sapply(expressionList, rlang::expr_text)
  n <- length(expressionList)
  lines <- paste0("# START ", 1:n, "\n", exprTexts, "\n# END ", 1:n, "\n")
  writeLines(lines, cmdFilePath)

  expr <- rlang::expr(DEEBcmd::startSlurmJobListCarefully(
    !!cmdFilePath,
    dbPath = !!dbPath,
    autoId = !!autoId,
    prefix = !!prefix,
    timeInMinutes = !!timeInMinutes,
    nCpus = !!nCpus))

  startComp(
    rlang::expr_text(expr),
    prefix = "starter",
    timeInMinutes = 1440,
    nCpus = 1,
    startAfterJobIds = startAfterJobIds,
    dbPath = dbPath,
    autoId = autoId)
}


#' @export
startSlurmJobListCarefully <- function(
  cmdFilePath,
  dbPath,
  autoId,
  prefix,
  timeInMinutes,
  nCpus
) {

  cmdTextAll <- readLines(cmdFilePath)
  startLineIdx <- stringr::str_which(cmdTextAll, "^\\# START \\d+")
  endLineIdx <- stringr::str_which(cmdTextAll, "^\\# END \\d+")
  stopifnot(length(startLineIdx) == length(endLineIdx))
  stopifnot(all(startLineIdx + 1 <= endLineIdx - 1))
  exprList <- lapply(
    seq_along(startLineIdx),
    \(i) {
      cmdText <- paste(cmdTextAll[(startLineIdx[i]+1):(endLineIdx[i]-1)], collapse="\n")
      rlang::parse_expr(cmdText)
    })

  evalExpressionListSlurm(
    exprList,
    dbPath = dbPath,
    autoId = autoId,
    prefix = prefix,
    timeInMinutes = timeInMinutes,
    nCpus = nCpus)
}


evalExpressionListSlurm <- function(
  expressionList,
  dbPath,
  autoId = NULL,
  prefix = "DEEB",
  timeInMinutes = NULL,
  nCpus = 1,
  maxJobs = 4000
) {
  jobIds <- numeric()
  for (i in seq_along(expressionList)) {
    z <- 0
    while (getNumberOfActiveSlurmJobs() >= maxJobs) {
      z <- z + 1
      waitTimeInSeconds <- 10 + sample.int(pmin(1000, 2^z), 1)
      cat("Too many Jobs. Wait for", waitTimeInSeconds, "seconds.\n")
      Sys.sleep(waitTimeInSeconds)
    }
    jobId <- startComp(
      rlang::expr_text(expressionList[[i]]),
      prefix = prefix,
      timeInMinutes = timeInMinutes,
      nCpus = nCpus,
      mail = FALSE,
      startAfterJobIds = NULL,
      autoId = autoId,
      dbPath = dbPath,
      pause = 0)
    jobIds <- c(jobIds, jobId)
  }
  return(jobIds)
}
