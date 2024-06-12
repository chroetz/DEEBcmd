#' @export
startEstimHyper <- function(
  dbPath,
  methodTable,
  truthNrFilter,
  forceOverwrite = FALSE,
  runSummaryAfter = TRUE,
  auto = FALSE,
  runLocal = FALSE,
  parallel = FALSE,
  isFirstCall = FALSE
) {

  jobCollection <- collectJobs(
    dbPath,
    methodTable,
    truthNrFilter,
    forceOverwrite
  )

  cat("There are", jobCollection$n, "jobs to do;", jobCollection$nSkipped, "others were skipped.\n")

  jobIds <- numeric()

  if (jobCollection$n == 0) {
    cat("Nothing to do.\n")
    if (!isFirstCall) return(invisible())
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
          timeInMinutes = if(hasValue(jobInfo$timeInMinutes)) jobInfo$timeInMinutes else 10,
          mail = FALSE)
        jobIds <- c(jobIds, jobId)
      }
    }
  }

  if (runSummaryAfter) {
    jobIds <- startNewEval(dbPath, startAfterJobIds = jobIds)
  }

  if (auto) {
    methods <- methodTable$method |> unique()
    jobIds <- startGenCube(dbPath, jobIds, methods)

    cmdText <-  rlang::expr_text(rlang::expr(
      DEEBcmd::interactAutoHyper(!!dbPath, auto = TRUE, runLocal = !!runLocal, parallel = !!parallel, isFirstCall = FALSE)
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
  expressionList <- list()
  methodInfoList <- list()
  prefixList <- character()

  for (i in seq_len(nrow(methodTable))) {

    methodInfo <- methodTable[i, ]
    obsNr <- DEEBpath::getObsNrFromName(dbPath, methodInfo$model, methodInfo$obs)
    hyperParmsPath <- DEEBpath::getMethodFile(dbPath, methodInfo$method)
    hyperParmsList <- ConfigOpts::readOptsBare(hyperParmsPath)
    if (ConfigOpts::getClassAt(hyperParmsList, 1) == "List") {
      hyperParmsList <- ConfigOpts::expandList(hyperParmsList)
      expansionNrList <- seq_along(hyperParmsList$list)
    } else {
      expansionNrList <- list(NULL)
    }

    for (expansionNr in expansionNrList) {
      if (forceOverwrite) {
        openTruthNrs <- truthNrFilter
      } else {
        openTruthNrs <- DEEBpath::getOpenTruthNrs(
          dbPath,
          truthNrFilter = truthNrFilter,
          obsNr = obsNr,
          model = methodInfo$model,
          method = methodInfo$method,
          expansionNr = expansionNr)
      }
      if (length(openTruthNrs) == 0) {
        cat("All results seem to exist. Skipping.\n")
        nSkipped <- nSkipped + 1
        next
      }
      cat(length(openTruthNrs), "new. Starting Job.\n")
      methodBase <- basename(methodInfo$method)
      methodInfoList <- append(methodInfoList, methodInfo)
      prefixList <- append(
        prefixList,
        if (is.null(expansionNr)) {
          paste("DEEBesti", methodInfo$model, methodBase, sep="-")
        } else {
          paste("DEEBesti", methodInfo$model, methodBase, expansionNr, sep="-")
        }
      )
      expressionList <- append(
        expressionList,
        rlang::expr(
          DEEBesti::runOne(
            dbPath = !!dbPath,
            truthNrFilter = !!openTruthNrs,
            obsNr = !!obsNr,
            model = !!methodInfo$model,
            method = !!methodInfo$method,
            expansionNr = !!expansionNr)
        )
      )
    }
  }

  jobTable <- tibble::tibble(
    expression = expressionList,
    prefix = prefixList) |>
    dplyr::bind_cols(methodInfo |> dplyr::bind_rows())

  return(lst(
    jobTable,
    n = nrow(jobTable),
    nSkipped))
}



evalExpressionList <- function(expressionList, parallel = TRUE, numCores = parallel::detectCores() - 1) {

  if (parallel) {
    cat("Create cluster of", numCores, "cores to run", length(expressionList), "in parallel.\n")
    cat("The following expressions will be executed on the parallel cluster:\n")
    for (expr in expressionList) cat(rlang::expr_text(expr), "\n")
    cat("Create Cluster.\n")
    cl <- parallel::makeCluster(numCores)
    cat("Start execution.\n")
    results <- parallel::parLapply(cl, expressionList, eval)
    cat("DOne. Stop Cluster.\n")
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
