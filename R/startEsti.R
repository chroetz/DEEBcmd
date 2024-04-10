startEstimHyper <- function(
  dbPath,
  methodTable,
  truthNrFilter,
  forceOverwrite = FALSE,
  runSummaryAfter = TRUE,
  auto = FALSE
) {
  nSkipped <- 0
  nStarted <- 0

  jobIds <- numeric()

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
      nStarted <- nStarted + 1
      methodBase <- basename(methodInfo$method)
      jobId <- startComp(
        rlang::expr_text(rlang::expr(
          DEEBesti::runOne(
            dbPath = !!dbPath,
            truthNrFilter = !!openTruthNrs,
            obsNr = !!obsNr,
            model = !!methodInfo$model,
            method = !!methodInfo$method,
            expansionNr = !!expansionNr)
        )),
        prefix = if (is.null(expansionNr)) {
          paste("DEEBesti", methodInfo$model, methodBase, sep="-")
        } else {
          paste("DEEBesti", methodInfo$model, methodBase, expansionNr, sep="-")
        },
        timeInMinutes = if(is.null(methodInfo$timeInMinutes)) 10 else methodInfo$timeInMinutes,
        mail = FALSE)
      jobIds <- c(jobIds, jobId)
    }
  }

  cat("Started", nStarted, "jobs and skipped", nSkipped, "Jobs.\n")

  if (runSummaryAfter && nStarted > 0) {
    startNewEval(dbPath, startAfterJobIds = jobIds, auto = auto)
  }

}
