startEstimHyper <- function(
  dbPath,
  methodTable,
  truthNrFilter,
  forceOverwrite = FALSE
) {

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
        next
      }
      methodBase <- basename(methodInfo$method)
      startComp(
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
    }
  }
}
