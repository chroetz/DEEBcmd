startEstimHyper <- function(
  dbPath,
  methodTable,
  truthNrFilter,
  forceOverwrite = FALSE
) {

  for (i in seq_len(nrow(methodTable))) {

    methodInfo <- methodTable[i, ]
    hyperParmsPath <- DEEBpath::getMethodFile(dbPath, methodInfo$method)
    hyperParmsList <- ConfigOpts::readOptsBare(hyperParmsPath)
    hyperParmsList <- ConfigOpts::expandList(hyperParmsList)
    obsNr <- DEEBpath::getObsNrFromName(dbPath, methodInfo$model, methodInfo$obs)

    len <- hyperParmsList$list |> length()
    expansionNrList <- if (len == 1) list(NULL) else seq_len(len)

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
        prefix = "DEEBesti",
        timeInMinutes = if(is.null(methodInfo$timeInMinutes)) 10 else methodInfo$timeInMinutes,
        mail = FALSE)
    }
  }
}
