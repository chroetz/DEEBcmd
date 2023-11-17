startEstimHyper <- function(
  dbPath,
  methodTable,
  truthNrFilter
) {

  for (i in seq_len(nrow(methodTable))) {
    methodInfo <- methodTable[i, ]
    hyperParmsPath <- DEEBpath::getMethodFile(dbPath, methodInfo$method)
    hyperParmsList <- ConfigOpts::readOptsBare(hyperParmsPath)
    hyperParmsList <- ConfigOpts::expandList(hyperParmsList)
    len <- hyperParmsList$list |> length()

    for (i in seq_len(len)) {
      obsNr <- DEEBpath::getObsNrFromName(dbPath, methodInfo$model, methodInfo$obs)
      startComp(rlang::expr_text(rlang::expr(
        DEEBesti::runOne(
          dbPath = !!dbPath,
          obsNr = !!obsNr,
          model = !!methodInfo$model,
          method = !!methodInfo$method,
          estiOptsFileName = !!methodInfo$estiOpts,
          expansionNr = !!i)
      )))
    }

  }
}
