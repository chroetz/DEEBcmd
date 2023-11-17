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
    obsNr <- DEEBpath::getObsNrFromName(dbPath, methodInfo$model, methodInfo$obs)
    len <- hyperParmsList$list |> length()
    if (len == 1) {
        startComp(rlang::expr_text(rlang::expr(
          DEEBesti::runOne(
            dbPath = !!dbPath,
            obsNr = !!obsNr,
            model = !!methodInfo$model,
            method = !!methodInfo$method,
            estiOptsFileName = !!methodInfo$estiOpts)
        )),
        prefix = "DEEBesti",
        timeInMinutes = if(is.null(methodInfo$timeInMinutes)) 10 else methodInfo$timeInMinutes,
        mail = FALSE)
    } else {
      for (i in seq_len(len)) {
        startComp(rlang::expr_text(rlang::expr(
          DEEBesti::runOne(
            dbPath = !!dbPath,
            obsNr = !!obsNr,
            model = !!methodInfo$model,
            method = !!methodInfo$method,
            estiOptsFileName = !!methodInfo$estiOpts,
            expansionNr = !!i)
        )),
        prefix = "DEEBesti",
        timeInMinutes = if(is.null(methodInfo$timeInMinutes)) 10 else methodInfo$timeInMinutes,
        mail = FALSE)
      }
    }
  }

}
