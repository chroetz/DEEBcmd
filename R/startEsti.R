startEstimHyper <- function(
  dbPath,
  methodTable,
  truthNrFilter
) {

  for (i in seq_len(nrow(methodTable))) {
    methodInfo <- methodTable[i, ]

    len <- DEEBesti::getExpansionLength(
      obsNr = methodInfo$obsNr,
      model = methodInfo$model,
      method = methodInfo$method
    )

    for (i in seq_len(len)) {

      startComp(rlang::expr_text(rlang::expr(
        DEEBesti::runOne(
          dbPath = !!dbPath,
          obsNr = !!methodInfo$obsNr,
          model = !!methodInfo$model,
          method = !!methodInfo$method,
          expansionNr = !!i)
      )))
    }

  }
}
