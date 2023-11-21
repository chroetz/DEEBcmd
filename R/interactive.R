#' @export
interact <- function(dbPath = NULL) {
  if (is.null(dbPath)) dbPath <- getwd()
  dbPath <- normalizePath(dbPath)
  if (!DEEBpath::isDeebDb(dbPath)) {
    stop(
      dbPath,
      " is not a DEEB database.",
      " Call `interact()` with a DEEB database as working directory",
      " or `interact('path/to/DEEBdatabase')`.")
  }
  askUserWhatToEval(dbPath)
}


askUserWhatToEval <- function(dbPath = ".") {

  dbPath <- normalizePath(dbPath, winslash="/", mustWork=TRUE)
  choice <- getUserInput(
    "Choose what to do",
    c("copyTruth" = "DEEBesti: copy truth",
      "hyper" = "DEEBesti: choose",
      "choose" = "DEEBeval: choose",
      "scan" = "DEEBeval: new, choose",
      "scanRun" = "DEEBeval: new, no plots, no scoreHTML, summary",
      "evalAll" = "DEEBeval: all, no plots, scoreHTML, summary",
      "evalAllSumm" = "DEEBeval: all, no plots, no scoreHTML, summary",
    ))

  switch(
    choice,
    copyTruth = startCopyTruth(dbPath),
    hyper = interactHyper(dbPath),
    scan = interactScanEval(dbPath),
    scanRun = startNewEval(dbPath),
    choose = interactChoose(dbPath),
    evalAll = startEvaluation(dbPath, FALSE, TRUE, TRUE),
    evalAllSumm = startEvaluation(dbPath, FALSE, TRUE, TRUE),
    stop("Choice not implemented."))
}


startCopyTruth <- function(dbPath) {
  startComp(
    rlang::expr_text(rlang::expr(DEEBesti::copyTruth(!!dbPath))),
    prefix = "DEEB_copyTruth",
    timeInMinutes = 20,
    mail = TRUE
  )
  return(invisible())
}


interactHyper <- function(dbPath) {
  cat("Scaning for possible choices...\n")
  methodTable <- DEEBpath::getMethodTableHyper(dbPath)
  modelFilter <- getUserInput(
    "Choose model(s)",
    methodTable$model |> unique(),
    multi = TRUE,
    default = "all")
  methodTable <- methodTable |> dplyr::filter(model %in% modelFilter)
  obsNameFilter <- getUserInput(
    "Choose obs",
    methodTable$obs |> unique(),
    multi = TRUE,
    default = "all")
  methodTable <- methodTable |> dplyr::filter(obs %in% obsNameFilter)
  methodsFilter <- getUserInput(
    "Choose method(s)",
    methodTable$method |> unique(),
    multi = TRUE,
    default = "all")
  methodTable <- methodTable |> dplyr::filter(method %in% methodsFilter)
  truthNrs <- DEEBpath::getUniqueTruthNrs(
    dbPath,
    modelFilter = modelFilter)
  truthNrFilter <- getUserInputNrs(
    "Choose truthNr(s)",
    truthNrs,
    multi = TRUE,
    default = "all")
  forceOverwrite <- getUserInputYesNo(
    "Force overwrite?",
    default = "No")
  readyToStart <- getUserInputYesNo(
    "Ready to start?",
    default = "Yes")
  if (readyToStart)
    startEstimHyper(
      dbPath,
      methodTable,
      truthNrFilter,
      forceOverwrite
    )
  return(invisible())
}


interactScanEval <- function(dbPath) {
  createPlots <- getUserInputYesNo(
    "Should plots be created?",
    default = "No")
  writeScoreHtml <- getUserInputYesNo(
    "Should the scores-html be created?",
    default = "Yes")
  createSummary <- getUserInputYesNo(
    "Should summary be created?",
    default = "Yes")
  readyToStart <- getUserInputYesNo(
    "Ready to start?",
    default = "Yes")
  if (readyToStart) {
    startComp(
      rlang::expr_text(rlang::expr(
        DEEBeval::runEvalTbl(
          !!dbPath,
          DEEBpath::getNew(!!dbPath),
          createPlots = !!createPlots,
          writeScoreHtml = !!writeScoreHtml,
          createSummary = !!createSummary,
          verbose = FALSE
        )
      )),
      prefix = "DEEBeval",
      timeInMinutes = 120,
      mail = TRUE
    )
  }
}


startNewEval <- function(dbPath) {
  startComp(
    rlang::expr_text(rlang::expr(
      DEEBeval::runEvalTbl(
        !!dbPath,
        DEEBpath::getNew(!!dbPath),
        createPlots = FALSE,
        writeScoreHtml = FALSE,
        createSummary = TRUE,
        verbose = FALSE
      )
    )),
    prefix = "DEEBeval",
    timeInMinutes = 120,
    mail = TRUE
  )
}


interactChoose <- function(dbPath) {
  cat("Scaning for possible choices...\n")
  analysis <- DEEBpath::getUniqueEntriesForEval(dbPath)
  models <- getUserInput(
    "Choose model(s)",
    analysis$models,
    multi = TRUE,
    default = "all")
  methodsFilter <- getUserInput(
    "Choose method(s)",
    analysis$methods,
    multi = TRUE,
    default = "all")
  truthNrFilter <- getUserInputNrs(
    "Choose truthNr(s)",
    analysis$truthNrs,
    multi = TRUE,
    default = "all")
  obsNrFilter <- getUserInputNrs(
    "Choose obsNr(s)",
    analysis$obsNrs,
    multi = TRUE,
    default = "all")
  taskNrFilter <- getUserInputNrs(
    "Choose taskNr(s)",
    analysis$taskNrs,
    multi = TRUE,
    default = "all")
  scoreFilter <- getUserInput(
    "Choose scoreFunction(s)",
    analysis$scoreFunctions,
    multi = TRUE,
    default = "all")
  createPlots <- getUserInputYesNo(
    "Should plots be created?",
    default = "No")
  writeScoreHtml <- getUserInputYesNo(
    "Should the scores-html be created?",
    default = "Yes")
  createSummary <- getUserInputYesNo(
    "Should summary be created?",
    default = "Yes")
  readyToStart <- getUserInputYesNo(
    "Ready to start?",
    default = "Yes")
  if (readyToStart) {
    startComp(
      rlang::expr_text(rlang::expr(
        DEEBeval::runEval(
          dbPath = !!dbPath,
          models = !!models,
          methodsFilter = !!methodsFilter,
          obsNrFilter = !!obsNrFilter,
          truthNrFilter = !!truthNrFilter,
          taskNrFilter = !!taskNrFilter,
          scoreFilter = !!scoreFilter,
          createPlots = !!createPlots,
          writeScoreHtml = !!writeScoreHtml,
          createSummary = !!createSummary,
          verbose = FALSE
        )
      )),
      prefix = "DEEBeval",
      timeInMinutes = 120,
      mail = TRUE
    )
  }
}

startEvaluation <- function(dbPath, createPlots, writeScoreHtml, createSummary) {
  models <- DEEBpath::getModels(dbPath)
  startComp(
    rlang::expr_text(rlang::expr(
      DEEBeval::runEval(
        dbPath = !!dbPath,
        models = !!models,
        createPlots = !!createPlots,
        writeScoreHtml = !!writeScoreHtml,
        createSummary = !!createSummary,
        verbose = FALSE
      )
    )),
    prefix = "DEEBeval",
    timeInMinutes = 240,
    mail = TRUE
  )
}
