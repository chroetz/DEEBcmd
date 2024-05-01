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
      "autoHyper" = "DEEBesti: auto hyper",
      "choose" = "DEEBeval: choose",
      "scan" = "DEEBeval: new, choose",
      "scanRun" = "DEEBeval: new, no plots, no scoreHTML, summary",
      "evalAll" = "DEEBeval: all, no plots, scoreHTML, summary",
      "evalAllSumm" = "DEEBeval: all, no plots, no scoreHTML, summary",
      "onlyScores" = "DEEBeval: all, no plots, no scoreHTML, no summary",
      "onlyScoreHtml" = "DEEBeval: only scoreHTML",
      "onlySummary" = "DEEBeval: only summary",
      "genCube" = "DEEBeval: generate best hypercube",
      "clean" = "clean things (choose)"
    ))

  switch(
    choice,
    copyTruth = startCopyTruth(dbPath),
    hyper = interactHyper(dbPath),
    autoHyper = interactAutoHyper(dbPath),
    scan = interactScanEval(dbPath),
    scanRun = startNewEval(dbPath),
    choose = interactChoose(dbPath),
    evalAll = startEvaluation(dbPath, FALSE, TRUE, TRUE),
    evalAllSumm = startEvaluation(dbPath, FALSE, FALSE, TRUE),
    onlyScores = startEvaluation(dbPath, FALSE, FALSE, FALSE),
    onlyScoreHtml = startScoresHtml(dbPath),
    onlySummary = startSummary(dbPath),
    genCube = startGenCube(dbPath),
    clean = startCleanChoose(dbPath),
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
  methodTableNamesAll <- DEEBpath::getMethodTableNames(dbPath)
  methodTableNamesChosen <- getUserInput(
    "Choose method tables(s)",
    methodTableNamesAll,
    multi = TRUE,
    default = "all")
  methodTable <- DEEBpath::getMethodTable(dbPath, methodTableNamesChosen)
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


#' @export
interactAutoHyper <- function(dbPath, auto = FALSE) {
  if (auto) {
    methodTableNamesChosen <- DEEBpath::getMethodTableNames(dbPath, auto = TRUE)
  } else {
    cat("Scaning for possible choices...\n")
    methodTableNamesAll <- DEEBpath::getMethodTableNames(dbPath)
    methodTableNamesChosen <- getUserInput(
      "Choose method tables(s)",
      methodTableNamesAll,
      multi = TRUE,
      default = "all")
  }
  methodTable <- DEEBpath::getMethodTable(dbPath, methodTableNamesChosen)
  truthNrs <- DEEBpath::getUniqueTruthNrs(dbPath)
  startEstimHyper(
    dbPath,
    methodTable,
    truthNrs,
    forceOverwrite = FALSE,
    runSummaryAfter = TRUE,
    auto = TRUE)
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
      prefix = "DEEBevalrunEvalTbl-choose",
      timeInMinutes = 120,
      mail = TRUE
    )
  }
}


startNewEval <- function(dbPath, startAfterJobIds = NULL, auto = FALSE) {
  jobId <- startComp(
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
    prefix = "DEEBeval-runEvalTbl-all",
    timeInMinutes = 120,
    mail = TRUE,
    startAfterJobIds = startAfterJobIds
  )
  if (auto) {
    startGenCube(dbPath, jobId, auto = TRUE)
  }
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
      prefix = "DEEBeval-runEval-choosen",
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
    prefix = "DEEBeval-runEval-all",
    timeInMinutes = 240,
    mail = TRUE
  )
}

startScoresHtml <- function(dbPath) {
  models <- DEEBpath::getModels(dbPath)
  for (model in models) {
    startComp(
      rlang::expr_text(rlang::expr(
        DEEBeval::runScoreHtml(!!dbPath, !!model))),
      prefix = paste0("DEEBeval-scoresHtml-", model),
      timeInMinutes = 60,
      mail = TRUE)
  }
}


startSummary <- function(dbPath) {
  startComp(
    rlang::expr_text(rlang::expr(
      DEEBeval::createSummary(!!dbPath))),
    prefix = "DEEBeval-summary",
    timeInMinutes = 60,
    mail = TRUE)
}


startGenCube <- function(dbPath, startAfterJobIds = NULL, auto = FALSE) {
  jobId <- startComp(
    rlang::expr_text(rlang::expr(
      DEEBeval::generateBestHyperCube(!!dbPath))),
    prefix = "DEEBeval-genCube",
    timeInMinutes = 10,
    mail = TRUE,
    startAfterJobIds = startAfterJobIds)
  if (auto) {
    startComp(
      rlang::expr_text(rlang::expr(
        DEEBcmd::interactAutoHyper(!!dbPath, auto = TRUE))),
      prefix = "DEEBcmd-auto",
      timeInMinutes = 60,
      mail = FALSE,
      startAfterJobIds = jobId)
  }
}


startCleanChoose <- function(dbPath) {
  choice <- getUserInput(
    "Choose what to clean",
    c("log" = "log files",
      "empty" = "remove empty method dirs",
      "nonCsv" = "remove method dirs without csv files"))
  switch(
    choice,
    empty = DEEBpath::removeEmptyMethodFolders(dbPath, FALSE),
    nonCsv = DEEBpath::removeEmptyMethodFolders(dbPath, TRUE),
    log = cleanLogDir(dbPath),
    stop("Choice not implemented."))
}
