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
      "copyBest" = "DEEBeval: copyBest",
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
    evalAll = startEvaluation(dbPath, FALSE, TRUE, TRUE, FALSE),
    evalAllSumm = startEvaluation(dbPath, FALSE, FALSE, TRUE, FALSE),
    onlyScores = startEvaluation(dbPath, FALSE, FALSE, FALSE, FALSE),
    onlyScoreHtml = startScoresHtml(dbPath),
    onlySummary = startSummary(dbPath),
    genCube = startGenCube(dbPath),
    copyBest = startCopyBest(dbPath),
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
  methodTableNamesChosenName <- getUserInput(
    "Choose method tables(s)",
    names(methodTableNamesAll),
    multi = TRUE,
    default = "all")
  methodTableNamesChosen <- methodTableNamesAll[methodTableNamesChosenName]
  methodTable <- DEEBpath::getMethodTable(dbPath, methodTableNamesChosen)
  modelFilter <- getUserInput(
    "Choose model(s)",
    methodTable$model |> unique(),
    multi = TRUE,
    default = "all")
  methodTable <- methodTable |> dplyr::filter(.data$model %in% modelFilter)
  obsNameFilter <- getUserInput(
    "Choose obs",
    methodTable$obs |> unique(),
    multi = TRUE,
    default = "all")
  methodTable <- methodTable |> dplyr::filter(.data$obs %in% obsNameFilter)
  methodsFilter <- getUserInput(
    "Choose method file(s)",
    methodTable$methodFile |> unique(),
    multi = TRUE,
    default = "all")
  methodTable <- methodTable |> dplyr::filter(.data$method %in% methodsFilter)
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
  runLocalParallel <- getUserInputYesNo(
    "Run local and in parallel?",
    default = "No")
  readyToStart <- getUserInputYesNo(
    "Ready to start?",
    default = "Yes")
  if (readyToStart)
    startEstimHyper(
      dbPath,
      methodTable,
      truthNrFilter,
      forceOverwrite,
      runLocal = runLocalParallel,
      parallel = runLocalParallel
    )
  return(invisible())
}


#' @export
interactAutoHyper <- function(dbPath, runLocal = FALSE, parallel = FALSE, autoId = NULL) {
  if (hasValue(autoId)) {
    methodTablePaths <- DEEBpath::getMethodTableNames(dbPath, autoId = autoId)
  } else {
    autoId <- DEEBpath::newAutoId(dbPath)
    cat("Scaning for possible choices...\n")
    methodTablePathsAll <- DEEBpath::getMethodTableNames(dbPath)
    methodTablePathsNames <- getUserInput(
      "Choose method tables(s)",
      names(methodTablePathsAll),
      multi = TRUE,
      default = "all")
    methodTablePaths <- methodTablePathsAll[methodTablePathsNames]
    if (isSlurmAvailable()) {
      runLocal <- FALSE
      parallel <- FALSE
    } else {
      runLocal <- TRUE
      parallel <- getUserInputYesNo("parallel?", "Yes")
    }

    DEEBpath::initializeAuto(dbPath, methodTablePaths, autoId = autoId)
  }

  if (length(methodTablePaths) == 0) {
    stop("Did not find any method table name.")
  }

  methodTable <- DEEBpath::getMethodTable(dbPath, methodTablePaths)
  truthNrs <- DEEBpath::getUniqueTruthNrs(dbPath)

  startEstimHyper(
    dbPath,
    methodTable,
    truthNrs,
    forceOverwrite = FALSE,
    runSummaryAfter = TRUE,
    autoId = autoId,
    runLocal = runLocal,
    parallel = parallel)
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
  onlySummarizeScore <- getUserInputYesNo(
    "onlySummarizeScore?",
    default = "No")
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
          onlySummarizeScore = !!onlySummarizeScore,
          verbose = FALSE
        )
      )),
      prefix = "DEEBevalrunEvalTbl-choose",
      timeInMinutes = 120,
      mail = TRUE
    )
  }
}


startNewEval <- function(dbPath, startAfterJobIds = NULL) {
  jobId <- startComp(
    rlang::expr_text(rlang::expr(
      DEEBeval::runEvalTbl(
        !!dbPath,
        DEEBpath::getNew(dbPath = !!dbPath),
        createPlots = FALSE,
        writeScoreHtml = FALSE,
        createSummary = TRUE,
        verbose = FALSE,
        onlySummarizeScore = FALSE
      )
    )),
    prefix = "DEEBeval-runEvalTbl-all",
    timeInMinutes = 200,
    mail = TRUE,
    startAfterJobIds = startAfterJobIds
  )
  return(jobId)
}


startNewEvalAuto <- function(dbPath, startAfterJobIds = NULL, autoId = NULL, autoRound = NULL) {
  cmd <- rlang::expr_text(rlang::expr(
      DEEBeval::runEvalTbl(
        !!dbPath,
        DEEBpath::getNewAuto(!!dbPath, autoId = !!autoId, autoRound = !!autoRound),
        createPlots = FALSE,
        writeScoreHtml = FALSE,
        createSummary = FALSE,
        verbose = FALSE,
        onlySummarizeScore = TRUE
      )
    ))
  jobId <- startComp(
    cmd,
    prefix = "DEEBeval-runEvalTbl-all",
    timeInMinutes = 200,
    mail = TRUE,
    startAfterJobIds = startAfterJobIds
  )
  return(jobId)
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
  onlySummarizeScore <- getUserInputYesNo(
    "onlySummarizeScore?",
    default = "No")
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
          onlySummarizeScore = !!onlySummarizeScore,
          verbose = FALSE
        )
      )),
      prefix = "DEEBeval-runEval-choosen",
      timeInMinutes = 120,
      mail = TRUE
    )
  }
}


startEvaluation <- function(dbPath, createPlots, writeScoreHtml, createSummary, onlySummarizeScore) {
  models <- DEEBpath::getModels(dbPath)
  startComp(
    rlang::expr_text(rlang::expr(
      DEEBeval::runEval(
        dbPath = !!dbPath,
        models = !!models,
        createPlots = !!createPlots,
        writeScoreHtml = !!writeScoreHtml,
        createSummary = !!createSummary,
        onlySummarizeScore = !!onlySummarizeScore,
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


startGenCube <- function(dbPath, startAfterJobIds = NULL, methodTable = NULL, autoId = NULL) {
  if (hasValue(methodTable)) {
    filePath <- tempfile(pattern = "methodsTable", tmpdir = DEEBpath::autoIdDir(dbPath, autoId), fileext = ".csv")
    readr::write_csv(methodTable, filePath)
  } else {
    filePath <- NULL
  }
  jobId <- startComp(
    rlang::expr_text(rlang::expr(
      DEEBeval::generateBestHyperCube(!!dbPath, !!filePath, !!autoId))),
    prefix = "DEEBeval-genCube",
    timeInMinutes = 10,
    mail = TRUE,
    startAfterJobIds = startAfterJobIds)
  return(jobId)
}


startCopyBest <- function(dbPath) {
  cat("Choose target DEEB DB:\n")
  toDbPath <- getUserInputDeebDb(dbPath)
  DEEBeval::copyBest(dbPath, toDbPath)
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
