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
      "evalNew" = "DEEBeval: new, only csv in evaluation",
      "scanRun" = "DEEBeval: new, no plots, no scoreHTML, summary",
      "evalAll" = "DEEBeval: all, no plots, scoreHTML, summary",
      "evalAllSumm" = "DEEBeval: all, no plots, no scoreHTML, summary",
      "onlyScores" = "DEEBeval: all, no plots, no scoreHTML, no summary",
      "onlyScoresAndCsv" = "DEEBeval: all multijob",
      "onlyScoreHtml" = "DEEBeval: only scoreHTML",
      "onlyPlotting" = "DEEBeval: only plots",
      "summaryChoose" = "DEEBeval: summary choose",
      "overall" = "DEEBeval: overall",
      "info" = "DEEBeval: collectInfo",
      "genCube" = "DEEBeval: generate best hypercube",
      "checkOpti" = "DEEBeval: check scores for optimization of hyper parms",
      "copyBest" = "DEEBeval: copyBest",
      "copyRegex" = "DEEBeval: copy methods by RegEx",
      "clean" = "clean things (choose)"
    ))

  switch(
    choice,
    copyTruth = startCopyTruth(dbPath),
    hyper = interactHyper(dbPath),
    autoHyper = interactAutoHyper(dbPath),
    evalNew = startEvalNewPerModel(dbPath),
    scanRun = startNewEval(dbPath),
    choose = interactChoose(dbPath),
    evalAll = startEvaluation(dbPath, FALSE, TRUE, TRUE, FALSE),
    evalAllSumm = startEvaluation(dbPath, FALSE, FALSE, TRUE, FALSE),
    onlyScores = startEvaluation(dbPath, FALSE, FALSE, FALSE, FALSE),
    onlyScoresAndCsv = startEvaluationMultiJob(dbPath, FALSE),
    onlyScoreHtml = startScoresHtml(dbPath),
    onlyPlotting = startPlotting(dbPath),
    summaryChoose = startSummary(dbPath),
    overall = startOverall(dbPath),
    info = startCollectInfo(dbPath),
    genCube = startGenCube(dbPath),
    checkOpti = checkOptimizationScores(dbPath),
    copyBest = startCopyBest(dbPath),
    copyRegex = startCopyRegex(dbPath),
    clean = startCleanChoose(dbPath),
    stop("Choice not implemented."))
}


startCopyTruth <- function(dbPath) {
  startComp(
    rlang::expr_text(rlang::expr(DEEBesti::copyTruth(!!dbPath))),
    prefix = "DEEB_copyTruth",
    timeInMinutes = 20,
    mail = TRUE,
    dbPath = dbPath
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
  methodTable <- DEEBpath::getMethodTable(dbPath, methodTableNamesChosen)  # Expands RegEx!!
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
  methodTable <- methodTable |> dplyr::filter(.data$methodFile %in% methodsFilter)
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
  runSummaryAfter <- getUserInputYesNo(
    "Run Summary afterwards?",
    default = "No")
  runLocal <- getUserInputYesNo(
    "Run local?",
    default = if (isSlurmAvailable()) "No" else "Yes")
  parallel <- NULL
  if (runLocal) {
    parallel <- getUserInputYesNo(
      "Run in parallel?",
      default = "No")
  }
  splitByTruthNr <- getUserInputYesNo("Split by truthNr?", default = "No")
  readyToStart <- getUserInputYesNo(
    "Ready to start?",
    default = "Yes")
  if (readyToStart)
    startEstimHyper(
      dbPath,
      methodTable,
      truthNrFilter,
      forceOverwrite,
      runSummaryAfter = runSummaryAfter,
      runLocal = runLocal,
      parallel = parallel,
      splitByTruthNr = splitByTruthNr
    )
  return(invisible())
}


#' @export
interactAutoHyper <- function(dbPath) {

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

  if (length(methodTablePaths) == 0) {
    stop("Did not find any method table name.")
  }

  methodTable <- DEEBpath::getMethodTable(dbPath, methodTablePaths) # Expands RegEx!!

  exprList <- lapply(seq_len(nrow(methodTable)), \(i) {
    methodInfo <- methodTable[i, ]
    rlang::expr(
      DEEBcmd::initOneEstimAutoHyper(
        dbPath = !!dbPath,
        runLocal = !!runLocal,
        parallel = !!parallel,
        methodInfo = !!methodInfo)
    )
  })
  if (isSlurmAvailable()) {
    startSlurmJobToEvalExpressionList(
      expressionList = exprList,
      dbPath = dbPath,
      autoId = NULL,
      prefix = "auto0",
      timeInMinutes = 1440,
      nCpus = 1
    )
  } else {
    evalExpressionList(dbPath, exprList, parallel = FALSE)
  }
  return(invisible())
}




#' @export
checkOptimizationScores <- function(dbPath) {

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

  if (length(methodTablePaths) == 0) {
    stop("Did not find any method table name.")
  }

  methodTable <- DEEBpath::getMethodTable(dbPath, methodTablePaths)  # Expands RegEx!!

  outDir <- DEEButil::getUniqueFileName(
    prefix = "checkScores",
    timeStamp = TRUE,
    dirPath = DEEBpath::summaryDir(dbPath),
    fileExtension = "",
    fullPath = TRUE)
  outFilePath <- paste0(outDir, ".csv")

  exprList <- lapply(seq_len(nrow(methodTable)), \(i) {
    methodInfo <- methodTable[i, ]
    rlang::expr(
      DEEBeval::getStateOfHyperParmOptimizationHasScore(
        dbPath = !!dbPath,
        methodTable = !!methodInfo,
        outDir = !!outDir)
    )
  })
  concatExpr <- rlang::expr(
    DEEBcmd::concatCsv(
      dirPath = !!outDir,
      outFilePath = !!outFilePath,
      removeDir = TRUE
    ))
  if (runLocal) {
    evalExpressionList(dbPath, exprList, parallel = parallel)
    eval(concatExpr)
  } else {
    jobIds <- evalExpressionListSlurm(
      expressionList = exprList,
      dbPath = dbPath,
      autoId = NULL,
      prefix = "check",
      timeInMinutes = 10,
      nCpus = 1)
    startComp(rlang::expr_text(concatExpr), prefix="concat", timeInMinutes=10, nCpus=2, mail=TRUE, startAfterJobIds=jobIds)
  }
  return(invisible())
}


startEvalNewPerModel <- function(dbPath) {
  models <- DEEBpath::getModels(dbPath)
  exprList <- lapply(models, \(model) {
    rlang::expr(
      DEEBeval::runEvalTbl(
        !!dbPath,
        DEEBpath::getNew(!!dbPath, !!model),
        scoreFilter = NULL,
        createPlots = FALSE,
        verbose = FALSE,
        writeScoreHtml = FALSE,
        createSummary = FALSE,
        onlySummarizeScore = FALSE,
        autoId = NULL))})
  if (isSlurmAvailable()) {
    startSlurmJobToEvalExpressionList(
      expressionList = exprList,
      dbPath = dbPath,
      autoId = NULL,
      prefix = "evalnew",
      timeInMinutes = 1440,
      nCpus = 1)
  } else {
    parallel <- getUserInputYesNo("parallel?", "Yes")
    evalExpressionList(dbPath, exprList, parallel = parallel)
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
    timeInMinutes = 1440,
    mail = TRUE,
    startAfterJobIds = startAfterJobIds,
    dbPath = dbPath
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
        onlySummarizeScore = TRUE,
        autoId = !!autoId
      )
    ))

  jobId <- startComp(
    cmd,
    prefix = "DEEBeval-runEvalTbl-all",
    timeInMinutes = 1440,
    mail = FALSE,
    startAfterJobIds = startAfterJobIds,
    autoId = autoId,
    dbPath = dbPath
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
      timeInMinutes = 1440,
      mail = FALSE,
      dbPath = dbPath
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
    timeInMinutes = 1440,
    mail = TRUE,
    dbPath = dbPath
  )
}

startEvaluationMultiJob <- function(dbPath, createPlots) {
  models <- DEEBpath::getModels(dbPath)
  for (model in models) {
    startComp(
      rlang::expr_text(rlang::expr(
        DEEBeval::runEval(
          dbPath = !!dbPath,
          models = !!model,
          createPlots = !!createPlots,
          writeScoreHtml = FALSE,
          createSummary = FALSE,
          onlySummarizeScore = FALSE,
          verbose = FALSE
        )
      )),
      prefix = "evalScore",
      timeInMinutes = 1440,
      mail = FALSE,
      dbPath = dbPath
    )
  }
}

startScoresHtml <- function(dbPath) {
  models <- DEEBpath::getModels(dbPath)
  for (model in models) {
    startComp(
      rlang::expr_text(rlang::expr(
        DEEBeval::runScoreHtml(!!dbPath, !!model))),
      prefix = paste0("scoresHtml-", model),
      timeInMinutes = 1440,
      mail = FALSE,
      dbPath = dbPath)
  }
}


startPlotting <- function(dbPath) {
  models <- DEEBpath::getModels(dbPath)
  for (model in models) {
    startComp(
      rlang::expr_text(rlang::expr(
        DEEBeval::runPlotting(!!dbPath, !!model))),
      prefix = paste0("plot-", model),
      timeInMinutes = 1440,
      mail = FALSE,
      dbPath = dbPath)
  }
}


startSummary <- function(dbPath) {
  collectScores <- getUserInputYesNo(
    "collectScores (_summary/scores.csv)?",
    default = "No")
  collectHyper <- getUserInputYesNo(
    "collectHyper (_summary/<model>_<method>.csv)?",
    default = "No")
  renderSummary <- getUserInputYesNo(
    "renderSummary?",
    default = "No")
  renderHyper <- getUserInputYesNo(
    "renderHyper?",
    default = "No")
  startComp(
    rlang::expr_text(rlang::expr(
      DEEBeval::createSummary(
        !!dbPath,
        collectScores = !!collectScores,
        collectHyper = !!collectHyper,
        renderSummary = !!renderSummary,
        renderHyper = !!renderHyper))),
    prefix = "DEEBeval-summary",
    timeInMinutes = 1440,
    nCpus = 4,
    mail = TRUE,
    dbPath = dbPath)
}


startOverall <- function(dbPath) {
  startComp(
    rlang::expr_text(rlang::expr(
      DEEBeval::createOverall(!!dbPath))),
    prefix = "Overall",
    timeInMinutes = 1440,
    mail = TRUE,
    dbPath = dbPath)
}


startCollectInfo <- function(dbPath) {
  startComp(
    rlang::expr_text(rlang::expr(
      DEEBeval::writeInfo(!!dbPath))),
    prefix = "CollTimeInfo",
    timeInMinutes = 1440,
    nCpus = 4,
    mail = TRUE,
    dbPath = dbPath)
}


startGenCube <- function(dbPath, cubeId = NULL, startAfterJobIds = NULL, methodTable = NULL, autoId = NULL) {

  if (hasValue(methodTable)) {
    filePath <- DEEButil::getUniqueFileName(
      prefix = "methodsTable",
      dirPath = DEEBpath::autoIdDir(dbPath, autoId),
      fileExtension = ".csv",
      fullPath = TRUE)

    methodTable <- # DEEBpath::getMethodTable() reads this later and expands regex for mode and obs
      methodTable |>
      dplyr::mutate(
        model = paste0("^", .data$model, "$"),
        obs = paste0("^", .data$obs, "$"))
    readr::write_csv(methodTable, filePath)

  } else {
    filePath <- NULL
  }
  jobId <- startComp(
    rlang::expr_text(rlang::expr(
      DEEBeval::generateBestHyperCube(!!dbPath, !!filePath, !!autoId, cubeId=!!cubeId))),
    prefix = "DEEBeval-genCube",
    timeInMinutes = 1440,
    mail = FALSE,
    startAfterJobIds = startAfterJobIds,
    autoId = autoId,
    dbPath = dbPath)
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
      "nonCsv" = "remove method dirs without csv files",
      "methods" = "remove methods with RegEx",
      "eval" = "remove eval"))
  switch(
    choice,
    empty = DEEBpath::removeEmptyMethodFolders(dbPath, FALSE),
    nonCsv = DEEBpath::removeEmptyMethodFolders(dbPath, TRUE),
    log = cleanLogDir(dbPath),
    methods = startCleanMethods(dbPath),
    eval = startCleanEval(dbPath),
    stop("Choice not implemented."))
}


startCleanEval <- function(dbPath) {
  modelsAll <- DEEBpath::getModels(dbPath)
  models <- getUserInput(
    "Choose model(s)",
    modelsAll,
    multi = TRUE,
    default = "all")
  for (model in models) {
    paths <- DEEBpath::getPaths(dbPath, model)
    unlink(paths$eval, recursive=TRUE)
  }
}


startCleanMethods <- function(dbPath) {
  modelsAll <- DEEBpath::getModels(dbPath)
  models <- getUserInput(
    "Choose model(s)",
    modelsAll,
    multi = TRUE,
    default = "all")
  cat("Type RegEx for methods to delete:\n")
  regex <- getLine()
  cat("RegEx:", regex, "\n")
  methodsTable <- DEEBpath::getMethods(dbPath, models, regex)
  cat("Following methods will be deleted:\n")
  cat(paste0(methodsTable$model, ": ", methodsTable$method, collapse="\n"), "\n")
  n <- nrow(methodsTable)
  cat("Will delete", n, "methods.\n")
  readyToStart <- getUserInputYesNo(
    "Ready to start?",
    default = "Yes")
  if (readyToStart) {
    fails <- 0
    for (i in seq_len(n)) {
      cat(i, ",", sep="")
      res <- unlink(methodsTable$path[[i]], recursive=TRUE, force=TRUE, expand=FALSE)
      fails <- fails + res
      paths <- DEEBpath::getPaths(dbPath, methodsTable$model[[i]])
      files <- list.files(paths$eval, paste0("^task\\d{2}", methodsTable$method[[i]], "_eval.csv"), full.names=TRUE)
      file.remove(files)
    }
    cat("\nDone.\n")
    cat("There were", fails, "fails out of", n, "\n")
  }

  return(invisible())
}



startCopyRegex <- function(dbPath) {
  modelsAll <- DEEBpath::getModels(dbPath)
  models <- getUserInput(
    "Choose model(s)",
    modelsAll,
    multi = TRUE,
    default = "all")
  cat("Type RegEx for methods to copy:\n")
  regex <- getLine()
  cat("RegEx:", regex, "\n")
  methodsTable <- DEEBpath::getMethods(dbPath, models, regex)
  cat("Following methods will be copied:\n")
  cat(paste0(methodsTable$model, ": ", methodsTable$method, collapse="\n"), "\n")
  n <- nrow(methodsTable)
  cat("Will copy", n, "methods.\n")
  cat("Choose target DEEB DB:\n")
  toDbPath <- getUserInputDeebDb(dbPath)
  cat("Chose:", toDbPath, "\n")
  readyToStart <- getUserInputYesNo(
    "Ready to start?",
    default = "Yes")
  if (readyToStart) {
    successes <- 0
    for (i in seq_len(n)) {
      cat(i, ",", sep="")
      targetPaths <- DEEBpath::getPaths(toDbPath, methodsTable$model[[i]])
      dir.create(targetPaths$esti, recursive=TRUE, showWarnings=FALSE)
      res <- file.copy(methodsTable$path[[i]], targetPaths$esti, recursive=TRUE, overwrite=TRUE)
      successes <- successes + res
    }
    cat("\nDone.\n")
    cat("There were", successes, "successes out of", n, "\n")
  }

  return(invisible())
}

