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
    c("hyper" = "DEEBesti: run estimations for hyper parameter optimization",
      "scan" = "DEEBeval: scan for new estimation files",
      "choose" = "DEEBeval: choose what to (re-)evaluate",
      "evalAll" = "DEEBeval: evaluate all (no plots)",
      "copyTruth" = "DEENesti: copy truth"))

  switch(
    choice,
    copyTruth = startCopyTruth(dbPath),
    hyper = interactHyper(dbPath),
    scan = interactScan(dbPath),
    choose = interactChoose(dbPath),
    evalAll = startEvaluation(dbPath),
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


interactScan <- function(dbPath) {
  cat("Scaning for new estimation files...\n")
  newEsti <- DEEBpath::getNew(dbPath)
  if (nrow(newEsti) == 0) {
    cat("No new estimation files detected.\n")
  } else {
    cat("Found", nrow(newEsti), "new estimation files. The first three are:\n")
    print(newEsti[1:min(3,nrow(newEsti)),])
  }
  choice <- getUserInput(
    "Choose what to do",
    c("new" = "evaluate new",
      "abort" = "abort"))
  if (choice != "new") return(invisible(NULL))
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

startEvaluation <- function(dbPath) {
  models <- DEEBpath::getModels(dbPath)
  startComp(
    rlang::expr_text(rlang::expr(
      DEEBeval::runEval(
        dbPath = !!dbPath,
        models = !!models,
        createPlots = FALSE,
        writeScoreHtml = TRUE,
        createSummary = TRUE,
        verbose = FALSE
      )
    )),
    prefix = "DEEBeval",
    timeInMinutes = 240,
    mail = TRUE
  )
}
