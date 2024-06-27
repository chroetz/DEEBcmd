#' @export
concatCsv <- function(dirPath, outFilePath, pattern = "\\.csv$", removeDir=FALSE) {
  filePaths <- list.files(dirPath, pattern = pattern, full.names = TRUE)
  data <-
    lapply(filePaths, readr::read_csv, col_types = readr::cols()) |>
    dplyr::bind_rows()
  outFileDir <- dirname(outFilePath)
  if (!dir.exists(outFileDir)) dir.create(outFileDir, recursive=TRUE)
  readr::write_csv(data, outFilePath)
  if (removeDir) unlink(dirPath, recursive = TRUE)
}
