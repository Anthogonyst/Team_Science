

#' Downloads a model curated by the package owner or their affiliates (license: CC-BY-SA-NC)
#' @param modelname The name of the model to download from udpipe repo
#' @examples
#' \dontrun{
#' DownloadUdpipeModel("english-gum")
#' list.files("data/models/", "english-gum.*")
#' 
#' }
#' @author Anthogonyst
#' @export
DownloadUdpipeModel <- function(modelname = "english-ewt") {
  output = udpipe::udpipe_download_model(modelname[[1]]) %>%
    { .[[2]] = basename(.[[2]]) ; . }
  
  copySuccess = file.copy(output[1, 2], "data/models/", overwrite = FALSE)
  file.remove(output[1, 2])
  
  if (! copySuccess) {
    warning("Model already exists.")
  } else {
    utils::download.file(
      paste0(dirname(output[1, 3]), "/LICENSE"),
      file.path(paste0("data/models/LICENSE-", output[1, 2])),
      mode = "wb"
    )
  }
  
  output
}

#' Cleans the mthml files inside the specified /data/ folder
#' @param dataFolder The name of the model to load from data/models
#' @examples
#' \dontrun{
#' DownloadUdpipeModel("english-ewt")
#' 
#' model = LoadUdpipeModel("english-ewt")
#' 
#' }
#' @author Anthogonyst
#' @export
LoadUdpipeModel <- function(modelname = "english-ewt") {
  models = list.files("data/models/", pattern = paste0(modelname, ".*\\.udpipe$"))
  
  if (length(models) > 1) {
    warning("Multiple models available, maybe use a more specific name?")
  }
  
  message(paste("Using model:", models[1]))
  udpipe::udpipe_load_model(models[1])
}