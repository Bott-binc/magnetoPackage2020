#'Importing Tiff Files
#'
#'Reads tiff also checks for if the image is a .tif or .tiff using .is_tiff()
#'
#'
#'@param fileName The image name
#'@param fileLoc the path from ~/ to the dir where the file is
#'@export


tiff_import <- function(fileName,fileLoc){
  if (!is.character(fileName) || length(fileName) != 1) {
    return(stop("fileName must be a single character"))
  }
  if (!is.character(fileLoc) || length(fileLoc) != 1) {
    return(stop("fileLoc must be a single character"))
  }
  .null(fileName, "fileName")
  .null(fileLoc, "fileLoc")

  tiffCheck <- .is_tiff(fileName)
  notEmpty <- not_empty_file(filePath = fileLoc, fileName = fileName)
  if (isTRUE(tiffCheck) & isTRUE(notEmpty)) {
    return(tiff::readTIFF(paste0(fileLoc,"/",fileName)))
  }
  else if(!isTRUE(tiffCheck)){
    return(stop(paste0("file", fileName, " is not a .tif or .tiff" )))
  }
  else if(!isTRUE(notEmpty)){
    return(stop(paste0("file", fileName, " is 0b/Empty")))
  }

}


#'Checking For Bright Images
#'
#'Using a logistic regression to find coefficients, uses the result to compute a probability
#'of the imageMatrix being #'bright (392 observations) with McFadden Pseudo- $R^2$ Value 0.2573778
#'
#'@param imageMatrix  imageMatrix points representing the brightness of that pixel
#'@param cutoffProbability The probability cut off for the decision of an imageMatrix being bright
#'@param beta0 The intercept of a logistic regression default is for magnetograms
#'@param beta1 The slope of the logistic regression default is for magnetograms
#'@param NADefault The default value set to NA's in the matrix
#'@return The decision of the image being bright (based off of the cutoffProbability)
#'@export

bright <- function(imageMatrix, beta0 = -2.774327, beta1 = 51.91687, cutoffProbability = 0.5, NADefault = 0){
  if (!is.matrix(imageMatrix)) {
    return(stop("imageMatrix must be a matrix"))
  }
  na <- which(is.na(imageMatrix))
  if (length(na) > 0) {
    warning(paste0("NA's found in the matrix.. setting them to ", NADefault))
    imageMatrix[na] <- NADefault
  }
  if (length(which(imageMatrix <= 1 & imageMatrix >= 0)) != length(imageMatrix)) {
    warning("Shouldnt be pixels with brightness above 1...scaling by largest value and
            taking abs value")
    imageMatrix <- abs(imageMatrix)/max(abs(imageMatrix))
  }


  .null(imageMatrix, "imageMatrix")

  aboveLen <- length(which(imageMatrix >= 0.80))
  totalLen <- length(imageMatrix)
  standardizedLen <- aboveLen/totalLen
  decision <- exp(beta0 + beta1 * standardizedLen)/(1 + exp(beta0 + beta1 * standardizedLen))

  if (decision >= cutoffProbability) {
    bright = TRUE
  }
  else {
    bright = FALSE
  }
  return(bright)
}




#' Finding Peaks for Image
#'
#' Finds peaks in the row sums of an image matrix, used for finding starts of lines
#' that are horizontal
#'
#' @param rowSums of the imported matrix from original picture
#' @param minDistance Minimum distance aloud between found peaks
#' @param maxPeakNumber Maximum peaks you are trying to find
#' @param percentFromEdge Percentage of the picture that you would like removed from edge
#' due to an over exposed picture or flares in scanning
#' if percentageLeftSide is unspecified uses percentage for both i.e left = 1 - right
#' @param minPeakHeight minimum peak height required for a peak to be counted
#' Default is 5perc of the max(rowSums)
#' @param plots Plots the peaks on rowSums
#' @param StartEndplotLine If you want to see lines at start and ends of each peaks
#' (starts are green, ends are blue)
#'
#' @return data frame of peaks, peak heights, starts and ends for each peak
#' @export
find_peaks <- function(rowSums, minDistance, maxPeakNumber, percentFromEdge,
                       minPeakHeight = (0.05*max(rowSums)), plots = TRUE, StartEndplotLine = TRUE) {
  if(!is.vector(rowSums, mode = "numeric")) {
    return(stop("rowSums must be a vector"))
  }
  reticulate::source_python("inst/python/finding_peakspy.py")
  peaks <- finding_peakspy(rowSums, minPeakHeight, minDistance)
  peaks[[1]] <- peaks[[1]] + 1 # python index correction
  peakInfo = data.frame(Index = peaks[[1]], Height = peaks[[2]]$peak_heights)
  if (dim(peakInfo)[1] == 0) {
    return(stop("No Peaks Found..."))
  }


  peaksNoEdge <- .edge_peaks_rm(peakInfo, rowSums, percentFromEdge)
  foundPeaks <- .highest_peaks(peaksNoEdge, maxPeaksAllowed = maxPeakNumber)

  ret <- .finding_Peak_Start_Ends(foundPeaks, rowSums)
  if(plots == TRUE){
  .plot_peaks(ret, rowSums)
  }
  return(ret)

}


#'Searching For Keyword
#'
#' Finds all files that contain a specific string, in this case for finding files
#' that contain a specific year. Default is to also look in symlinks for files, if a dir is not accessable
#' will return an warning to the user
#'
#' @param path Where you would like the function to look.
#' @param keyword The word that you are trying to find in file names.
#' @return  Vector with all paths for each item found with the keyword contained in it.
#' @export
find_paths_for_keyword <- function(path = "~/", keyword){
  word = paste0("*", keyword, "*")
  spath = vector()
  symlinks <- as.character(fs::dir_ls(path = path , recurse = TRUE, type = "symlink"))
  if (!identical(symlinks, character(0))) { #The character 0 is what is displayed if no symlinks show up
    symL <- length(symlinks)
    for (i in 1:symL) {
      spath <- c(spath, as.character(fs::dir_ls(path = symlinks[i], glob = word, recurse = TRUE, type = "file", fail = FALSE)))
    }

  }
  pathWithKeyword  <- fs::dir_ls(path = path, glob = word, recurse = TRUE, type = "any")
  allPathWithKeyword <- c(spath, pathWithKeyword)
  if (length(allPathWithKeyword) == 0) {
    return(stop("No files found with that name"))
  }
  else{
    return(as.character(allPathWithKeyword))
  }
}



# TIS_bright_findPeaks <- function(fileName, fileLoc, beta0 = -2.774327, beta1 = 51.91687, cutoffProbability = 0.5,
#                                   NADefault = 0, minAloudDistance = 100, maxPeakNumber = 4, percentFromEdge = 10,
#                                   minPeakHeight = NA, withPlots = TRUE, StartEndPlotLine = TRUE){
#
#   if (brightDecision == TRUE){
#     .not_bright_image(imageMatrix = imageMatrix)
#     distance <- 100
#   }
#   else{
#     .for_bright_image(imageMatrix = imageMatrix)
#     distance <- 50
#   }
#   col_sums <- colSums(gaussImage)
#   rowSums <- rowSums(gaussImage)^2
#
# }

