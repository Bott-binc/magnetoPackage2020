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
  else if (!isTRUE(tiffCheck)) {
    return(stop(paste0("file", fileName, " is not a .tif or .tiff" )))
  }
  else if (!isTRUE(notEmpty)) {
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
#' @param percentEdgeForLeft the left percentage that you want to remove(default = NULL)
#' @param minPeakHeight minimum peak height required for a peak to be counted
#' Default is 5perc of the max(rowSums)
#' @param plots Plots the peaks on rowSums
#' @param StartEndplotLine If you want to see lines at start and ends of each peaks
#' (starts are green, ends are blue)
#'
#' @return data frame of peaks, peak heights, starts and ends for each peak
#' @export
find_peaks <- function(rowSums, minDistance, maxPeakNumber, percentFromEdge, percentEdgeForLeft = NULL,
                       minPeakHeight = (0.05*max(rowSums)), plots = TRUE, StartEndplotLine = TRUE) {
  if (!is.vector(rowSums, mode = "numeric")) {
    return(stop("rowSums must be a vector"))
  }
  #python code
  scipy <- reticulate::import("scipy")
  peaks <- scipy$signal$find_peaks(x = rowSums, height = minPeakHeight, distance = minDistance)
  #end of python code
  #reticulate::source_python(file = system.file("py", package = packageName()))
  #peaks <- finding_peakspy(rowSums, minPeakHeight, minDistance)
  peaks[[1]] <- peaks[[1]] + 1 # python index correction
  peakInfo = data.frame(Index = peaks[[1]], Height = peaks[[2]]$peak_heights)
  if (dim(peakInfo)[1] == 0) {
    return(stop("No Peaks Found..."))
  }

  peaksNoEdge <- .edge_peaks_rm(peakInfo, rowSums, percentFromEdge, percentEdgeForLeft = percentEdgeForLeft)
  foundPeaks <- .highest_peaks(peaksNoEdge, maxPeaksAllowed = maxPeakNumber)


  ret <- .finding_Peak_Start_Ends(foundPeaks, rowSums)
  if (plots == TRUE) {
  .plot_peaks(ret, rowSums, StartEndLine = StartEndplotLine)
  }
  return(ret[order(ret$Index),])

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



# TIS_RoughBounds <- function(fileName, fileLoc, Brightbeta0 = -2.774327, Brightbeta1 = 51.91687, cutoffProbability = 0.5,
#                                   NADefault = 0, minAllowedDistance = 100, maxPeakNumber = 4, percentFromEdge = 10,
#                                   minPeakHeightperc = 0.05, brightQuantile = 0.95, withPlots = TRUE, StartEndPlotLine = TRUE) {
#   imageMatrix <- tiff_import(fileName, fileLoc)
#   .horizontal_image_check(imageMatrix)
#   brightDecision <- bright(imageMatrix, beta0 = Brightbeta0, beta1 = Brightbeta1, cutoffProbability, NADefault)
#
#   if (brightDecision == TRUE) {
#     imagedf <- .not_bright_image(imageMatrix = imageMatrix)
#     distance <- minAllowedDistance
#   }
#   else{#non bright images
#     imagedf <- .for_bright_image(imageMatrix = imageMatrix, brightQuantile)
#     distance <- minAllowedDistance/2
#   }
#   gaussImageMatrix <- abs(t( apply(imageMatrix, MARGIN = 1, FUN = deconv_gauss, sig = 10, kern.trunc = 0.05, nw = 3 )))
#   col_sums <- colSums(gaussImageMatrix)
#   rowSums <- rowSums(gaussImageMatrix)^2
#   minHeightPeaks <- minPeakHeightperc*max(rowSums)
#   peaks <- find_peaks(rowSums = rowSums, minDistance = distance, maxPeakNumber = maxPeakNumber, percentFromEdge = percentFromEdge,
#                       minPeakHeight = minHeightPeaks, plots = withPlots, StartEndplotLine = StartEndPlotLine)
#
#
#
# }

#' Title
#'
#' @param imageName The name of the file
#' @param file_loc the path from ~/ to the dir where the file is
#' @param trimAmountTop Number of pixels off of top of image (usually for common flares)
#' @param trimAmountBottom Number of pixels off of bottom of image
#' @param beta0 The intercept of a logistic regression default is for magnetograms
#' @param beta1 The slope of the logistic regression default is for magnetograms
#' @param cutoffProbability The probability cut off for the decision of an imageMatrix being bright
#' @param NADefault The default value set to NA's found in the matrix
#' @param FilterBright Vector specifying the dimensions of the kernel,
#' which will be used to perform either delation or erosion, such as c(13,13)
#' @param FilterNonBright Vector specifying the dimensions of the kernel,
#' which will be used to perform either delation or erosion, such as c(8,8)
#' @param methodBright one of 'delation'(adds to image, making brights brighter), 'erosion' (subtracts from image brights darker)
#' @param methodNonBright one of 'delation'(adds to image, making brights brighter), 'erosion' (subtracts from image brights darker)
#' @param thresholdBright should be between 0 and 1 for normalized images Default = 0.8
#' @param thresholdNonBright should be between 0 and 1 for normalized images Default = 0.5
#'
#' @return imageMatrix processed and trimed, in landscape orientation
#' @export
import_process_image <- function(imageName, file_loc, trimAmountTop = 100,
                                 trimAmountBottom = 50, beta0 = -2.774327,
                                 beta1 = 51.91687, cutoffProbability = 0.5,
                                 NADefault = 0, FilterBright = c(13, 13),
                                 FilterNonBright = c(8, 8),
                                 methodBright = "delation",
                                 methodNonBright = "delation",
                                 thresholdBright = 0.8,
                                 thresholdNonBright = 0.5){


  imageRAW <- tiff_import(fileName = imageName, fileLoc = file_loc)
  image <- .horizontal_image_check(imageRAW)
  imagecut <- .trim_top_bottom(image,
                               trimAmountTop = 100,
                               trimAmountBottom = 50) #takes off the usual flair spots
  imageMatrix <- .process_image(imagecut,
                                cutoffProbability = cutoffProbability,
                                NADefault = NADefault,
                                beta1 = beta1, beta0 = beta0,
                                FilterBright = FilterBright,
                                FilterNonBright = FilterNonBright,
                                methodBright = methodBright,
                                methodNonBright = methodNonBright,
                                thresholdBright = thresholdBright,
                                thresholdNonBright = thresholdNonBright) # checks bright and processes returns processed
  return(imageMatrix)
}



#' Image Blurring with Rollmean
#'
#' uses rollMean from zoo to blurr image ( used to get rid of timing gaps)
#'
#' @param imageMatrix the image matrix from import_process_image
#' @param topcut Top cutoff from .top_cut()
#' @param bottomcut Bottom cutoff from .bottom_cut()
#' @param fill Default "extend" see rollMean in zoo for details
#' @param k See rollMean() in zoo for details
#'
#' @return rolled image matrix to user
mean_roll_image <- function(imageMatrix, topcut, bottomcut, fill = "extend", k = 40){
  imageWithoutTopBottom <- imageMatrix[-c(0:topcut, bottomcut:nrow(imageMatrix)), ]
  vert <- t(imageWithoutTopBottom)
  rolledImage <- t(zoo::rollmean(vert, k = k, fill = fill))
  rolledImage[which(rolledImage != 0)] <- 1
  return(rolledImage)
}




#' Finding Envelopes for Two Traces
#'
#' @param rolledImage product of mean_roll_image()
#' @param imageMatrix ImageMatrix from import_process_image()
#' @param bottomcut from .Image_cut()
#' @param returnType Either "MatrixScaled" used if you are manipulating with imageMatrix,
#' "plottingScaled" used if you are plotting the imageMatrix with overlay of these lines
#' "rolledImageScaled" used if you are overlaying the rolled image with these lines
#' @param max_roc maximum rate of change allowed between two pixels on the line before deemed as noise
#' @param sepDist how far you want the envelope to be below the line you are tracing
#' @param maxNoise the length of creating points alowed before considered to be off of the trace
#'
#' @return list of all four envelopes
#' @export
find_envelopes <- function(rolledImage, imageMatrix, bottomcut, returnType, sepDist = 10, max_roc = 50, maxNoise = 100){
  if (returnType == "MatrixScaled") {
    topEnvelope <- bottomcut - .top_env(rolledImage = rolledImage,
                                        max_roc = max_roc,
                                        sepDist = sepDist,
                                        maxNoise = maxNoise)
    topLowerEnvelope <- bottomcut - .top_lower_env(rolledImage = rolledImage,
                                                   max_roc = max_roc,
                                                   sepDist = sepDist,
                                                   maxNoise = maxNoise)
    bottomUpperEnvelope <- bottomcut - .bottom_upper_env(rolledImage = rolledImage,
                                                         max_roc = max_roc,
                                                         sepDist = sepDist,
                                                         maxNoise = maxNoise)
    bottomEnvelope <- bottomcut - .bottom_env(rolledImage = rolledImage,
                                              max_roc = max_roc,
                                              sepDist = sepDist,
                                              maxNoise = maxNoise)
  }
  else if (returnType == "PlottingScaled") {
    topEnvelope <- nrow(imageMatrix) - bottomcut + .top_env(rolledImage = rolledImage,
                                                            max_roc = max_roc,
                                                            sepDist = sepDist,
                                                            maxNoise = maxNoise)
    topLowerEnvelope <- nrow(imageMatrix) - bottomcut + .top_lower_env(rolledImage = rolledImage,
                                                                       max_roc = max_roc,
                                                                       sepDist = sepDist,
                                                                       maxNoise = maxNoise)
    bottomUpperEnvelope <- nrow(imageMatrix) - bottomcut + .bottom_upper_env(rolledImage = rolledImage,
                                                                             max_roc = max_roc,
                                                                             sepDist = sepDist,
                                                                             maxNoise = maxNoise)
    bottomEnvelope <- nrow(imageMatrix) - bottomcut + .bottom_env(rolledImage = rolledImage,
                                                                  max_roc = max_roc,
                                                                  sepDist = sepDist,
                                                                  maxNoise = maxNoise)
  }
  else if (returnType == "rolledImageScaled") {
    topEnvelope <- .top_env(rolledImage = rolledImage,
                            max_roc = max_roc,
                            sepDist = sepDist,
                            maxNoise = maxNoise)
    topLowerEnvelope <- .top_lower_env(rolledImage = rolledImage,
                                       max_roc = max_roc,
                                       sepDist = sepDist,
                                       maxNoise = maxNoise)
    bottomUpperEnvelope <- .bottom_upper_env(rolledImage = rolledImage,
                                             max_roc = max_roc,
                                             sepDist = sepDist,
                                             maxNoise = maxNoise)
    bottomEnvelope <- .bottom_env(rolledImage = rolledImage,
                                  max_roc = max_roc,
                                  sepDist = sepDist,
                                  maxNoise = maxNoise)
  }
  else {
    stop("returnType is not correct, please look at documentation")
  }
  return(list(TopEnvelope = topEnvelope,
              TopLowerEnvelope = topLowerEnvelope,
              BottomUpperEnvelope = bottomUpperEnvelope,
              BottomEnvelope = bottomEnvelope))
}
