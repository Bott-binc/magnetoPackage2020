#' Tiff File Check
#'
#' checks to see if the last part of a file name is .tif or .tiff
#'
#' @param character in the form something.tif
#' @return bool of TRUE or FALSE

.is_tiff <- function(character){
  lastStringInSplit <- strsplit(character, "")
  lenStrSp = length(lastStringInSplit[[1]])
  fileType <- lastStringInSplit[[1]][as.integer(lenStrSp - 3):lenStrSp]
  fileTypeOneString = paste0(fileType[1], fileType[2], fileType[3], fileType[4])
  if (fileTypeOneString == ".tif" | fileTypeOneString == "tiff") {
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}


#' Null Argument Checking
#'
#' Checks a parameter for a null argument
#' @param  parameter what you would like to check if is null
#' @param parameterName Allows the error message to be in context
#' @return stop if is null

.null <- function(parameter, parameterName){
  if (is.null(parameter)) {
    return(stop(paste0("Need to specify ", parameterName)))
  }

}

#' Empty File Checking
#'
#' Checks a given file path with name to see if the file is empty 0b
#'
#' @param filePath The path for the directory where the file is located
#' @param fileName The name of the file located in the path directory
#' @return a bool of TRUE or FALSE
#' @export
not_empty_file <- function(filePath, fileName){
  if (is.na(filePath) ||  is.na(fileName)) {
    return(stop("missing filePath or fileName"))
  }
  if (is.na(as.character(file.info(paste0(filePath, fileName))[9]))){
    return(stop(paste0("The file ", filePath, fileName, " doesn't exist")))
  }
  if (as.numeric(file.info(paste0(filePath,fileName))[1]) != 0) {
    return(notEmpty = TRUE)
  }
  else {
    return(notEmpty = FALSE)
  }
}



#' Horizontal Image Checking
#'
#' Checks to see if the image is vertical or horizontal, if vertical, makes horizontal
#'
#' @param matrix The matrix of an imported tiff,png,etc...
#' @return The matrix horizontally
.horizontal_image_check <- function(matrix){
  ncol_matrix <- ncol(matrix)
  nrow_matrix <- nrow(matrix)

  if (ncol_matrix < nrow_matrix) {
    retVal <- apply(matrix,1,rev)
  }
  else {
    retVal <- matrix
  }
  return(retVal)
}



#Array Changes

#For class array, scales the array to aid in digitization
#NOTE: if > ,,1 exists, will not be returned to the user

#@param array Any class arrays

#@return edited array
# .array_class_edit <- function(array){
#   if (class(array) == "array") {
#     array <- array[,,1]
#     array <- 1/array
#     if (min(array) == 1) {
#       array <- array - 1
#       array <- array/max(array)
#     }
#     return(array)
#
#   }
#   else{
#     return(array)  # This is the case of no array
#   }
# }



#This is for later with the full function (with for loops so the function doesn't crash)
# tryCatchError <- function(aFunction){
#   running <- tryCatch(aFunction, error = function(e) e)
#   if (inherits(running, "error")) {
#     return(running)
#   }
#   else {
#     return(running)
#   }
# }



#' Triple Trace Checking
#'
#' Using find_peaks, checks for three traces at the bottom with the same length
#'
#' @param imageMatrix Horizontal image processed
#' @param minDistance The min distance aloud between found peaks
#' @param percentFromEdge the distance alous from the edge of the image
#' @param threshold how close the last three peaks have to be in height +-
#'
#' @return TRUE or FALSE for finding a triple or not
.triple_check <- function(imageMatrix, minDistance = 50, percentFromEdge = 2, threshold = 200){
  sums <- rowSums(imageMatrix)
  tripleCheck <- find_peaks(sums, minDistance = minDistance, maxPeakNumber = 6,
                            percentFromEdge = percentFromEdge, plots = FALSE)

  if (length(tripleCheck$Index) == 6) { # possible a triple so we check weather
    #the timing peaks are close together in heights
    #this can be an indication that there are possibly three traces on the image
    if (tripleCheck$Height[5] - threshold <= tripleCheck$Height[4] &
        tripleCheck$Height[4] <= tripleCheck$Height[5] + threshold &
        tripleCheck$Height[5] - threshold <= tripleCheck$Height[6] &
        tripleCheck$Height[6] <= tripleCheck$Height[5] + threshold) {
      return(TRUE)
    }
    else {
      return(FALSE)
    }
  }
}
