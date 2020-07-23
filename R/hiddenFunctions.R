
#' Maximum/Minimum distance(percentage) from Edge
#'
#' Takes a percentage and calculates the cutoff value for a
#' function to look for something.  Used for find_peaks
#'
#' @param rowSums The sum of the rows of the matrix
#' @param percentage Percentage of the picture that you would like removed from edge
#'  if percentageLeftSide is unspecified uses percentage for both i.e left = 1 - right
#' @param percentageLeftSide the left percentage that you want to remove(default = NULL)
#' @return vector of (rightSideDistance, leftSideDistance)

.allowed_edge_distance <- function(rowSums, percentage, percentageLeftSide = NULL){
  if (percentage > 100 ) {
    return(stop("Can't have a percentage over 100 "))
  }
  if (!is.null(percentageLeftSide)) {
    if( percentageLeftSide  > 100) {
      return(stop("Can't have a percentage over 100 "))
    }
  }
  if (percentage & !is.null(percentageLeftSide)) {
    if ((percentage + percentageLeftSide) >= 100) {
      return(stop("Can't have percentage plus percentageLeftSide be greater then or equal to 100.. no area left"))
    }
  }
  right <- percentage/100
  if (is.null(percentageLeftSide)) {
    left <- right
    rightDist <- round((1 - right)*length(rowSums))
    leftDist <- round((left)*length(rowSums))
  }
  else{
    rightDistance <- right
    leftDistance <- percentageLeftSide/100

    rightDist <- round(rightDistance*length(rowSums))
    leftDist <- round((leftDistance)*length(rowSums))
  }


  ret <- data.frame(leftDist = leftDist,rightDist = rightDist)
  return(ret)
}



#'Scanning Flair Around Outside of Image
#'
#'Checks the list of peaks for any flairs in the photos at start and end, they are removed
#'
#'@param FindingPeaksdf the resulting data frame from finding_peaks
#'(if called separately make sure in correct form nx2: Index column and Height column)
#'@param rowSums  of the imported matrix from original picture
#'@param percentEdge passed into .allowed_edge_distance
#'@param percentEdgeForLeft is user needs to specify a different percentage for left and right side cutoff
#'will make percentEdge for the right side and percentEdgeForLeft for the left side of image
#'@return void
.edge_peaks_rm <- function(FindingPeaksdf, rowSums, percentEdge, percentEdgeForLeft = NULL){
  if (!is.vector(rowSums) | !is.data.frame(FindingPeaksdf)) {
    return(stop("rowSums(vector) and FindingPeaksdf(dataframe) must be in the correct form"))
  }
  if (length(rowSums) < max(FindingPeaksdf$Index)){
    return(stop("rowSums should be at least the same length as your largest indexed peak"))
  }
  rm <- vector()
  dist <- .allowed_edge_distance(rowSums, percentEdge, percentEdgeForLeft)
  rightSide <- dist$rightDist
  leftSide <- dist$leftDist
  for (k in 1:length(FindingPeaksdf$Index)) {

    if (FindingPeaksdf$Index[k] >= rightSide) {

      rm <- c(rm, FindingPeaksdf$Index[k])

    }
    else if (FindingPeaksdf$Index[k] <= leftSide) {

      rm <- c(rm, FindingPeaksdf$Index[k])

    }
  }
  matchIndex = vector()
  for (i in 1:length(FindingPeaksdf$Index)) {
    if (!is.na(match(FindingPeaksdf$Index[i], rm))) {
      matchIndex <- c(matchIndex, i)

    }
  }
  if (is.logical(matchIndex)) {
    retHeights <- FindingPeaksdf$Height
    retIndex <- FindingPeaksdf$Index
  }
  else{
    retHeights <- FindingPeaksdf$Height[-matchIndex]
    retIndex <- FindingPeaksdf$Index[-matchIndex]
  }
  FindingPeaksdf <- data.frame(Index = retIndex, Height = retHeights)
  return(FindingPeaksdf)
}


#' Highest Peaks
#'
#' Finds the highest peaks up to the maximum number of peaks allowed(set by user on call)
#'
#' @param FindingPeaksdf the resulting data frame from finding_peaks
#'(if called separately make sure in correct form nx2: Index column and Height column)
#' @param maxPeaksAllowed The maximum number of lines on a picture you would like to find
#'
#' @return The highest peaks up to the max number allowed
.highest_peaks <- function(FindingPeaksdf, maxPeaksAllowed){
  if (!is.data.frame(FindingPeaksdf)) {
    return(stop("FindingPeaksdf(dataframe) must be a dataframe"))
  }
  if (maxPeaksAllowed <= 0){
    return(stop("MaxPeaksAllowed must be more then 0"))
  }
  highestPeaksIndex <- vector()
  highestPeaksHeight <- vector()
  if (length(FindingPeaksdf$Index) > maxPeaksAllowed) {
    sorting <- sort(FindingPeaksdf$Height, decreasing = TRUE, index.return = TRUE)
    for (j in 1:maxPeaksAllowed) {
      highestPeaksIndex <- c(highestPeaksIndex, FindingPeaksdf$Index[sorting$ix[j]])#ix is a product from sort
      highestPeaksHeight <- c(highestPeaksHeight, FindingPeaksdf$Height[sorting$ix[j]])
    }
  }
  if (length(FindingPeaksdf$Index) <= maxPeaksAllowed) {
    highestPeaksIndex <- FindingPeaksdf$Index
    highestPeaksHeight <- FindingPeaksdf$Height
  }
  highestPeaks <- data.frame(highestPeaksIndex, highestPeaksHeight)
  names(highestPeaks) <-  c("Index", "Height")
  return(highestPeaks)
}


#' Distance to Closest Peak or Border
#'
#' Finds the closest peak(or border) index for a given peak on either side
#'
#' @param FindingPeaksdf the resulting data frame from finding_peaks
#'(if called separately make sure in correct form nx2: Index column and Height column)
#' @param rowSums of the imported matrix from original picture
#' @param peakIndex the index of FindingPeaksdf that contains the peak you want to look at
#'
#' @return dataframe  1x2 leftDist, rightDist for columns
.finding_distance_to_peaks <- function(FindingPeaksdf, rowSums, peakIndex){
  FindingPeaksIndex <- sort(FindingPeaksdf$Index, decreasing = FALSE)
  currentIndex <- which(FindingPeaksIndex == FindingPeaksdf$Index[peakIndex])
  if (peakIndex > length(FindingPeaksIndex)) {
    return(stop("Less peaks then peak index"))
  }
  if (length(FindingPeaksIndex) == 1) {
    distanceToRight <- length(rowSums) - FindingPeaksIndex[currentIndex]
    distanceToLeft <- FindingPeaksIndex[currentIndex] - 1
  }
  else if (currentIndex > 1 & currentIndex < length(FindingPeaksIndex)) {
    distanceToRight <- FindingPeaksIndex[currentIndex + 1] - FindingPeaksIndex[currentIndex]
    distanceToLeft <- FindingPeaksIndex[currentIndex] - FindingPeaksIndex[currentIndex - 1]
  }
  else if (currentIndex == 1) {
    distanceToRight <- FindingPeaksIndex[currentIndex + 1] - FindingPeaksIndex[currentIndex]
    distanceToLeft <- FindingPeaksIndex[currentIndex] - 1
  }
  else if (currentIndex == length(FindingPeaksIndex)) {
    distanceToRight <- length(rowSums) - FindingPeaksIndex[currentIndex]
    distanceToLeft <- FindingPeaksIndex[currentIndex] - FindingPeaksIndex[currentIndex - 1]
  }
  Distance <- data.frame(leftDist = distanceToLeft, rightDist = distanceToRight)
  return(Distance)
}




#' Finding starting point and ending point for each peak
#'
#' @param FindingPeaksdf the resulting data frame from finding_peaks
#'(if called separately make sure in correct form nx2: Index column and Height column)
#' @param rowSums of the imported matrix from original picture
#'
#' @return FindingPeaksdf along with the starting and ending points of each peak
.finding_Peak_Start_Ends <- function(FindingPeaksdf, rowSums){
  peakStart <- vector()
  peakEnd <- vector()
  for (k in 1:length(FindingPeaksdf$Index)) {
    height <- FindingPeaksdf$Height[k]
    tempHeightLeft <- height
    tempHeightRight <- height
    closestPeaks <- .finding_distance_to_peaks(FindingPeaksdf, rowSums, peakIndex = k)
    index = min(closestPeaks$leftDist, closestPeaks$rightDist)

    for (j in 1:index) {
      rightSide <- FindingPeaksdf$Index[k] + j
      leftSide <- FindingPeaksdf$Index[k] - j
      if (j == index){
        peakStart[k] <- leftSide
        peakEnd[k] <- rightSide
        break
      }
      if (rowSums[leftSide] <= tempHeightLeft &
          rowSums[rightSide] <= tempHeightRight) {
        tempHeightLeft <- rowSums[leftSide]
        tempHeightRight <- rowSums[rightSide]
      }
      else if (rowSums[leftSide + 1] <= tempHeightLeft &
               rowSums[rightSide] <= tempHeightRight) {

        tempHeightLeft <- rowSums[leftSide + 1]
        tempHeightRight <- rowSums[rightSide]
      }
      else if (rowSums[leftSide] <= tempHeightLeft &
              rowSums[rightSide + 1] <= tempHeightRight) {

        tempHeightLeft <- rowSums[leftSide]
        tempHeightRight <- rowSums[rightSide + 1]
      }
      else{
        peakStart[k] <- leftSide
        peakEnd[k] <- rightSide
        break
      }
    }

  }

  ret <- data.frame(FindingPeaksdf, PeakStart = round(peakStart), PeakEnd = round(peakEnd))
  return(ret)
}



#' Plotting Peaks from find_peaks
#'
#' This can be used alone from find_peaks, ensure that you have the correct df form.
#' starting are green lines, ending are blue lines
#'
#' @param FindPeaksdf usually found with find_peaks nx4: Index, Height, PeakStart, PeakEnd
#' @param rowSums of the imported matrix from original picture
#' @param StartEndLine bool, if you want a plotted starting and ending line for each peak
#'
#' @return void
.plot_peaks <- function(FindPeaksdf, rowSums, StartEndLine = TRUE){
  graphics::plot(rowSums, type = "l")
  graphics::points(FindPeaksdf$Index, FindPeaksdf$Height, pch = 20, col = "red")
  if (StartEndLine == TRUE) {
    graphics::abline(v = FindPeaksdf$PeakStart, col = "green")
    graphics::abline(v = FindPeaksdf$PeakEnd, col = "blue")
  }
}



#' Non bright image scaling with gaussian deconvolution
#'
#' @param imageMatrix An imported image, can be imported with tiff_import()
#' @param sig Significance parameter, used in dnorm as the final quantile
#' @param kern.trunc Truncated value for the kernel
#' @param nw A positive double-precision number, the time-bandwidth parameter
#'
#' @return Gaussian matrix scaled for bright
.not_bright_image <- function(imageMatrix, sig = 10, kern.trunc = 0.05, nw = 3){
  if (length(imageMatrix) < 20) {
    return(stop("the imageMatrix must be larger then 20 to run gaussian decon"))
  }
  imageMatrix[is.na(imageMatrix)] <- 0
  gaussImageMatrix <- suppressWarnings(abs(t( apply(imageMatrix, MARGIN = 1, FUN = deconv_gauss, sig = 10, kern.trunc = 0.05, nw = 3 ) )))


  imageMatrix[imageMatrix < (1 - mean(imageMatrix,na.rm = TRUE))] <- 0
  imageMatrix[imageMatrix > 0] <- 1

  gaussImageMatrix[0:floor((1/62)*nrow(imageMatrix))] <- mean(gaussImageMatrix)
  gaussImageMatrix[(nrow(gaussImageMatrix) - 0.01*nrow(imageMatrix)):(nrow(gaussImageMatrix)),] <- mean(gaussImageMatrix)
  gaussImageMatrix[gaussImageMatrix < 0] <- 0
  retdf <- list(imageMatrix = imageMatrix, gaussImageMatrix = gaussImageMatrix)
  return(retdf)
}



#' Bright image scaling to gaussian
#'
#' @param imageMatrix An imported image, can be imported with tiff_import()
#' @param sig Significance parameter, used in dnorm as the final quantile
#' @param kern.trunc Truncated value for the kernel
#' @param nw A positive double-precision number, the time-bandwidth parameter
#' @param brightQuantile the quantile that will be a cutoff for image pixels set to 0
#'
#' @return Gaussian matrix scaled for bright
.for_bright_image <- function(imageMatrix, sig = 10, kern.trunc = 0.05, nw = 3, brightQuantile = 0.95){
  if (length(imageMatrix) < 20) {
    return(stop("the imageMatrix must be larger then 20 to run gaussian decon"))
  }
  imageMatrix[is.na(imageMatrix)] <- 0
  imageMatrix[imageMatrix < (stats::quantile(imageMatrix, brightQuantile))] <- 0
  imageMatrix[imageMatrix > 0] <- 1
  gaussImageMatrix <-  suppressWarnings(abs(t(apply(imageMatrix, MARGIN = 1, FUN = deconv_gauss, sig = 10, kern.trunc = 0.05, nw = 3 ))))

  gaussImageMatrix[0:floor((1/62)*nrow(imageMatrix))] <- mean(gaussImageMatrix)
  gaussImageMatrix[(nrow(gaussImageMatrix) - 0.01*nrow(imageMatrix)):nrow(gaussImageMatrix),] <- mean(gaussImageMatrix)
  gaussImageMatrix[gaussImageMatrix < 0] <- 0
  retdf <- list(imageMatrix = imageMatrix, gaussImageMatrix = gaussImageMatrix)
  return(retdf)
}


# rough_bounds <- function(imageMatrix, FindPeaksdf){
#
#   startrow <- round(PossiblePeaks$firstPeak[START_INDEX]) - 50
#   upperbound <- vector(length = ncol(image))
#   for (c in 1:ncol(image)) {
#     if (image[startrow, c] == 0) {
#       upperbound[c] <- startrow
#       next
#     } else{
#       #Search above and below
#       for (i in 1:10) {
#         above <- startrow + i
#         below <- startrow - i
#         if (image[above, c] == 0) {
#           upperbound[c] <- above
#           startrow <- above
#           break
#         }
#         if (image[below, c] == 0) {
#           upperbound[c] <- below
#           startrow <- below
#           break
#         }
#       }
#
#     }
#   }
#   upperbound <- abs(upperbound - nrow(image))
# }
