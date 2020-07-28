
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
    if (percentageLeftSide  > 100) {
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
  if (maxPeaksAllowed <= 0) {
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
  imageMatrix[is.na(imageMatrix)] <- 0
  gaussImageMatrix <- suppressWarnings(abs(t( apply(imageMatrix, MARGIN = 1, FUN = deconv_gauss, sig = 10, kern.trunc = 0.05, nw = 3 ) )))


  imageMatrix[imageMatrix < (1 - mean(imageMatrix,na.rm = TRUE))] <- 0
  imageMatrix[imageMatrix > 0] <- 1

  gaussImageMatrix[0:floor((1/62)*nrow(imageMatrix))] <- mean(gaussImageMatrix)
  gaussImageMatrix[(nrow(gaussImageMatrix) - 0.01*nrow(imageMatrix)):(nrow(gaussImageMatrix)),] <- mean(gaussImageMatrix)
  gaussImageMatrix[gaussImageMatrix < 0] <- 0
  retdf <- list(imageMatrix = imageMatrix, gaussImageMatrix = gaussImageMatrix)
  return(retdf)

  return(gaussimageMatrix)
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
  imageMatrix[is.na(imageMatrix)] <- 0
  imageMatrix[imageMatrix < (stats::quantile(imageMatrix, brightQuantile))] <- 0
  imageMatrix[imageMatrix > 0] <- 1
  gaussImageMatrix <- abs(t(apply(imageMatrix, MARGIN = 1, FUN = deconv_gauss, sig = 10, kern.trunc = 0.05, nw = 3 )))

  gaussImageMatrix[0:floor((1/20) * nrow(imageMatrix))] <- mean(gaussImageMatrix)
  gaussImageMatrix[(nrow(gaussImageMatrix) - 0.05 * nrow(imageMatrix)):nrow(gaussImageMatrix), ] <- mean(gaussImageMatrix)
  gaussImageMatrix[gaussImageMatrix < 0] <- 0
  retdf <- list(imageMatrix = imageMatrix, gaussImageMatrix = gaussImageMatrix)
  return(retdf)
}




#' Trace Starting and Ending Points
#'
#' Can also be used to remove those ranges where the trace isn't if returnMat is TRUE
#'
#' @param imageMatrix Imported image into matrix form, can use tiff_import()
#' @param cutPercentage A bound for which the start and end is never found, could be a flare
#' on the sides of the scanned image
#' @param peakThreshold Smallest difference allowed for a difference to be a possible peak
#' @param gapAllow Distance in which another index could be considered to be the same peak,
#' applicable for large peaks
#' @param returnMat bool, default is to return the image with removed start and end to the user, if FALSE
#' will return just the index of the start and end
#'
#' @return Image matrix with removed start and end, or the index of these start and ends
.get_trace_start_ends <- function(imageMatrix, cutPercentage = 1, peakThreshold = 5, gapAllow = 20, returnMat = TRUE){
  imageMatrix <- .horizontal_image_check(imageMatrix)
  processedImage <- .for_bright_image(imageMatrix) #Even if not bright image, found this to be the most consistent
  colSumsImage <- colSums(processedImage$gaussImageMatrix)
  len <- length(colSumsImage)
  diffsColSms <- abs(diff(colSumsImage))
  pkThresh <- peakThreshold/100
  possibleStartDiffs <- which(diffsColSms >= pkThresh) # how big of a difference you are looking for (potential peaks)
  #riddance of gaps between possible starts(includes the black surround on an image)
  chosenDiffs <- possibleStartDiffs[which(abs(diff(possibleStartDiffs))  <= gapAllow)]
  cutPerc <- round(cutPercentage/100*len)
  chosenDiffs <- chosenDiffs[which(chosenDiffs <= (len - cutPerc) & chosenDiffs >= cutPerc)]
  first <- chosenDiffs[1]
  newFirst <- first
  last <- chosenDiffs[length(chosenDiffs)]
  newLast <- last
  compareLeft <- colSumsImage[first]
  compareRight <- colSumsImage[last]
  middle <- len/2
  middleMean <- mean(colSumsImage[round((middle - 0.2*len)):round((middle + 0.2*len))])
  if (compareLeft > middleMean) {
    compareLeft = middleMean
  }
  if (compareRight > middleMean) {
    compareRight = middleMean
  }
  for (j in 1:round(0.2*len)) {
    if (colSumsImage[first + j] <= compareLeft + 1 & colSumsImage[first + j]  <= middleMean - 3) {
      newFirst <- first + j
    }
  }
  for (k in 1:round(0.2*len)) {
    if (colSumsImage[last - k] <= compareRight + 1 & colSumsImage[last - k]  <= middleMean - 3) {
      newLast <- last - k
    }
  }
  if (returnMat == FALSE) {
  return(list(newFirst, newLast))
  }
  else {
    ImageNoSides <- list(imageMatrix = processedImage$imageMatrix[,-c(0:newFirst, newLast:ncol(imageMatrix))],
                          gaussiaMatrix = processedImage$gaussImageMatrix[,-c(0:newFirst, newLast:ncol(imageMatrix))])

    return(ImageNoSides)
  }
}




#' Finding Rough Bounds for Two Traces
#'
#' @param imageMatrix An imported image, can be imported with tiff_import()
#' @param FindPeaksdf -- Row Index, Max Height, PeakStart, PeakEnd -- identifies traces by summing rows
#' @param rowSums Of the imported matrix from original picture
#' @param max_roc Maximum Rate-of-Change of pixel location column-by-column
#' @returns
#
.find_bounds_2 <- function(imageMatrix, FindPeaksdf, rowSums, max_roc = 20){
  #
  #  FindPeaksdf -- Row Index of "peak", Max Height of "peak", PeakStart/PeakEnd - which are the edges
  #  of the ACTUAL spikey bit of the peak, not the overall mountain shape
  #

  # what if there ARE no white pixels ... what happens?
  # subset to imageMatrix[, first_col_that_has_white:last_col_that_has_white]


  #########################################################################
  ##
  ## Upper bound, trace 1  (upper = top of image)

  # First white pixel per column
  min_white <- apply(imageMatrix, MAR = 2, FUN = function(x) {
    min( which(x == 1) )
  })

  # Some are text and artifacts
  d_e <- diffs(min_white)
  bad_text <- which(abs(d_e) >= max_roc)  # need to check here

  # Instead, jump to other side of these ... then find the first white going down
  min_white_fix <- apply(imageMatrix[, bad_text], MAR = 2, FUN = function(x) {
    first_white <- min( which(x == 1) )
    bottom_first <- min( which(x[(first_white+1):length(x)] == 0) )
    min( which( x[(bottom_first+1):length(x)] == 1 ) )
  })
  min_white[bad_text] <- min_white_fix

  ############################################################################
  ##
  ## Lower bound, trace 4? (lower = bottom of image)

  # Last white pixel per column
  max_white <- apply(imageMatrix, MAR = 2, FUN = function(x) {
    max( which(x == 1) )
  })

  # Some are text and artifacts
  d_e <- diffs(max_white)
  bad_text <- which(abs(d_e) >= max_roc)  # need to check bad_text for length > 1

  # Instead, jump to other side of these ... then find the first white going down
  max_white_fix <- apply(imageMatrix[, bad_text], MAR = 2, FUN = function(x) {
    last_white <- max( which(x == 1) )
    bottom_black <- max( which(x[1:(last_white - 1)] == 0) )
    max( which( x[1:(bottom_black - 1)] == 1 ) )
  })
  max_white[bad_text] <- max_white_fix

  # fix the bound indexes to make sense?
  return(list(top_bound = min_white, bottom_bound = max_white))
}


#
#   # PeakStart[1] is the first peak, from top of image moving down
#   startrow_top <- FindPeaksdf$PeakStart[1] - minPeakDistance
#
#
#   ## Actually lower bound, trace 2  (lower = bottom of image)
#   max_white <- apply(imageMatrix, MAR = 2, FUN = function(x) {
#     white_pixels <- which(x == 1)
#     max(white_pixels[white_pixels > startrow_top]) }
#   )
#
#   # Case 1 - the peaks are clearly separated, which means FindPeaksdf$PeakEnd[1] < FindPeaksdf$PeakStart[2]
#   # (or, rather, at least a few pixels part ...)
#   if(FindPeaksdf$PeakEnd[1] < FindPeaksdf$PeakStart[2] - 1) {
#
#     middle <- rep( round((FindPeaksdf$PeakEnd[1] + FindPeaksdf$PeakStart[2]) / 2), ncol(imageMatrix) )
#
#   } else {  # Case 2 - the peaks are smeared together - possibly still separated, but not clear from FindPeaks
#
#     ## Work from these two, find the midpoint of the traces OR the collision
#     middle_white <- apply(imageMatrix, MAR = 2, FUN = function(x) {
#       white_pixels <- which(x == 1)
#       white_pixels <- white_pixels[white_pixels >= min_white & white_pixels <= max_white]
#       d_white <- diff(white_pixels)
#
#       if(any(d_white) > 1) { # there's a gap between traces, find the middle of it
#
#         bottom_trace1 <- white_pixels[which(d_white > 1)[1] - 1]
#         top_trace2 <- white_pixels[which(d_white > 1)[1]]
#         round( (bottom_trace1 + top_trace2) / 2 )
#
#       } else {  # the traces collided - there's no gap between them!
#
#         round(( white_pixels[1] + white_pixels[length(white_pixels)] ) / 2 )
#
#       }
#     })
#
#     ## Identify collided columns, if any
#     collided <- apply(imageMatrix, MAR = 2, FUN = function(x) {
#       white_pixels <- which(x == 1)
#       white_pixels <- white_pixels[white_pixels >= min_white & white_pixels <= max_white]
#       d_white <- diff(white_pixels)
#
#       if (any(d_white) > 1) { # there's a gap between traces, no collision
#
#         FALSE
#
#       } else {  # the traces collided - there's no gap between them!
#
#         TRUE
#
#       }
#     })
#
#
#     # have collided, middle_white, min_white, max_white
#     top_envelope <- min_white
#     middle_envelope <- middle_white
#     bottom_envelope <- max_white
#     collided_cols <- collided
#
#   }
# }
#
#
#
# .remove_timing_traces <- function(FindPeaksdf){
#   if (FindPeaksdf$PeakEnd[2] > FindPeaksdf$PeakStart[3] || FindPeaksdf$PeakEnd[1] > FindPeaksdf[3]) {
#     return(stop("Cannot remove timing as the traces overlap timing lines"))
#   }
#   else{
#     FindPeaksdf[-c(3,4), ]
#     return(FindPeaksdf)
#   }
# }
