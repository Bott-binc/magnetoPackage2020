
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

    rightDist <- round((1 - rightDistance)*length(rowSums))
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
  if (length(rowSums) < max(FindingPeaksdf$Index)) {
    return(stop("rowSums should be at least the same length as your largest indexed peak"))
  }
  rm <- vector()
  dist <- .allowed_edge_distance(rowSums, percentEdge, percentageLeftSide = percentEdgeForLeft)
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
      if (j == index) {
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
#' @param Filter a vector specifying the dimensions of the kernel,
#'  which will be used to perform either delation or erosion, such as c(8,8)
#' @param method one of 'delation'(adds to image, making brights brighter), 'erosion' (subtracts from image brights darker)
#' @param threshold should be between 0 and 1 for normalized images
#' @param imageMatrix An imported image, can be imported with tiff_import()
#'
#' @return Gaussian matrix scaled for bright
.not_bright_image <- function(imageMatrix, Filter = c(8,8), method = 'delation', threshold = 0.5){#cutoffQuantile, sig = 10, kern.trunc = 0.05, nw = 3){
  imageMatrix[is.na(imageMatrix)] <- 0
  Dialation <- OpenImageR::delationErosion(imageMatrix, Filter = Filter, method = method)
  imageProcessed <- OpenImageR::image_thresholding(Dialation, thresh = threshold)

  # gaussImageMatrix <- suppressWarnings(abs(t( apply(imageMatrix, MARGIN = 1, FUN = deconv_gauss, sig = 10, kern.trunc = 0.05, nw = 3 ) )))
  #
  # imageMatrix[imageMatrix < (1 - mean(imageMatrix,na.rm = TRUE))] <- 0
  # imageMatrix[imageMatrix > 0] <- 1
  #
  # gaussImageMatrix[0:floor((1/62)*nrow(imageMatrix))] <- mean(gaussImageMatrix)
  # gaussImageMatrix[(nrow(gaussImageMatrix) - 0.01*nrow(imageMatrix)):(nrow(gaussImageMatrix)),] <- mean(gaussImageMatrix)
  # gaussImageMatrix[is.na(gaussImageMatrix)]
  # gaussImageMatrix[gaussImageMatrix < 0] <- 0
  # gaussImageMatrix[gaussImageMatrix < (stats::quantile(gaussImageMatrix, cutoffQuantile))] <- 0
  # gaussImageMatrix[gaussImageMatrix > 0] <- 1
  # retdf <- list(imageMatrix = imageMatrix, gaussImageMatrix = gaussImageMatrix)
  return(imageProcessed)

}



#' Bright image scaling to gaussian
#'
#' @param Filter a vector specifying the dimensions of the kernel,
#'  which will be used to perform either delation or erosion, such as c(13,13)
#' @param method one of 'delation'(adds to image, making brights brighter), 'erosion' (subtracts from image brights darker)
#' @param threshold should be between 0 and 1 for normalized images
#' @param imageMatrix An imported image, can be imported with tiff_import()
#'
#' @return Gaussian matrix scaled for bright
.for_bright_image <- function(imageMatrix, Filter = c(13,13), method = 'delation', threshold = 0.8){#imageMatrix, sig = 10, kern.trunc = 0.05, nw = 3, brightQuantile = 0.95){
  imageMatrix[is.na(imageMatrix)] <- 0
  Dialation <- OpenImageR::delationErosion(imageMatrix, Filter = Filter, method = method)
  imageProcessed <- OpenImageR::image_thresholding(Dialation, thresh = threshold)
  # imageMatrix[is.na(imageMatrix)] <- 0
  # imageMatrix[imageMatrix < (stats::quantile(imageMatrix, brightQuantile))] <- 0
  # imageMatrix[imageMatrix > 0] <- 1
  # gaussImageMatrix <- abs(t(apply(imageMatrix, MARGIN = 1, FUN = deconv_gauss, sig = 10, kern.trunc = 0.05, nw = 3 )))
  #
  # gaussImageMatrix[0:floor((1/20) * nrow(imageMatrix))] <- mean(gaussImageMatrix)
  # gaussImageMatrix[(nrow(gaussImageMatrix) - 0.05 * nrow(imageMatrix)):nrow(gaussImageMatrix), ] <- mean(gaussImageMatrix)
  # gaussImageMatrix[is.na(gaussImageMatrix)]
  # gaussImageMatrix[gaussImageMatrix < 0] <- 0
  # gaussImageMatrix[gaussImageMatrix < (stats::quantile(gaussImageMatrix, brightQuantile))] <- 0
  # gaussImageMatrix[gaussImageMatrix > 0] <- 1
  # retdf <- list(imageMatrix = imageMatrix, gaussImageMatrix = gaussImageMatrix)
  return(imageProcessed)
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
#' @param maxStart absolute max for the left side of the image (can be overridden with the length)
#' @param minEnd absolute min for the right side of the image ( can be overridden with the 0)
#'
#' @return Image matrix with removed start and end, or the index of these start and ends
.get_trace_start_ends <- function(imageMatrix, cutPercentage = 1, peakThreshold = 5, gapAllow = 20,
                                  returnMat = TRUE, maxStart = 700, minEnd = 4800){
  imageMatrix <- .horizontal_image_check(imageMatrix)
  processedImage <- .for_bright_image(imageMatrix) #Even if not bright image, found this to be the most consistent
  SumsImage <- colSums(processedImage)
  len <- length(SumsImage)
  diffsColSms <- abs(diff(SumsImage))
  pkThresh <- peakThreshold/100
  possibleStartDiffs <- which(diffsColSms >= pkThresh) # how big of a difference you are looking for (potential peaks)
  #riddance of gaps between possible starts(includes the black surround on an image)
  chosenDiffs <- possibleStartDiffs[which(abs(diff(possibleStartDiffs))  <= gapAllow)]
  cutPerc <- round(cutPercentage/100*len)
  chosenDiffs <- chosenDiffs[which(chosenDiffs <= (len - cutPerc) & chosenDiffs >= cutPerc)] # which chooseDiffs are inbetween cut perc
  first <- chosenDiffs[1] # starting with the first one in the list(closest to the out side of the image, and working in) Left Side
  newFirst <- first
  last <- chosenDiffs[length(chosenDiffs)] # starting with the last one in the list(closest to the out side of the image, and working in) Right Side
  newLast <- last
  # See if the next image has a larger or smaller col sum
  compareLeft <- SumsImage[first]
  compareRight <- SumsImage[last]
  middle <- len/2
  middleMean <- mean(SumsImage[round((middle - 0.2*len)):round((middle + 0.2*len))]) # catches run up onto the main flat of the colSums
  #plot to see main flat
  if (compareLeft > middleMean) { # checking that we aren't on the flat already
    compareLeft = middleMean
  }
  if (compareRight > middleMean) { # checking that we aren't on the flat already
    compareRight = middleMean
  }
  if (first + round(0.2 * len) >= len) { # Check that first + 20 perc isn't greater then total length
    index <- len - first - 1
  }
  else {
    index <- round(0.2*len)
  }
  # for the left side ````````
  for (j in 1:index) {
    if ( (first + j) > maxStart) {
      break
    }
    if (SumsImage[first + j] <= compareLeft + 1 & SumsImage[first + j]  <= middleMean - 3) {
      newFirst <- first + j
    }
  }
  #``````````````````````````
  if (last - round(0.2 * len) <= 0) { # Check that first + 20 perc isn't greater then total length
    index <- last - 1
  }
  else {
    index <- round(0.2*len)
  }
  #for Right Side ````````````
  for (k in 1:index) { # for the right side
    if ((last + j) < minEnd) {
      break
    }
    if (SumsImage[last - k] <= compareRight + 1 & SumsImage[last - k]  <= middleMean - 3) {
      newLast <- last - k
    }
  }
  #`````````````````````````
  if (returnMat == FALSE) {
  return(list(Start = newFirst, End = newLast))
  }
  else {# returnMat is true (removes the parts of image in those created bounds\)
    ImageNoSides <- processedImage[,-c(0:newFirst, newLast:ncol(imageMatrix))]
                         # gaussiaMatrix = processedImage[,-c(0:newFirst, newLast:ncol(imageMatrix))])

    return(ImageNoSides)
  }
}


# Dont think that I am using this right now
# .get_trace_top_bottom <- function(imageMatrix, minDistance, maxPeakNumber, percentFromEdge){
#   browser()
#   rowSums <- rowSums(imageMatrix)
#   chosenFlats <- vector()
#   peaks <- find_peaks(rowSums, minDistance = minDistance, maxPeakNumber = maxPeakNumber, percentFromEdge = percentFromEdge, plots = FALSE)
#   firstPeak <- peaks$PeakStart[1]
#   lastPeak <- peaks$PeakEnd[length(peaks$Index)]
#   diffIndex <- which(diff(rowSums(imageMatrix)) == 0)
#   possibleFlats <- rowSums[diffIndex]
#   # for (i in 1:(length(possibleFlats) - 3)) {
#   #   if (possibleFlats[i] == possibleFlats[i + 2] & possibleFlats[i] == possibleFlats[i + 3]) {
#   #     chosenFlats <- c(chosenFlats, possibleFlats[i])
#   #   }
#   # }
#   topFlats <- rowSums[which(diffIndex < firstPeak)]#which(diffIndex == chosenFlats) < firstPeak)
#   bottomFlats <- rowSums[which(diffIndex > lastPeak)]#which(diffIndex == chosenFlats) > lastPeak)
#   return(list(topFlats[length(topFlats)], bottomFlats[1]))
# }

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
  d_e <- diff(min_white)
  bad_text <- which(abs(d_e) >= max_roc)  # need to check here

  # Instead, jump to other side of these ... then find the first white going down
  min_white_fix <- apply(imageMatrix[, bad_text], MAR = 2, FUN = function(x) {
    first_white <- min( which(x == 1) )
    bottom_first <- min( which(x[(first_white + 1):length(x)] == 0) )
    min( which( x[(bottom_first + 1):length(x)] == 1 ) )
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
  d_e <- diff(max_white)
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


#' Top Image Cut
#'
#' returns part of the image that doesn't have any horizontal lines (traces) in,
#' will discard lettering(main use)
#'
#' @param imageMatrix Imported image into matrix form, can use tiff_import()
#' @param percentEdgeForLeft passed into find peaks, if not specified, uses
#' percentFromEdge for both left and right sides, if specified, percentFromEdge
#' is defaulted to just the right side of the plot
#' @param percentFromEdge used in find_peaks if you know there wont be a peak
#'  in a region
#'
#' @return the recommended cutoff of the top of your image
.top_image_cut <- function(imageMatrix, percentFromEdge, percentEdgeForLeft = NULL){
  rowsumsImage <- rowSums(imageMatrix)
  diffRowSumsImage <- diff(rowsumsImage)
  zeros <- .find_a_number(diffRowSumsImage, specNumber = 0)
  peaks <- find_peaks(rowSums = rowsumsImage, minDistance = 50, maxPeakNumber = 4, percentFromEdge = percentFromEdge,
                      plots = FALSE, percentEdgeForLeft = percentEdgeForLeft)
  firstPeak <- peaks$Index[1] # closest peak to the top of the image(because searching as if the image was vertical)
  longestCut <- sort(zeros$RunLength[which(zeros$StartIndex < firstPeak)], decreasing = TRUE)[1] # longest less then the first peak start
  indexesOfLongestCuts <- zeros$StartIndex[which(zeros$RunLength == longestCut)] # indexes of all points with that run length of 0's
  if (length(indexesOfLongestCuts) > 1) {# more then one found with that run length
    # takes the closest point less than the start of the first peak
    topCut <- indexesOfLongestCuts[sort(which(indexesOfLongestCuts < firstPeak), decreasing = TRUE)][1]
  }
  else if (length(indexesOfLongestCuts) == 1) {# only one found with that run length
    #takes the only one of that run length
    topCut <- indexesOfLongestCuts[sort(which(indexesOfLongestCuts < firstPeak), decreasing = TRUE)]
  }
  else{# none found with alg, just using top of image
    warning("No top cuts found.. defaulting to 0")
    topCut = 0
  }
  return(topCut)
}


#' Bottom Image Cut
#'
#' attempts to find a gap between the traces and timing marks, if found it will
#'  return the point in which the image can be trimmed removing the timing marks
#'
#' @param imageMatrix Imported image into matrix form, can use tiff_import()
#' @param percentFromEdge used in find_peaks if you know there wont be a peak
#' in a region
#' @param percentEdgeForLeft passed into find peaks, if not specified, uses
#' percentFromEdge for both left and right sides, if specified, percentFromEdge
#' is defaulted to just the right side of the plot
#' @param shortestAlowedSeqOfZeros smallest gap alowed to be found to consider
#' the trace not intersecting the timing marks
#'
#' @return value of bottom cut that should be removed
.bottom_image_cut <- function(imageMatrix, percentFromEdge, percentEdgeForLeft = NULL, shortestAlowedSeqOfZeros = 50){
  rowsumsImage <- rowSums(imageMatrix)
  diffRowSumsImage <- diff(rowsumsImage)
  zeros <- .find_a_number(diffRowSumsImage, specNumber = 0)
  peaks <- find_peaks(rowSums = rowsumsImage, minDistance = 100, maxPeakNumber = 4, percentFromEdge = percentFromEdge,
                      plots = FALSE, percentEdgeForLeft = percentEdgeForLeft)
  SecondPeak <- peaks$Index[2]
  ThirdPeak <- peaks$Index[3]

  longestCut <- sort(zeros$RunLength[which(zeros$StartIndex > SecondPeak & zeros$StartIndex < ThirdPeak)],
                     decreasing = TRUE)[1] # longest greater then the second peak end less then the third peak start
  indexesOfLongestCuts <- zeros$StartIndex[which(zeros$RunLength == longestCut)] # indexes of all points with that run length of 0's
  if (length(longestCut) == 0 || is.na(longestCut)) {# none found with alg, just using bottom of image (no warning displayed for this one though)
    warning("No cuts found.. defaulting to bottom of the image")
    bottomCut = length(rowsumsImage)
  }
  else if (longestCut < shortestAlowedSeqOfZeros) {
    warning("Intersection in Timing Found")
    return(length(rowsumsImage))
  }
  else if (length(indexesOfLongestCuts) > 1) {# more then one found with that run length
    # takes the takes the closest point to the start of the third peak
    bottomCut <- indexesOfLongestCuts[sort(which(indexesOfLongestCuts > SecondPeak & indexesOfLongestCuts < ThirdPeak),
                                           decreasing = TRUE)][1]
  }
  else if (length(indexesOfLongestCuts) == 1) {# only one found with that run length
    #takes the only one of that run length
    bottomCut <- indexesOfLongestCuts[sort(which(indexesOfLongestCuts > SecondPeak & indexesOfLongestCuts < ThirdPeak),
                                           decreasing = TRUE)]
  }
  return(bottomCut)
}



#' Rough Trim
#'
#' trims roughly the top and bottom if the user knows where there will definitly not be any points
#'
#' @param image A matrix
#' @param trimAmountTop Number of pixels
#' @param trimAmountBottom Number of pixels
#'
#' @return the matrix without the specified bounds
.trim_top_bottom <- function(image, trimAmountTop = 100, trimAmountBottom = 50){
  if (trimAmountTop != 0 & trimAmountBottom != 0) {
    imageProcessed <- image[-c(1:trimAmountTop, (nrow(image) - trimAmountBottom + 1):nrow(image)),]
  }
  else if (trimAmountTop != 0) {
    imageProcessed <- image[-c(1:trimAmountTop),]
  }
  else if (trimAmountBottom != 0) {
    imageProcessed <- image[-c((nrow(image) - trimAmountBottom + 1):nrow(image)),]
  }
  return(imageProcessed)
}



#' Process Image
#'
#' @param imageMatrix Imported image into matrix form, can use tiff_import()
#' @param beta0 From logistic regression on what images to be considered bright
#' @param beta1 From logistic regression on what images to be considered bright
#' @param cutoffProbability Passed into bright: The probability cut off for the decision of an imageMatrix being bright
#' @param NADefault The defult value set to points of NA found by the system
#'
#' @return The processed image with the gaussian and the non gaussian in an array labeled respectively
.process_image <- function(imageMatrix, beta0 = -2.774327, beta1 = 51.91687, cutoffProbability = 0.5, NADefault = 0){
  bright <- bright(imageMatrix, beta0 = beta0, beta1 = beta1, cutoffProbability = cutoffProbability, NADefault = NADefault)
  if (bright == TRUE) {
    imageProcessed <- .for_bright_image(imageMatrix)
  }
  if (bright == FALSE) {
    imageProcessed <- .not_bright_image(imageMatrix)#, cutoffQuantile = cutoffQuantile)
  }
  return(imageProcessed)
}


#' Find A Specific Number
#'
#' Used for finding all sequences of one number in a vector and start indexes
#' and lengths of each sequence are returned to the user
#'
#' @param vector a generic vector of any length
#' @param specNumber a number that you would like to find all sequences of
#'
#' @return vector of all occurrences lengths of each sequence and the index of each start
.find_a_number <- function(vector, specNumber){
  StartIndex = vector()
  Length = vector()
  foundZero <- FALSE
  counter <- 0

  for (i in 1:length(vector)) {
    # last diff is specNumber in vector
    if (i == length(vector) & vector[i] == specNumber) {
      if (counter == specNumber) { #this is the first specNumber found in a while
        Length <- append(Length, 1)
        StartIndex <- append(StartIndex, i)
      }
      else {# this isnt the first specNumber found in a while i.e specNumber,specNumber,specNumber,specNumber,(this one)
        Length <- append(Length, counter)
      }
    }
    else if (isTRUE(foundZero) & vector[i] == specNumber) { # found another specNumber in a row
      counter <- counter + 1
    }
    else if (isTRUE(foundZero)) { #end of the specNumber sequence
      Length <- append(Length, counter)
      foundZero = FALSE
      counter = specNumber
    }
    else if (isFALSE(foundZero) & vector[i] == specNumber) { #first specNumber found for a new sequence
      StartIndex <- append(StartIndex, i)
      foundZero <- TRUE
      counter <- 1
    }
  }
  zeros <- data.frame(StartIndex = StartIndex, RunLength = Length)
  return(zeros)
}
