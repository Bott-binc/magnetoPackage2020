context(desc = ".allowed_edge_distance")
test_that("returns correct values, checks for percentage i.e. less than 100",{
  rowSums <- rep(1, 100)
  ret <- data.frame(leftDist = 20, rightDist = 80)

  expect_equal(.allowed_edge_distance(rowSums, percentage = 20), ret)
  ret2 <- data.frame(leftDist = 20, rightDist = 60)
  expect_equal(.allowed_edge_distance(rowSums, percentage = 60, percentageLeftSide = 20), ret2)

})
test_that("returns the correct Error",{
  rowSums <- rep(1, 100)
  expect_error(.allowed_edge_distance(rowSums, percentage = 110),
               regexp = "Can't have a percentage over 100")
  expect_error(.allowed_edge_distance(rowSums, percentage = 110, percentageLeftSide = 90),
               regexp = "Can't have a percentage over 100")
  expect_error(.allowed_edge_distance(rowSums, percentage = 80, percentageLeftSide = 110),
               regexp = "Can't have a percentage over 100")
  expect_error(.allowed_edge_distance(rowSums, percentage = 80, percentageLeftSide = 50),
               regexp = "Can't have percentage plus percentageLeftSide be greater then")
})



context(desc = ".edge_peaks_rm")
test_that("returns findingPeaks in corrrect form, removes correct peaks", {
  findingPeaks <- data.frame(Index = c(26, 500, 1000, 1500, 2000), Height = rep(1000, 5))
  rowSums <- rep(1, 2500)
  findingPeaks90 <- data.frame(Index = c(500, 1000, 1500, 2000), Height = rep(1000, 4))
  findingPeaks80 <- data.frame(Index = c(1000, 1500), Height = rep(1000, 2))
  expect_equal(.edge_peaks_rm(findingPeaks, rowSums, percentEdge = 10), findingPeaks90)
  expect_equal(.edge_peaks_rm(findingPeaks, rowSums, percentEdge = 20 ), findingPeaks80)
  expect_equal(.edge_peaks_rm(findingPeaks, rowSums, percentEdge = 1), findingPeaks)

})
test_that("returns the correct Error",{
  findingPeaks <- data.frame(Index = c(1, 500, 1000, 1500, 2000), Height = rep(1000, 5))
  rowSums <- rep(1, 2500)
  expect_error(.edge_peaks_rm(c(1:100), rowSums, percentEdge = 20),
               regexp = "must be in the correct form")
  expect_error(.edge_peaks_rm(findingPeaks, rowSums = 5, percentEdge = 20),
               regexp = "rowSums should be at least the same length as your largest indexed peak")
})



context(desc = ".highest_peaks")
test_that("returns the correct highest peaks",{
  findingPeaks <- data.frame(Index = c(1, 500, 1000, 1500, 2000), Height = c(20,100,60,80,40))
  highestFour <- data.frame(Index = c(500, 1500, 1000, 2000), Height = c(100, 80, 60, 40))
  highestTwo <- data.frame(Index = c(500, 1500), Height = c(100, 80))
  expect_equal(.highest_peaks(findingPeaks, maxPeaksAllowed = 4), highestFour)
  expect_equal(.highest_peaks(findingPeaks, maxPeaksAllowed = 2), highestTwo)
  expect_equal(.highest_peaks(highestTwo, maxPeaksAllowed = 4), highestTwo)
})
test_that("Correct Error Reported",{
  findingPeaks <- data.frame(Index = c(1, 500, 1000, 1500, 2000), Height = c(20,100,60,80,40))
  expect_error(.highest_peaks(findingPeaks, maxPeaksAllowed = 0),
               regexp = "MaxPeaksAllowed must be more then 0")
  expect_error(.highest_peaks(c(1:100), maxPeaksAllowed = 2),
               regexp = "must be a dataframe")
})



context(desc = ".finding_distance_to_peaks")
test_that("Returns the correct outcome",{
  findingPeaks <- data.frame(Index = c(1, 500, 1100, 1500, 2000), Height = c(20,100,60,80,40))
  findingPeaksOne <- data.frame(Index = c(500), Height = c(100))
  rowSums <- rep(1, 2500)
  datThird <- data.frame(leftDist = 600 , rightDist = 400)
  datFifth <- data.frame(leftDist = 500, rightDist = 500)
  datFirst <- data.frame(leftDist = 0, rightDist = 499)
  datFirstOne <- data.frame(leftDist = 499, rightDist = 2000)
  expect_equal(.finding_distance_to_peaks(findingPeaks, rowSums = rowSums, peakIndex = 3), datThird)
  expect_equal(.finding_distance_to_peaks(findingPeaks, rowSums = rowSums, peakIndex = 5), datFifth)
  expect_equal(.finding_distance_to_peaks(findingPeaks, rowSums = rowSums, peakIndex = 1), datFirst)
  expect_equal(.finding_distance_to_peaks(findingPeaksOne, rowSums = rowSums, peakIndex = 1), datFirstOne)
  })
test_that("Correct Error Reported",{
  findingPeaks <- data.frame(Index = c(1, 500, 1100, 1500, 2000), Height = c(20,100,60,80,40))
  rowSums <- rep(1, 2500)
  expect_error(.finding_distance_to_peaks(findingPeaks, rowSums, 6),
              regexp = "Less peaks then peak index")

})


# need to add the test for the case of j == index
context(desc = ".finding_Peak_Start_Ends")
test_that("Returns the correct start and ends",{
  findingPeaks <- data.frame(Index = c(1708, 1494, 1037, 812),
                              Height = c(152775.73, 149577.55, 29652.66, 26414.92))
  expected <- data.frame(Index = c(1708, 1494, 1037, 812),
                         Height = c(152775.73, 149577.55, 29652.66, 26414.92),
                         PeakStart = c(1656, 1454, 973, 800),
                         PeakEnd = c(1760, 1534, 1101, 824))
  rowSums = readRDS("~/magneto/tests/testData/rowSums.RDS")
  ConstDecreasingPeak <- data.frame(Index = 1000, Height = 2)
  constDecExpected <- data.frame(Index = 1000, Height = 2, PeakStart = 1, PeakEnd = 1999)
  rowSumsDec = c(rep(1,999), 2, rep(1, 1000))
  expect_equal(.finding_Peak_Start_Ends(FindingPeaksdf = findingPeaks, rowSums = rowSums), expected)
  expect_equal(.finding_Peak_Start_Ends(ConstDecreasingPeak, rowSumsDec), constDecExpected)
})



context(desc = ".not_bright_image")
test_that("scales correctly with correct return", {
  image <- matrix(c(NA, NA, NA, 0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),nrow = 4, ncol = 6) #avg 0.45 = 4.5/10
  expected <- readRDS("~/magneto/tests/testData/nonBrightRet.RDS")
  expect_equal((.not_bright_image(imageMatrix = image)), expected)
})




context(desc = ".for_bright_image")
test_that("scales correctly with correct return", {
  image <- matrix(c(NA, NA, NA, 0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),nrow = 4, ncol = 6) #avg 0.45 = 4.5/10
  expected <- readRDS("~/magneto/tests/testData/forBrightnessRet.RDS")
  expect_equal(suppressWarnings(.for_bright_image(imageMatrix = image)), expected)
})

