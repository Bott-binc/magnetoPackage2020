context(desc = "tiff_import")
test_that("tiff_import returns only tiffs", {
  testFile <- tiff_import(fileName = "AGC-D-18610822-18610823.tif", fileLoc = "~/magneto/tests/testData/")
  expect_equal_to_reference(testFile, update = FALSE, file = "~/magneto/tests/testData/tiffimport1861.RDS")

})
test_that("Correct Error Reported",{
  expect_error(tiff_import("DoesNotExist.png", "~/magneto/tests/testData/"),
               regexp = "doesn't exist")
  expect_error(tiff_import("test.tif", c("~/magneto/tests/testData/", "extraThing")),
               regexp = "fileLoc must be a single character")
  expect_error(tiff_import(c("test.tif", "test.tif"), "~/magneto/tests/testData/"),
               regexp = "fileName must be a single character")
  expect_error(tiff_import(fileName = "Emptyfile.png", fileLoc = "~/magneto/tests/testData/"),
               regex = "not a .tif or .tiff")
  expect_error(tiff_import(fileName = "Emptyfile.tiff", fileLoc = "~/magneto/tests/testData/"),
               regexp = "is 0b/Empty")
})


context(desc = "bright")
test_that("bright returns correct decision for correct cutoff and scales if there are values
          greater then 1 in the matrix", {
  expect_equal(bright(imageMatrix = matrix(c(0,0,0,0.8), byrow = TRUE, nrow = 2)), TRUE)
  expect_equal(bright(imageMatrix = matrix(c(0,0,0,0.7), byrow = TRUE, nrow = 2)), FALSE)
})
test_that("Correct Error Reported/Correct Warning Reported",{
  expect_error(bright(data.frame(testing = 1:5, column = 1:5)),
               regexp = "imageMatrix must be a matrix")
  expect_warning(bright(matrix(c(0.1,0.1,0.1,7), byrow = TRUE, nrow = 2)),
                 regexp = "Shouldnt be pixels with brightness above 1...scaling by largest value")
  expect_warning(bright(matrix(c(0.1,0.2,0.7,NA), byrow = TRUE, nrow = 2)),
                 regexp = "NA's found in the matrix.. setting them to 0")
})



context(desc = "find_peaks")
test_that("Correct return given to user", {
  rowSums <- readRDS("~/magneto/tests/testData/rowSums.RDS")
  expectedRet <- readRDS("~/magneto/tests/testData/FullPeakStartEnd.RDS")
  expect_equal(find_peaks(rowSums = rowSums, minDistance = 100, maxPeakNumber = 4,
                          percentFromEdge = 10, plots = FALSE), expectedRet)
})
test_that("Correct Error Reported/Correct Warning Reported", {
  rowSums <- readRDS("~/magneto/tests/testData/rowSums.RDS")
  rowSumsNoPeaks <- rep(1, 1000)
  expect_error(find_peaks("This is not a numeric vector", minDistance = 100, maxPeakNumber = 4,
                          percentFromEdge = 10, plots = FALSE),
              regexp = "rowSums must be a vector")
  expect_error(find_peaks(rowSumsNoPeaks, minDistance = 100, maxPeakNumber = 4,
                          percentFromEdge = 10, plots = FALSE),
               regexp = "No Peaks Found...")
})



context(desc = "find_paths_for_keyword")
test_that("Returns the correct values",{
  expected <- "/home/ben/magneto/tests/testData/tiffimport1861.RDS"
  #NOTE:Symlink is on my profile, will have to change to test if on another
  expectedSymLink <- "/home/ben/magnetoMARK/Scripts/MarkW/Thesis_Comparison.R"
  expect_equal(find_paths_for_keyword(path = "~/magneto/tests", keyword = "tiffimport1861"), expected)
  expect_warning(expect_equal(find_paths_for_keyword(path = "~/", keyword = "Thesis"), expectedSymLink),
                 regexp = "Failed to search directory")
})
test_that("Returns The Correct Error",{
  expect_error(find_paths_for_keyword(path = "~/magneto/tests", keyword = "thereIsNoFileWithThisName"),
               regexp = "No files found with that name")
})



context(desc = "deconv_gauss")
test_that("Returns expected value", {
  image <- matrix(data = c(1:12), nrow = 4, ncol = 6)
  expected <- c(-3.1587712, -3.5025280, -3.6482256, -3.7332883,
                -3.7663271, -3.3626800, -1.7124795, 1.8264546,
                6.8017323, 11.3811585, 13.3243517, 11.5739424,
                7.0550070,  1.9330442, -1.9662331, -4.1387981,
                -5.0355809, -4.9397223, -3.3559227,  0.4073428,
                6.0395086, 11.7147557, 15.0063607, 14.5793650)
  expect_equal((deconv_gauss(imageMatrix = image)), expected)
})
