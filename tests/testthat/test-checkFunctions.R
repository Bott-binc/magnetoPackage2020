context(desc = ".is_tiff")
test_that(".is_tiff returns the the correct bool", {
  vector <- c("test.tif", "test-@.tif", "test.tiff", "test-@.tiff", "test.png", "test-@.pdf")
  ret <- vector()
  for (i in 1:6) {
    ret[i] <- .is_tiff(vector[i])
  }
  expect_equal(ret, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
})



context(desc = ".null")
test_that(".null returns an error if the specifed parameter is null",{

  expect_error(.null(parameter = NULL, parameterName = "john"),
               regexp = "Need to specify john")
  expect_null(.null(parameter = "four", parameterName = "age"))

})



context(desc = "not_empty_file")
test_that("Checking that returns correct bool",{
  expect_equal(not_empty_file("~/magneto/tests/testData/", "AGC-D-18610822-18610823.tif"), TRUE)
  expect_equal(not_empty_file("~/magneto/tests/testData/", "Emptyfile.png"), FALSE)


})
test_that("Correct Error Reported", {
  expect_error(not_empty_file("~/magneto/tests/testData/", "DoesntExistFile"),
               regexp = "doesn't exist")
  expect_error(not_empty_file(NA, "Emptyfile.png"),
               regexp = "missing filePath or fileName")
})



context(desc = ".horizontal_image_check")
test_that("Checking for a change from vertical to horozontal",{
  vertical <- readRDS("~/magneto/tests/testData/vertImagedf.RDS")
  horizontal <- readRDS("~/magneto/tests/testData/horImagedf.RDS")
  expect_equal(.horizontal_image_check(vertical), horizontal)
  expect_equal(.horizontal_image_check(horizontal), horizontal)
})



# context(desc = ".array_clas_edit")
# test_that("Given array, edits correctly, if not array continues",{
#   vector1 <- c(0.5,0.9,0.3)
#   vector2 <- c(0.10,0.11,0.12,0.13,0.14,0.20)
#   startArray <- array(c(vector1,vector2),dim = c(3,3,2))
#   expected <- array(c(vector1, vector2), dim = c(3,3,1))
# })



context(desc = ".triple_check")
test_that("Triple is identified and non triple is okay", {
  triple <- readRDS("~/magneto/tests/testData/imageProcessedTripleD-18880915.RDS")
  bottomcutTrip <- readRDS("~/magneto/tests/testData/bottomcutD-18880915.RDS")
  bottomcutDub <- readRDS("~/magneto/tests/testData/bottomCutH-19260103.RDS")
  topcutTrip <- readRDS("~/magneto/tests/testData/topcutD-18880915.RDS")
  topcutDub <- readRDS("~/magneto/tests/testData/topCutH-19260103.RDS")
  double <- readRDS("~/magneto/tests/testData/matrixImageH-19260103.RDS")
  expectedTriple <- TRUE
  expectedDuble <- FALSE
  expect_equal(.triple_check(triple, topCut = topcutTrip, bottomCut = bottomcutTrip), expectedTriple)
  expect_equal(.triple_check(double, topCut = topcutDub, bottomCut = bottomcutDub), expectedDuble)
})



context(desc = ".intersection_check")
test_that("Intersection identified", {
  imageName <-  "AGC-D-19211221-19211223.tif"
  topLowerEnv <- readRDS("~/magneto/tests/testData/topLowerIntersection.RDS")
  bottomUpperEnv <- readRDS("~/magneto/tests/testData/bottomUpperIntersection.RDS")
  expect_warning(.intersection_check(topEnv = topLowerEnv, bottomEnv = bottomUpperEnv, imageName = imageName),
                regexp = "There is an intersection at")
})
