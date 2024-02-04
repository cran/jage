test_that("age estimation runs for 1 row", {
  out=find_fuzzies.f(c(NA,NA,9,10,11,14,15,10,15,11),prior="jeff")
  expect_s3_class(out[[2]], "data.table")
  expect_true(nrow(out[[2]])==1)
  #expect_true(length(is.na(out))==0)
})
