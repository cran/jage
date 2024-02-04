test_that("age estimation runs for 3 cases", {
  out=find_fuzzies.f(nmdid.test[1:3,],prior="jeff")
  expect_s3_class(out, "data.table")
  expect_true(nrow(out)==3)
  #expect_true(length(is.na(out))==0)
})
