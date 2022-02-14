test_that("check_consistency_flag works", {
  valid_flags <- c('no consistency check', 'check consistency and report', 'check consistency and fix')
  for(iFlag in 1:length(valid_flags)) {
    expect_equal(check_consistency_flag(valid_flags[iFlag]), iFlag - 1)
  }
  expect_error(check_consistency_flag("some other flag"))
  expect_error(check_consistency_flag(0))
  expect_error(check_consistency_flag(valid_flags))
  expect_error(check_consistency_flag(NA))
  expect_error(check_consistency_flag(NULL))
})

test_that("check_logical_flag works", {
  expect_equal(check_logical_flag(TRUE), TRUE)
  expect_equal(check_logical_flag(FALSE), FALSE)
  expect_error(check_logical_flag(1))
  expect_error(check_logical_flag("TRUE"))
  expect_error(check_logical_flag(c(TRUE, FALSE)))
  x <- NA
  expect_error(check_logical_flag(x))
  x <- NULL
  expect_error(check_logical_flag(x))
})

test_that("check_string_parameter works", {
  expect_equal(check_string_parameter('TRUE'), "TRUE")
  expect_error(check_string_parameter(1))
  expect_error(check_string_parameter(TRUE))
  expect_error(check_string_parameter(c("TRUE", "FALSE")))
  x <- NA
  expect_error(check_string_parameter(x))
  x <- NULL
  expect_error(check_string_parameter(x))
})


test_that("is_compiled works", {
  expect_type(is_compiled(), "logical" )
})
