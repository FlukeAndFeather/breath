test_that("MATLAB DNs convert to POSIXct", {
  expect_equal(
    dn_to_posix(737817.8),
    as.POSIXct("2020-01-27 19:12:00", tz = "UTC")
  )
})
