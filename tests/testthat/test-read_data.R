skip("test manually")

# Choose nc file to read
bm181021 <- read_depth(file.choose(), "US/Pacific")

test_that("sampling rate matches data", {
  data_freq <- 1 / as.numeric(diff(bm181021$data$dt))
  fs <- rep(bm181021$fs, length(data_freq))
  expect_equal(data_freq, fs, tolerance = 0.001)
})

# Decimate data to 1 hz
bm181021_1hz <- bm181021 %>%
  smooth_p(1) %>%
  decimate(1)

test_that("decimation results in correct frequency", {
  data_freq <- 1 / as.numeric(diff(bm181021_1hz$data$dt))
  fs <- rep(1, length(data_freq))
  expect_equal(data_freq, fs, tolerance = 0.001)
})
