test_that("GACE runs for monthly data", {
  y <- ts(rnorm(36, 100, 5), frequency = 12)
  fc <- gace_forecast(y, periods = 6, freq = "month")
  expect_equal(nrow(fc), 36 + 6)
})

test_that("GACE runs for weekly data", {
  y <- ts(rnorm(104, 100, 10), frequency = 52)
  fc <- gace_forecast(y, periods = 10, freq = "week")
  expect_equal(nrow(fc), 104 + 10)
})

test_that("Caps are applied and prevent explosion", {
  y <- ts(cumsum(rnorm(40, 2, 1)), frequency = 4)
  fc <- gace_forecast(
    y,
    periods = 4,
    freq = "quarter",
    cap_low = -0.05,
    cap_high = 0.10
  )
  
  fc_values <- subset(fc, type == "forecast")$value
  
  expect_true(max(fc_values) < max(y) * 1.5)
})