test_that("simple na.cum*", {
	x <- c(NA, NA, TRUE, NA, FALSE, TRUE, NA, NA)
	expect_equal(na.cumsum(x), c(NA, NA, 1L, NA, 1L, 2L, NA, NA))
	expect_equal(na.cumsum(x, na.locf=T), c(NA, NA, 1L, 1L, 1L, 2L, 2L, 2L))
	expect_equal(na.cumsum(x, TRUE, TRUE), c(0L, 0L, 1L, 1L, 1L, 2L, 2L, 2L))

	expect_equal(na.cumprod(x), c(NA, NA, TRUE, NA, FALSE, FALSE, NA, NA))
	expect_equal(na.cumprod(x, na.locf=T), c(NA, NA, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
	expect_equal(na.cumprod(x, T, T), c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))

	expect_equal(na.cummax(x), c(NA, NA, TRUE, NA, TRUE, TRUE, NA, NA))
	expect_equal(na.cummax(x, na.locf=T), c(NA, NA, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))
	expect_equal(na.cummax(x, T, T), c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))

	expect_equal(na.cummin(x), c(NA, NA, TRUE, NA, FALSE, FALSE, NA, NA))
	expect_equal(na.cummin(x, na.locf=T), c(NA, NA, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
	expect_equal(na.cummin(x, T, T), c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))

	x <- c(NA, NA, 2, NA, 5, 1, NA, NA)
	expect_equal(na.cumsum(x), c(NA, NA, 2, NA, 7, 8, NA, NA))
	expect_equal(na.cumsum(x, TRUE), c(NA, NA, 2, 2, 7, 8, 8, 8))
	expect_equal(na.cumsum(x, TRUE, TRUE), c(0, 0, 2, 2, 7, 8, 8, 8))

	expect_equal(na.cumprod(x), c(NA, NA, 2, NA, 10, 10, NA, NA))
	expect_equal(na.cumprod(x, TRUE), c(NA, NA, 2, 2, 10, 10, 10, 10))
	expect_equal(na.cumprod(x, TRUE, TRUE), c(1, 1, 2, 2, 10, 10, 10, 10))

	expect_equal(na.cummax(x), c(NA, NA, 2, NA, 5, 5, NA, NA))
	expect_equal(na.cummax(x, TRUE), c(NA, NA, 2, 2, 5, 5, 5, 5))
	expect_equal(na.cummax(x, TRUE, TRUE), c(2, 2, 2, 2, 5, 5, 5, 5))

	expect_equal(na.cummin(x), c(NA, NA, 2, NA, 2, 1, NA, NA))
	expect_equal(na.cummin(x, TRUE), c(NA, NA, 2, 2, 2, 1, 1, 1))
	expect_equal(na.cummin(x, TRUE, TRUE), c(2, 2, 2, 2, 2, 1, 1, 1))
})
