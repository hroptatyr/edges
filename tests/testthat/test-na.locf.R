test_that("simple locf", {
	x <- c(NA, NA, "EUR", NA, "INR", "EUR", NA, NA)
	expect_equal(na.locf(x), c(NA, NA, "EUR", "EUR", "INR", "EUR", "EUR", "EUR"))
	expect_equal(na.locf(x, TRUE), c("EUR", "EUR", "EUR", "EUR", "INR", "EUR", "EUR", "EUR"))

	x <- c(NA, NA, 2, NA, 4, 5, NA, NA)
	expect_equal(na.locf(x), c(NA, NA, 2, 2, 4, 5, 5, 5))
	expect_equal(na.locf(x, TRUE), c(2, 2, 2, 2, 4, 5, 5, 5))
})

test_that("simple nocb", {
	x <- c(NA, NA, "EUR", NA, "INR", "EUR", NA, NA)
	expect_equal(na.nocb(x), c("EUR", "EUR", "EUR", "INR", "INR", "EUR", NA, NA))
	expect_equal(na.nocb(x, TRUE), c("EUR", "EUR", "EUR", "INR", "INR", "EUR", "EUR", "EUR"))

	x <- c(NA, NA, 2, NA, 4, 5, NA, NA)
	expect_equal(na.nocb(x), c(2, 2, 2, 4, 4, 5, NA, NA))
	expect_equal(na.nocb(x, TRUE), c(2, 2, 2, 4, 4, 5, 5, 5))
})

test_that("data.frame locf/nocb", {
	x <- data.frame(ccy=c(NA, NA, "EUR", NA, "INR", "EUR", NA, NA))
	expect_equal(na.locf(x), data.frame(ccy=c(NA, NA, "EUR", "EUR", "INR", "EUR", "EUR", "EUR")))
	expect_equal(na.nocb(x), data.frame(ccy=c("EUR", "EUR", "EUR", "INR", "INR", "EUR", NA, NA)))
	expect_equal(na.locf(x, first.backward=TRUE), data.frame(ccy=c("EUR", "EUR", "EUR", "EUR", "INR", "EUR", "EUR", "EUR")))
	expect_equal(na.nocb(x, last.forward=TRUE), data.frame(ccy=c("EUR", "EUR", "EUR", "INR", "INR", "EUR", "EUR", "EUR")))
})
