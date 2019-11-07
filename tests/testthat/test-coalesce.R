test_that("simple", {
	expect_equal(coalesce(c(NA, NA, "bar"), c("foo", NA, "zar")), c("foo",NA,"bar"))
	expect_equal(coalesce(factor(c(NA, NA, "bar")), factor(c("foo", NA, "zar"))), c("foo",NA,"bar"))
	expect_equal(coalesce(as.Date(c(NA, NA, "2020-01-01")), as.Date(c("2017-01-01", NA, "2022-01-01"))), as.Date(c("2017-01-01", NA, "2020-01-01")))
	expect_equal(coalesce(as.Date(c(NA, NA, "2020-01-01")), NULL, as.Date(c("2017-01-01", NA, "2022-01-01"))), as.Date(c("2017-01-01", NA, "2020-01-01")))
	expect_equal(coalesce(as.Date(c(NA, NA, "2020-01-01")), NULL, as.Date(c("2017-01-01", NA, NA, "2022-01-01"))), as.Date(c("2017-01-01", NA, "2020-01-01", "2022-01-01")))
})
