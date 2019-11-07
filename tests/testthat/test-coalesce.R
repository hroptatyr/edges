test_that("simple", {
	expect_equal(coalesce(c(NA, NA, "bar"), c("foo", NA, "zar")), c("foo",NA,"bar"))
	expect_equal(coalesce(factor(c(NA, NA, "bar")), factor(c("foo", NA, "zar"))), c("foo",NA,"bar"))
	expect_equal(coalesce(as.Date(c(NA, NA, "2020-01-01")), as.Date(c("2017-01-01", NA, "2022-01-01"))), as.Date(c("2017-01-01", NA, "2020-01-01")))
	expect_equal(coalesce(as.Date(c(NA, NA, "2020-01-01")), NULL, as.Date(c("2017-01-01", NA, "2022-01-01"))), as.Date(c("2017-01-01", NA, "2020-01-01")))
	expect_equal(coalesce(as.Date(c(NA, NA, "2020-01-01")), NULL, as.Date(c("2017-01-01", NA, NA, "2022-01-01"))), as.Date(c("2017-01-01", NA, "2020-01-01", "2022-01-01")))
})

test_that("coalesce extending", {
	expect_equal(coalesce(NULL, c("ab",NA), NULL, c(NA,"cd","ef")), c("ab","cd","ef"))
	expect_equal(coalesce(NULL, as.Date(c("2000-01-01",NA)), NULL, as.Date(c(NA,"2001-01-01","2002-01-01"))), as.Date(c("2000-01-01","2001-01-01","2002-01-01")))
})

test_that("coalesce recycle", {
	expect_equal(coalesce(c("ab",NA,"ef"), NULL, "xy"), c("ab","xy","ef"))
})
