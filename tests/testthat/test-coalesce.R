test_that("simple", {
	expect_equal(coalesce(c(NA, NA, "bar"), c("foo", NA, "zar")), c("foo",NA,"bar"))
	expect_equal(coalesce(factor(c(NA, NA, "bar")), factor(c("foo", NA, "zar"))), c("foo",NA,"bar"))
	expect_equal(coalesce(as.Date(c(NA, NA, "2020-01-01")), as.Date(c("2017-01-01", NA, "2022-01-01"))), as.Date(c("2017-01-01", NA, "2020-01-01")))
	expect_equal(coalesce(as.Date(c(NA, NA, "2020-01-01")), NULL, as.Date(c("2017-01-01", NA, "2022-01-01"))), as.Date(c("2017-01-01", NA, "2020-01-01")))
	expect_equal(coalesce(as.Date(c(NA, NA, "2020-01-01")), NULL, as.Date(c("2017-01-01", NA, NA, "2022-01-01"))), as.Date(c("2017-01-01", NA, "2020-01-01", "2022-01-01")))
	expect_equal(coalesce(c(11L, NA, 13L, NA, 15L, NA), c(NA, 12L, 5L), c(11L, NA, 1L, 14L, NA)), c(11L, 12L, 13L, 14L, 15L, NA))
})

test_that("coalesce extending", {
	expect_equal(coalesce(NULL, c("ab",NA), NULL, c(NA,"cd","ef")), c("ab","cd","ef"))
	expect_equal(coalesce(NULL, as.Date(c("2000-01-01",NA)), NULL, as.Date(c(NA,"2001-01-01","2002-01-01"))), as.Date(c("2000-01-01","2001-01-01","2002-01-01")))
})

test_that("coalesce recycle", {
	expect_equal(coalesce(c("ab",NA,"ef"), NULL, "xy"), c("ab","xy","ef"))
})

test_that("coalesce NULL", {
	expect_true(is.null(coalesce(NULL)))
	expect_true(is.null(coalesce(NULL, NULL)))
	expect_true(is.null(coalesce(NULL, NULL, NULL)))

	expect_true(!is.na(coalesce(1, NULL)))
	expect_true(!is.na(coalesce(1, NULL, NULL)))
	expect_equal(length(coalesce(NULL, c(1,2), NULL)), 2L)
	expect_equal(length(coalesce(1, NULL, c(1,2))), 2L)
})

test_that("tcoalesce", {
	x = c(11L, NA, 13L, NA, 15L, NA)
	y = c(NA, 12L, 5L)
	z = c(11L, NA, 1L, 14L, NA, NA)
	expect_equal(tcoalesce(x, y, z), list(11L, 12L, 11L))
	expect_equal(tcoalesce(x, y, z, rev=TRUE), list(15L, 5L, 14L))

	x = c(NA, 13L, NA)
	y = c(NA, "FOO", "BAR")
	z = c(NA, NA)
	expect_equal(tcoalesce(x,y,z), list(13L, "FOO", NA))
	expect_equal(tcoalesce(x,y,z, rev=TRUE), list(13L, "BAR", NA))

	expect_equal(tcoalesce(data.frame(a=x,b=y,c=as.Date(c(z,NA)))), list(a=13L,b="FOO",c=as.Date(NA)))
})

test_that("tcoalesce", {
	x = data.frame(V1=c("abc","def","ghi","jkl"), V2=c(NA, 2, 3, NA))
	expect_equal(tcoalesce.complete.cases(x), list(V1="def", V2=2))
	expect_equal(tcoalesce.complete.cases(x, rev=TRUE), list(V1="ghi", V2=3))
})
