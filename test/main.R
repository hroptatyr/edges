require(edges)

x <- factor(c(NA, NA, "bar"))
y <- factor(c("foo", NA, "zar"))
coalesce(x, y)

x <- as.Date(c(NA, NA, "2020-01-01"))
y <- as.Date(c("2017-01-01", NA, "2022-01-01"))
coalesce(x, y)

x <- c(NA, NA, "EUR", "EUR", "INR", "EUR", "EUR", "INR")
edges(x)
