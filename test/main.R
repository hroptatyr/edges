require(myR)

x <- factor(c(NA, NA, "bar"))
y <- factor(c("foo", NA, "zar"))
coalesce(x, y)
