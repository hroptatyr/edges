R ?= R --vanilla

PKG = edges_0.1.0.tar.gz

.PHONY: all
all: build install

.PHONY: clean
clean:
	rm -f $(PKG)

.PHONY: build
build:
	$(R) CMD build . --no-build-vignettes

.PHONY: install
install: build
	$(R) CMD INSTALL $(PKG)

.PHONY: test
test:
	$(R) --vanilla -e 'devtools::test()'

.PHONY: check
check:
	$(R) CMD check $(PKG) --as-cran --ignore-vignettes --no-stop-on-test-error
