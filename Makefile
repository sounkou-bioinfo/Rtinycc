# h/t to @jimhester and @yihui for this parse block:
# https://github.com/yihui/knitr/blob/dc5ead7bcfc0ebd2789fe99c527c7d91afb3de4a/Makefile#L1-L4
# Note the portability change as suggested in the manual:
# https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Writing-portable-packages
PKGNAME := $(shell sed -n 's/Package: *\([^ ]*\)/\1/p' DESCRIPTION)
PKGVERS := $(shell sed -n 's/Version: *\([^ ]*\)/\1/p' DESCRIPTION)

FLATBUF_SRC := tools/flatbuf
FLATBUF_DST := inst/go/vendor/github.com/apache/arrow/go/v18/arrow/internal/flatbuf

restore-flatbuf:
	@if [ -d "$(FLATBUF_SRC)" ] && [ ! -d "$(FLATBUF_DST)" ]; then \
	  echo "Restoring flatbuf files to $(FLATBUF_DST)"; \
	  mkdir -p "$(FLATBUF_DST)"; \
	  cp -r "$(FLATBUF_SRC)"/* "$(FLATBUF_DST)/"; \
	fi


all: check



rd:
	R -e 'roxygen2::roxygenize()'
build:  install_deps
	R CMD build .

check: build
	R CMD check --as-cran --no-manual $(PKGNAME)_$(PKGVERS).tar.gz

install_deps:
	R \
	-e 'if (!requireNamespace("remotes")) install.packages("remotes")' \
	-e 'remotes::install_deps(dependencies = TRUE)'

install: build
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz
install2: restore-flatbuf
	R CMD INSTALL --no-configure .
clean:
	@rm -rf $(PKGNAME)_$(PKGVERS).tar.gz $(PKGNAME).Rcheck

# Development targets
dev-install:
	R CMD INSTALL --preclean .

test: install
	R -e "tinytest::test_package('$(PKGNAME)', testdir = 'inst/tinytest')"

rdm: install
	R -e "rmarkdown::render('README.Rmd')"
.PHONY: all rd build check install_deps install clean dev-install dev-test dev-preprocess-test dev-parse-test dev-all-tests
