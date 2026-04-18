# h/t to @jimhester and @yihui for this parse block:
# https://github.com/yihui/knitr/blob/dc5ead7bcfc0ebd2789fe99c527c7d91afb3de4a/Makefile#L1-L4
# Note the portability change as suggested in the manual:
# https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Writing-portable-packages
PKGNAME := $(shell sed -n 's/Package: *\([^ ]*\)/\1/p' DESCRIPTION)
PKGVERS := $(shell sed -n 's/Version: *\([^ ]*\)/\1/p' DESCRIPTION)


all: check



rd:
	R -e 'roxygen2::roxygenize(load_code = "source")'
vig:
	R -e "tools::buildVignettes(dir = '.')"
vig-md:
	R -e "for (f in Sys.glob('vignettes/*.Rmd')) { out <- sub('\\\\.Rmd$$', '.md', f); rmarkdown::render(f, output_format = rmarkdown::md_document(variant = 'gfm'), output_file = basename(out), output_dir = dirname(out), quiet = FALSE, envir = new.env(parent = globalenv())) }"
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
install2:
	R CMD INSTALL --no-configure .

install3:
	R CMD INSTALL .
clean:
	@rm -rf $(PKGNAME)_$(PKGVERS).tar.gz $(PKGNAME).Rcheck

# Development targets
dev-install:
	R CMD INSTALL --preclean .
dev-install-debug-win:
	RTINYCC_DEBUG_BUILD=1 R CMD INSTALL --preclean .

test1: 
	R -e "tinytest::test_package('$(PKGNAME)', testdir = 'inst/tinytest', ncpu=1L)"
test2: 
	R -e "tinytest::test_package('$(PKGNAME)', testdir = 'inst/tinytest', ncpu=2L)"
test0:
	R -e "tinytest::test_package('$(PKGNAME)', testdir = 'inst/tinytest')"
test: install
	R -e "tinytest::test_package('$(PKGNAME)', testdir = 'inst/tinytest')"

rdm: install
	R -e "rmarkdown::render('README.Rmd')"
.PHONY: all rd vig vig-md build check install_deps install clean dev-install dev-test dev-preprocess-test dev-parse-test dev-all-tests
