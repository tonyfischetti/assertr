
.DELETE_ON_ERROR:


.PHONY: all clean mrproper dist check test install begin help done verify

load_all:
	Rscript -e 'devtools::load_all(".")'

install:
	cd ../ && R CMD INSTALL --preclean --no-multiarch --with-keep.source assertr

test:
	Rscript -e 'devtools::test()'

check:
	Rscript -e 'devtools::check(document = FALSE, args = c("--no-manual"))'

check_doc:
	Rscript -e 'devtools::check_man()'

check_rhub:
	Rscript -e 'devtools::check_rhub()'

check_win_devel:
	Rscript -e 'devtools::check_win_devel()'

check_win_release:
	Rscript -e 'devtools::check_win_release()'

check_win_oldrelease:
	Rscript -e 'devtools::check_win_oldrelease()'

build_vignettes:
	Rscript -e 'devtools::build_vignettes()'

build_manual:
	Rscript -e 'devtools::build_manual()'

document:
	Rscript -e 'devtools::document()'

build_src:
	Rscript -e 'devtools::build()'

build_bin:
	Rscript -e 'devtools::build(binary = TRUE, args = c("--preclean"))'

