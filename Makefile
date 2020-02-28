all:
	make book
	make code

book:
	Rscript -e "bookdown::render_book('index.Rmd')"

code:
	Rscript -e "sapply(list.files(pattern = 'Rmd'), knitr::purl)"
