.DEFAULT_GOAL := help

RMD := main-script.Rmd
DOCX := manuscript/betadiv-enp.docx
IMAGE := kguidonimartins/betadiv-enp

.PHONY: analysis manuscript convert-pdf clean_cache

all: clean_cache analysis manuscript ## run report and convert-pdf targets

manuscript: ## compile manuscript
	Rscript -e "rmarkdown::render(\
	input = '$(RMD)', \
	output_format = 'all', \
	output_file = '$(DOCX)' \
	)"

analysis: ## compile analysis document as output/analysis.html
	Rscript -e "rmarkdown::render(input = 'R/analysis.Rmd', params = list(show_results = TRUE), output_dir = 'output')"

convert-pdf: ## convert manuscript to a pdf file
	lowriter --convert-to pdf $(DOCX) --outdir manuscript

docker_build: ## build docker image
	docker build -t $(IMAGE) .

docker_run: ## run docker image and copy rendered to the current directory
	@echo "You need to run: docker run --rm -v `pwd`:/rendered kguidonimartins/betadiv-enp"
	docker run --rm -v `pwd`:/rendered $(IMAGE)

docker_push:  ## push docker image to dockerhub
	docker push $(IMAGE)

clean_cache: ## remove cache
	rm -rf main-script_cache
	rm -rf main-script_files
	rm -rf R/analysis_cache
	rm -rf output/analysis_files

help: ## show this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
