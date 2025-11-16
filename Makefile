# Makefile for BES-manuscript
# This Makefile handles the complete workflow from data processing to PDF generation

# Variables
MAIN_QMD := BES-manuscript.qmd
SUPP_QMD := BES-manuscript-supp.qmd
TITLE_QMD := BES-manuscript-title-page.qmd
MAIN_PDF := BES-manuscript.pdf
SUPP_PDF := BES-manuscript-supp.pdf
TITLE_PDF := BES-manuscript-title-page.pdf
MAIN_TEX := BES-manuscript.tex
SUPP_TEX := BES-manuscript-supp.tex
TITLE_TEX := BES-manuscript-title-page.tex
FIGURES_DIR := figures
FUNCTIONS_DIR := R/functions
DATA_DIR := data/derived

# Environmental and flux data files
ENV_FLUX_DATA := $(wildcard data/raw/flux-data/*.Rda)

# Default commit for latexdiff (can be overridden)
DIFF_COMMIT ?= HEAD~1

# R function files
R_FUNCTIONS := $(wildcard $(FUNCTIONS_DIR)/*.R)

# R processing scripts
R_PROCESS_01 := R/process/01-process-neonSoilFlux.R
R_PROCESS_02 := R/process/02-process-field-licor.R
R_PROCESS_03 := R/process/03-field-neonSoilFlux-combine.R
R_PROCESS_04 := R/process/04-extract-diffusivity-field.R
R_PROCESS_F4 := R/process/f4-flux-results.R
R_PROCESS_F5 := R/process/f5-flux-results-year.R
R_PROCESS_F6 := R/process/f6-r2-plot.R
R_PROCESS_F7 := R/process/f7-diffusivity-plot.R
R_PROCESS_SF1 := R/process/sf1-gap-filled-stats.R
R_PROCESS_SF2 := R/process/sf2-uncertainty-stats.R

# Generated data files
# Note: all-year-flux-results.Rda can be added to dependencies to test full rebuilds
DATA_ALL_YEAR := $(DATA_DIR)/all-year-flux-results.Rda
DATA_LICOR := $(DATA_DIR)/licor-all-data.Rda
DATA_FIELD_INFO := $(DATA_DIR)/field-data-info.Rda
DATA_COMBINED := $(DATA_DIR)/combined-field-data.Rda
DATA_DIFFUSIVITY := $(DATA_DIR)/diffusivity-gradient.Rda

# All derived data files (for convenience)
DERIVED_DATA := $(DATA_ALL_YEAR) \
                $(DATA_LICOR) \
                $(DATA_FIELD_INFO) \
                $(DATA_COMBINED) \
                $(DATA_DIFFUSIVITY)

# Generated figure files
FIGURES := $(FIGURES_DIR)/diffusivity-plot.png \
           $(FIGURES_DIR)/flux-results-year.png \
           $(FIGURES_DIR)/flux-results.png \
           $(FIGURES_DIR)/gap-filled-stats.png \
           $(FIGURES_DIR)/r2-plot.png \
           $(FIGURES_DIR)/uncertainty-stats.png

# Default target
.PHONY: all
all: $(MAIN_PDF) $(SUPP_PDF) $(TITLE_PDF)

# Title page PDF
$(TITLE_PDF): $(TITLE_QMD) bes-bibliography.bib methods-in-ecology-and-evolution.csl
	@echo "Rendering title page..."
	quarto render $(TITLE_QMD)

# Main manuscript PDF
$(MAIN_PDF): $(MAIN_QMD) figures/collar-images.jpeg figures/model-diagram.pdf figures/neonSoilFluxOutline.png $(FIGURES_DIR)/diffusivity-plot.png $(FIGURES_DIR)/flux-results-year.png $(FIGURES_DIR)/flux-results.png $(FIGURES_DIR)/r2-plot.png $(DATA_LICOR) bes-bibliography.bib methods-in-ecology-and-evolution.csl
	@echo "Rendering main manuscript..."
	quarto render $(MAIN_QMD)

# Generate title .tex file if it doesn't exist
$(TITLE_TEX): $(TITLE_QMD) bes-bibliography.bib methods-in-ecology-and-evolution.csl
	@echo "Rendering title page to generate .tex file..."
	quarto render $(TITLE_QMD)

# Generate manuscript .tex file if it doesn't exist
$(MAIN_TEX): $(MAIN_QMD) bes-bibliography.bib methods-in-ecology-and-evolution.csl
	@echo "Rendering manuscript to generate .tex file..."
	quarto render $(MAIN_QMD)

# Generate supplement .tex file if it doesn't exist
$(SUPP_TEX): $(SUPP_QMD) bes-bibliography.bib methods-in-ecology-and-evolution.csl
	@echo "Rendering supplemental material to generate .tex file..."
	quarto render $(SUPP_QMD)

# Supplemental PDF
$(SUPP_PDF): $(SUPP_QMD) $(FIGURES_DIR)/uncertainty-stats.png $(FIGURES_DIR)/gap-filled-stats.png bes-bibliography.bib methods-in-ecology-and-evolution.csl
	@echo "Rendering supplemental material..."
	quarto render $(SUPP_QMD)

# Generated data files with their source scripts
$(DATA_ALL_YEAR): $(R_PROCESS_01)
	@echo "Processing all-year NEON flux results..."
	Rscript $(R_PROCESS_01)

$(DATA_FIELD_INFO) $(DATA_LICOR): $(DATA_ALL_YEAR) $(R_PROCESS_02) $(R_FUNCTIONS)
	@echo "Processing LICOR data and generating field data info..."
	Rscript $(R_PROCESS_02)

$(DATA_COMBINED): $(DATA_ALL_YEAR) $(DATA_FIELD_INFO) $(DATA_LICOR) $(R_PROCESS_03)
	@echo "Combining field and NEON data..."
	Rscript $(R_PROCESS_03)

$(DATA_DIFFUSIVITY): $(DATA_FIELD_INFO) $(DATA_COMBINED) $(R_PROCESS_04)
	@echo "Extracting diffusivity gradient data..."
	Rscript $(R_PROCESS_04)

# Generated figure files with their source scripts
$(FIGURES_DIR)/flux-results.png: $(DATA_COMBINED) $(DATA_FIELD_INFO) $(R_PROCESS_F4)
	@echo "Generating flux results plot..."
	Rscript $(R_PROCESS_F4)

$(FIGURES_DIR)/flux-results-year.png: $(DATA_COMBINED) $(DATA_FIELD_INFO) $(R_PROCESS_F5)
	@echo "Generating annual flux results plot..."
	Rscript $(R_PROCESS_F5)

$(FIGURES_DIR)/r2-plot.png: $(DATA_COMBINED) $(DATA_FIELD_INFO) $(R_PROCESS_F6)
	@echo "Generating R2 plot..."
	Rscript $(R_PROCESS_F6)

$(FIGURES_DIR)/diffusivity-plot.png: $(DATA_DIFFUSIVITY) $(R_PROCESS_F7)
	@echo "Generating diffusivity plot..."
	Rscript $(R_PROCESS_F7)

$(FIGURES_DIR)/gap-filled-stats.png: $(DATA_COMBINED) $(DATA_FIELD_INFO) $(R_PROCESS_SF1)
	@echo "Generating gap-filled stats plot..."
	Rscript $(R_PROCESS_SF1)

$(FIGURES_DIR)/uncertainty-stats.png: $(DATA_COMBINED) $(DATA_FIELD_INFO) $(R_PROCESS_SF2)
	@echo "Generating uncertainty stats plot..."
	Rscript $(R_PROCESS_SF2)


# Individual targets for specific outputs
.PHONY: data
data: $(DERIVED_DATA)

.PHONY: env-and-flux-data
env-and-flux-data: $(ENV_FLUX_DATA)

.PHONY: figures
figures: $(FIGURES)

.PHONY: main
main: $(MAIN_PDF)

.PHONY: supp
supp: $(SUPP_PDF)

# LaTeX diff target
.PHONY: diff
diff: $(MAIN_TEX) $(SUPP_TEX)
	@echo "Creating diff PDF against commit $(DIFF_COMMIT)..."
	latexdiff-vc --git -r $(DIFF_COMMIT) --pdf $(MAIN_TEX)
	latexdiff-vc --git -r $(DIFF_COMMIT) --pdf $(SUPP_TEX)

# Clean targets
.PHONY: clean
clean:
	@echo "Cleaning generated files..."
	rm -f $(MAIN_PDF) $(SUPP_PDF)
	rm -f $(MAIN_QMD:.qmd=.tex) $(SUPP_QMD:.qmd=.tex)
	rm -f $(FIGURES)
	rm -f $(DERIVED_DATA)
	echo rm -f *-diff*

.PHONY: clean-figures
clean-figures:
	@echo "Cleaning figures..."
	rm -f $(FIGURES)

.PHONY: clean-data
clean-data:
	@echo "Cleaning derived data..."
	rm -f $(DERIVED_DATA)

.PHONY: clean-pdf
clean-pdf:
	@echo "Cleaning PDF files..."
	rm -f $(MAIN_PDF) $(SUPP_PDF) ${TITLE_PDF}
	rm -f $(MAIN_QMD:.qmd=.tex) $(SUPP_QMD:.qmd=.tex) $(TITLE_QMD:.qmd=.tex)

.PHONY: clean-diff
clean-diff:
	@echo "Cleaning diff TeX and PDFs..."
	rm -f *-diff*

.PHONY: deep-clean
deep-clean:
	@echo "Performing deep clean of all generated files including NEON downloads and calculated fluxes..."
	rm -f $(MAIN_PDF) $(SUPP_PDF)
	rm -f $(MAIN_QMD:.qmd=.tex) $(SUPP_QMD:.qmd=.tex)
	rm -f $(FIGURES)
	rm -f $(DERIVED_DATA)
	rm -f $(ENV_FLUX_DATA)
	rm -f *-diff*

# Force rebuild targets
.PHONY: force-all
force-all: clean all

.PHONY: force-main
force-main: clean-pdf main

.PHONY: force-supp
force-supp: clean-pdf supp

# Help target
.PHONY: help
help:
	@echo "Available targets:"
	@echo "  all           - Build both main manuscript and supplemental PDFs (default)"
	@echo "  main          - Build main manuscript PDF only"
	@echo "  supp          - Build supplemental PDF only"
	@echo "  data          - Generate all derived data files"
	@echo "  figures       - Generate all figure files"
	@echo "  diff          - Create latexdiff PDF (default: against HEAD~1)"
	@echo "  clean         - Remove all generated files"
	@echo "  clean-figures - Remove generated figure files"
	@echo "  clean-data    - Remove generated data files"
	@echo "  clean-pdf     - Remove generated PDF files"
	@echo "  deep-clean    - Remove all generated files including downloaded NEON data and calculated fluxes"
	@echo "  force-all     - Clean and rebuild everything"
	@echo "  force-main    - Clean and rebuild main manuscript"
	@echo "  force-supp    - Clean and rebuild supplemental material"
	@echo "  check-deps    - Check if required dependencies are installed"
	@echo "  install-r-packages - Install required R packages"
	@echo "  dev-setup     - Check dependencies and install R packages"
	@echo "  help          - Show this help message"
	@echo ""
	@echo "Individual data files:"
	@echo "  $(DATA_ALL_YEAR)"
	@echo "  $(DATA_LICOR)"
	@echo "  $(DATA_FIELD_INFO)"
	@echo "  $(DATA_COMBINED)"
	@echo "  $(DATA_DIFFUSIVITY)"
	@echo ""
	@echo "Individual figure files:"
	@echo "  $(FIGURES_DIR)/diffusivity-plot.png"
	@echo "  $(FIGURES_DIR)/flux-results-year.png"
	@echo "  $(FIGURES_DIR)/flux-results.png"
	@echo "  $(FIGURES_DIR)/gap-filled-stats.png"
	@echo "  $(FIGURES_DIR)/r2-plot.png"
	@echo "  $(FIGURES_DIR)/uncertainty-stats.png"
	@echo ""
	@echo "Usage examples:"
	@echo "  make diff                    # Diff against previous commit"
	@echo "  make diff DIFF_COMMIT=4308241b8  # Diff against specific commit"
	@echo "  make diff DIFF_COMMIT=HEAD~5     # Diff against 5 commits ago"
	@echo ""
	@echo "Dependencies:"
	@echo "  - R with required packages (tidyverse, lubridate, broom, etc.)"
	@echo "  - Quarto for PDF rendering"
	@echo "  - LaTeX for PDF generation"

# Check dependencies
.PHONY: check-deps
check-deps:
	@echo "Checking dependencies..."
	@command -v R >/dev/null 2>&1 || { echo "Error: R is not installed or not in PATH"; exit 1; }
	@command -v quarto >/dev/null 2>&1 || { echo "Error: Quarto is not installed or not in PATH"; exit 1; }
	@echo "Dependencies check passed"

# Install R packages (optional target)
.PHONY: install-r-packages
install-r-packages:
	@echo "Installing required R packages..."
	Rscript -e "install.packages(c('tidyverse', 'lubridate', 'broom', 'grid', 'gridExtra', 'gtable', 'gt', 'sf', 'jsonlite', 'lutz', 'neonSoilFlux', 'neonUtilities', 'doParallel', 'foreach'), repos='https://cran.r-project.org')"

# Development targets
.PHONY: dev-setup
dev-setup: check-deps install-r-packages
	@echo "Development environment setup complete"
