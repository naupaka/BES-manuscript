# Makefile for BES-manuscript
# This Makefile handles the complete workflow from data processing to PDF generation

# Variables
R_SCRIPT := 00-start-here.R
MAIN_QMD := BES-manuscript.qmd
SUPP_QMD := BES-manuscript-supp.qmd
MAIN_PDF := BES-manuscript.pdf
SUPP_PDF := BES-manuscript-supp.pdf
FIGURES_DIR := figures
DATA_DIR := data/derived

# R processing files (in order of execution)
R_PROCESS_FILES := R/process/01-process-neonSoilFlux.R \
                   R/process/02-process-field-licor.R \
                   R/process/03-field-neonSoilFlux-combine.R \
                   R/process/04-extract-diffusivity-field.R \
                   R/process/f4-flux-results.R \
                   R/process/f5-flux-results-year.R \
                   R/process/f6-r2-plot.R \
                   R/process/f7-diffusivity-plot.R \
                   R/process/sf1-gap-filled-stats.R \
                   R/process/sf2-uncertainty-stats.R

# Generated data files
DERIVED_DATA := $(DATA_DIR)/all-year-flux-results.Rda \
                $(DATA_DIR)/combined-field-data.Rda \
                $(DATA_DIR)/diffusivity-gradient.Rda \
                $(DATA_DIR)/field-data-info.Rda \
                $(DATA_DIR)/licor-all-data.Rda

# Generated figure files
FIGURES := $(FIGURES_DIR)/diffusivity-plot.png \
           $(FIGURES_DIR)/flux-results-year.png \
           $(FIGURES_DIR)/flux-results.png \
           $(FIGURES_DIR)/gap-filled-stats.png \
           $(FIGURES_DIR)/r2-plot.png \
           $(FIGURES_DIR)/uncertainty-stats.png

# Default target
.PHONY: all
all: $(MAIN_PDF) $(SUPP_PDF)

# Main manuscript PDF
$(MAIN_PDF): $(MAIN_QMD) $(FIGURES) $(DERIVED_DATA)
	@echo "Rendering main manuscript..."
	quarto render $(MAIN_QMD)

# Supplemental PDF
$(SUPP_PDF): $(SUPP_QMD) $(FIGURES) $(DERIVED_DATA)
	@echo "Rendering supplemental material..."
	quarto render $(SUPP_QMD)

# Create a single target that generates all R outputs
.PHONY: r-outputs
r-outputs: $(R_SCRIPT) $(R_PROCESS_FILES)
	@echo "Running R processing scripts..."
	@Rscript $(R_SCRIPT)

# Make all derived data and figures depend on the R outputs target
$(DERIVED_DATA) $(FIGURES): r-outputs

# Individual targets for specific outputs
.PHONY: data
data: $(DERIVED_DATA)

.PHONY: figures
figures: $(FIGURES)

.PHONY: main
main: $(MAIN_PDF)

.PHONY: supp
supp: $(SUPP_PDF)

# Clean targets
.PHONY: clean
clean:
	@echo "Cleaning generated files..."
	rm -f $(MAIN_PDF) $(SUPP_PDF)
	rm -f $(MAIN_QMD:.qmd=.tex) $(SUPP_QMD:.qmd=.tex)
	rm -f $(FIGURES)
	rm -f $(DERIVED_DATA)

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
	rm -f $(MAIN_PDF) $(SUPP_PDF)
	rm -f $(MAIN_QMD:.qmd=.tex) $(SUPP_QMD:.qmd=.tex)

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
	@echo "  all          - Build both main manuscript and supplemental PDFs (default)"
	@echo "  main         - Build main manuscript PDF only"
	@echo "  supp         - Build supplemental PDF only"
	@echo "  data         - Run R scripts to generate derived data files"
	@echo "  figures      - Run R scripts to generate figure files"
	@echo "  r-outputs    - Run all R processing scripts"
	@echo "  clean        - Remove all generated files"
	@echo "  clean-figures - Remove generated figure files"
	@echo "  clean-data   - Remove generated data files"
	@echo "  clean-pdf    - Remove generated PDF files"
	@echo "  force-all    - Clean and rebuild everything"
	@echo "  force-main   - Clean and rebuild main manuscript"
	@echo "  force-supp   - Clean and rebuild supplemental material"
	@echo "  check-deps   - Check if required dependencies are installed"
	@echo "  install-r-packages - Install required R packages"
	@echo "  dev-setup    - Check dependencies and install R packages"
	@echo "  watch        - Watch for changes and rebuild (requires entr)"
	@echo "  help         - Show this help message"
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
	Rscript -e "install.packages(c('tidyverse', 'lubridate', 'broom', 'grid', 'gridExtra', 'gtable', 'gt', 'sf', 'jsonlite', 'lutz', 'neonSoilFlux', 'neonUtilities'), repos='https://cran.r-project.org')"

# Development targets
.PHONY: dev-setup
dev-setup: check-deps install-r-packages
	@echo "Development environment setup complete"

# Watch for changes and rebuild (requires entr or similar)
.PHONY: watch
watch:
	@echo "Watching for changes... (requires 'entr' to be installed)"
	@echo "Press Ctrl+C to stop"
	find . -name "*.R" -o -name "*.qmd" -o -name "*.yml" | entr -c make all
