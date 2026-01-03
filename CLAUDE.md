# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`dmisc` is a personal R utility package containing helper functions, templates, and tools for statistical analysis. The package focuses on:
- Survival analysis utilities (Cox model predictions)
- Spline modeling helpers (knot selection)
- Project scaffolding functions
- RMarkdown report templates

## Development Commands

### Testing
```r
# Run all tests
devtools::test()

# Run specific test file
devtools::test_active_file("tests/testthat/test-knots.R")

# Run tests with coverage
covr::package_coverage()
```

### Documentation
```r
# Regenerate documentation from roxygen2 comments
devtools::document()

# Load package for interactive development
devtools::load_all()

# Check package (runs tests, checks documentation, etc.)
devtools::check()
```

### Build and Install
```r
# Install package locally
devtools::install()

# Build package
devtools::build()
```

## Code Architecture

### Function Categories

**Statistical Analysis Functions** (`R/adjusted_survival_curves.R`, `R/knots.R`)
- Core utility functions for statistical modeling
- Pattern: Comprehensive input validation with `stopifnot()` and `stop()` for clear error messages
- Return data structures: data frames with snake_case column names or numeric vectors
- Heavy use of tidyverse packages (dplyr, purrr, tidyr)

**Project Scaffolding** (`R/statistical_analysis_template.R`, `R/report_template.R`)
- Functions that create project directories and templates
- Use `withr::with_dir()` for directory context management
- Integrate with RMarkdown templates in `inst/rmarkdown/templates/`
- RStudio project templates in `inst/rstudio/templates/project/`

### Documentation Style

All exported functions follow consistent roxygen2 patterns:
- Title: One-line description
- Description: 2-3 sentence detailed explanation
- `@param`: Each parameter with type and NA handling behavior
- `@return`: Clear description of return value structure
- `@details`: Technical explanations and algorithm details
- `@references`: Citations for statistical methods (especially Frank Harrell's work)
- `@examples`: 2-3 practical examples showing typical usage
- `@export`: Required for public API functions

### Testing Conventions

Tests use `testthat` edition 3 with descriptive test names following the pattern:
- `test_that("function does X when Y", { ... })`
- Group related tests by functionality
- Always test: basic functionality, edge cases, NA handling, input validation errors
- Use `set.seed()` for reproducible random data in tests
- Test both success paths and error conditions with `expect_error()`

## Package Dependencies

**Core imports**: dplyr, purrr, survival, tidyr, stringr, withr, usethis, renv, quarto, rmarkdown

When adding functions:
- Survival analysis → use `survival` package (already imported)
- Spline modeling → use base `stats::quantile()` and reference `splines` package in docs
- Data manipulation → use tidyverse functions (dplyr/tidyr/purrr already imported)
- For new dependencies, add to DESCRIPTION Imports and document why needed

## Key Conventions

- **No over-engineering**: Keep functions focused and simple; avoid premature abstractions
- **Quantile-based methods**: Prefer quantile placements over evenly-spaced values for splines
- **Extensibility**: Use `match.arg()` for strategy parameters to support future additions
- **Error messages**: Quote parameter names in errors (e.g., "'x' must be numeric")
- **NA handling**: Remove NAs with clear messaging, validate after removal
