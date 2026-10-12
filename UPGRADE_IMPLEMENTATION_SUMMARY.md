# SBDA Survey Data Analytics — UX, Data Literacy and Cleaning Upgrade

This package preserves the existing statistical and M&E capabilities while improving usability for beginner, intermediate and professional analysts.

## Main improvements

- Three analysis experience levels: Beginner, Intermediate and Professional. Authorization roles remain separate.
- Simplified beginner and intermediate navigation, plus multilingual sidebar search.
- Expanded English, French and Kinyarwanda navigation translations.
- Persistent workspace/dataset context bar and a context-aware “What should I do next?” action.
- Guided row operations: remove first/last rows, exact row indexes, row ranges, or keep only a range.
- Guided column operations: delete selected, keep selected, or move selected columns to the front.
- Preview-first cleaning with before/after dimensions and the existing undo/history workflow.
- Variable profiling for numeric and categorical fields with missingness, uniqueness, distributions and responsible warnings.
- Dashboard direct manipulation: drag-and-drop visual ordering, width cycling, Undo/Redo and chart-choice guidance.
- Packaging hygiene: local environment secrets, session workspaces and the machine-specific `renv` package library are excluded.

## Statistical safeguards

- Raw imported data are not silently translated or overwritten.
- Potential outliers are flagged for investigation rather than automatically deleted.
- Proportions should be interpreted with explicit denominators and missingness.
- Survey weights, strata, clusters and identifier variables receive cautionary treatment during destructive cleaning.
- Cleaning actions remain previewable and reproducible through the existing cleaning history/recipe architecture.

## Validation completed in the build environment

- Project static release validator: PASS, no warnings/errors.
- Navigation internationalization regression: PASS.
- JavaScript syntax check: PASS.
- Static delimiter checks on modified R source files: PASS.

## Runtime validation note

An R interpreter is not installed in the build environment, so the Shiny application and `testthat` suite could not be executed end-to-end here. Run the normal project startup and R test suite on a machine with the project R dependencies installed before production deployment.
