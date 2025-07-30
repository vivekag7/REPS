# REPS 1.0.0

**Note:** This release marks the rebranding and official first major release of the package.  
The package was previously known as `cbsREPS` (version 0.1.0) and is now renamed to `REPS`.

---

## Major Features

### New Index Methods Supported in `calculate_price_index()`
- Added support for three additional index calculation methods:
  - Repricing method (hedonic geometric adjustment)
  - Time Dummy index
  - Rolling Time Dummy index
- These methods can now be called individually or together via the `method` argument.
- Example:
  ```r
  calculate_price_index(..., method = c("fisher", "laspeyres", "repricing"))
  ```

### Multi-Method Execution and Output
- When passing multiple methods, the function returns a named list of outputs.
- Enables seamless comparison of different price index methodologies in a single step.

### New Plotting Utilities
- `plot_price_index()` introduced for quick visualization of index results.
  - Automatically detects multi-method input.
  - Clearly labeled plots with period breaks for time-based readability.

---

## Regression Diagnostics

### New Function: `calculate_regression_diagnostics()`
- Computes diagnostic statistics for log-linear hedonic models per period:
  - Adjusted R²
  - Shapiro-Wilk normality test
  - Durbin-Watson test for autocorrelation
  - Breusch-Pagan test for heteroscedasticity

### New Function: `plot_regression_diagnostics()`
- Visualizes diagnostics over time in a 3×2 grid.
- Includes threshold markers to easily spot anomalies or violations of assumptions.

---

## Bug Fixes and Compatibility

- Improved input validation in `validate_input()`:
  - Clear distinction between errors and warnings.
  - Accepts period formats like `"202101"` and `"2020Q1"`.
- Removed dependency on `assertthat`; replaced with native R error handling.
- Guardrails added to prevent invalid combinations (e.g., `resting_points = TRUE` with multiple methods).

---

## Documentation and Testing

- Comprehensive updates to documentation, examples, and function manuals.
- New vignette: `calculate_regression_diagnostics` showcasing diagnostics workflow.
- Expanded test coverage across all new features to ensure stability and correctness.

---

REPS 1.0.0 sets the stage for scalable, reproducible price index analysis. Whether you’re working with Fisher, Laspeyres, or regression-based methods, this version provides you with the tools needed to compute, compare, and interpret indices with ease and statistical rigor.

