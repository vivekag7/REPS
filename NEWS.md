# REPS 1.1.1

## Bug Fixes
* Fixed minor bugs related to parameter handling and data type coercion across various hedonic index calculation methods.
* Added an explicit error catch to the plotting function to prevent execution when `resting_points = TRUE` is incorrectly passed.

## Internal Improvements
* **Major Architectural Refactoring:** Extracted the linear model fitting and prediction logic from individual hedonic methods into centralized, high-performance internal helpers (`fit_hedonic_model` and `format_index_output`)
* Standardized internal data preparation and final index output formatting across Laspeyres, Paasche, Fisher, Time Dummy, and Repricing methods to guarantee strict consistency in output structures. 
* Refactoring maintains exact API compatibility with `v1.1.0` while significantly improving code maintainability and speed