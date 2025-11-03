
## wmisc 0.3.6

### New functions

* `update_self()`: Checks for a wmisc update and installs it. 
* `nice_item_analysis()`: Returns a nice table for item analysis of single scales

## wmisc 0.3.4

### New functions

* `ci_score()`: Returns confidence intervals for test scores.
* `critical_difference()`: Returns critical difference for test scores.
* `nice_contingency_table()`: Creates a nicely formatted contingency table with one or more summary functions.
* `create_data_description()`: Creates a `README.md` file with a data description.

### Changes

* `nice_frequencies()`: Now has a `grouping` argument. This allows creating contingency tables (e.g. `nice_frequencies(mtcars$cyl, mtcars$am)`).
* `nice_table()`:
  * New argument `markdown`. If `TRUE`, interprets cell content as markdown.
  * Now a generic function with methods for `nice_regression_table()`, `nice_efa()`, and a default method for `data.frame` objects.
  * `rownames = NULL` now automatically shows row names unless they are `as.character(1:nrow(x))`.
  * New argument `sort` allows sorting by a character vector.
* `nice_regression_table()`: Now supports `gls` models.

### Corrections

* `nice_efa()`: now works for one factor solutions.

## wmisc 0.2.24

### New functions

* `nice_frequencies()`: Provides HTML and Word tables for the frequency distribution of a variable.
* `nice_sem()`: Provides HTML and Word tables for `lavaan` SEM objects.
* `nice_regression_table()`: Provides HTML and Word tables for `lm`, `lme`, `lmerTest`, or `glmer` models.
* `round_numeric()`: Rounds numeric columns in a data frame to a specified number of digits.
* `logit2prob()` / `prob2logit()`: Convert between logit and probability.
* `add_label()`: Adds haven labels. Accepts a list input, e.g., `mtcars <- add_label(mtcars, list(cyl = "cylind", mpg = "Miles"))`.
* `get_labels()`: Retrieves haven labels.
* `nice_loadings()`: Extracts loadings from a `psych::fa` object.
* `nice_efa()`: Returns a formatted table from a `psych::fa` object.
* `nice_agreement_table()`: Returns a formatted table for agreement analyses.
* `flip()`: Flips a data frame or matrix, e.g., `flip(mtcars, rownames = TRUE)`.
* `change_values()`: Recodes values using formula syntax, e.g., `change_values(c(1, 2, 3), 2 ~ "two", 3 ~ "three")`.
* `percentage_bar()`: Creates a ggplot percentage bar, e.g., `percentage_bar(20, "test")`.

### Changes

* `nice_table()`: Now defaults to `gt` tables; the older `kable` output is being retired.


## wmisc 0.2.18

### New functions

* `auto_corr()`
* `nice_statnum()`
* `nice_descriptives()`


## wmisc 0.2.13

### Changes

* Reworked `agreement_analysis()`


## wmisc 0.2.11

### New functions

* `check()`, `start_check()`, `end_check()`, `check_within()`, `check_in()`, `check_not()`


## wmisc 0.2.10

### New function

* `progress_feedback()`


## wmisc 0.2.9

### New function

* `fill_missing_l2()`: Fills in missing values in multilevel/repeated measurement long format data. Useful for level-2 variables like gender that may only appear in one measurement occasion.


## wmisc 0.2.8

### New functions

* `create_blogsite()`: Helps create a blog-like site for R Markdown pages.
* `new_blog_entry()`: Sets up a new blog entry markdown page.


## wmisc 0.2.5

### New function

* `chi_test_table()`: Compares the proportions of a dichotomous variable across two groups for multiple variables. Similar to `t_test_table()` but for categorical variables.


## wmisc 0.2.4

### Changes

* `t_test_table()`: Added arguments `caption`, `bootstrap_options`, and `full_width`.


## wmisc 0.2.3

### New function

* `reference_package_version()`

### Changes

* `alpha_table()`:
  * Argument `VAR` renamed to `scales`.
  * New argument `difficulty`: If `TRUE`, reports item difficulties.
  * New argument `values`: Required for calculating item difficulty; specify min and max values for each scale.
