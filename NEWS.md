
# wmisc 0.2.25

## Changes

- `nice_frequencies()`: Now has a grouping argument. This allows to create contingency tables (e.g. `nice_frequencies(mtcars$cyl, mtcars$am)`).

# wmisc 0.2.24

## New Functions

- `nice_frequencies()`: Provides html and word tables for the frequency distribution of a variable.
- `nice_sem()`: Provides html and word tables for lavaan sem objects.
- `nice_regression_table()`: Provides html and word tables for one or multiple `lm`, `lme`, `lmerTest`, or `glmer` objects.
- `round_numeric()`: This function rounds numeric columns in a data frame to a specified number of digits.
- `logit2prob()`/ `prob2logit()`: Convert Logit to probability and vice versa
- `add_label()`: Adds haven labels. With a list option: `mtcars <- add_label(mtcars, list(cyl = "cylind", mpg = "Miles"))`
- `get_labels()`: Adds haven labels. With a list option: `mtcars <- add_label(mtcars, list(cyl = "cylind", mpg = "Miles"))`
- `nice_loadings()` extract loadings from psych::fa object
- `nice_efa()` Returns a nice table from an psych::fa object.
- `nice_agreement_table()` Returns a nice table for agreement_analyses.
- `flip()`: flip a data.frame or matrix: `flip(mtcars, rownames = TRUE)`
- `change_values()` recode values: `change_values(c(1, 2, 3), 2 ~ "two", 3 ~ "three")`
- `percentage_bar()` creates ggplot percentage bar: `percentage_bar(20, "test")`

## Changes

- added gt table support for nice table and set to default. Retiring "kable".


# wmisc 0.2.18

- new functions: `auto_corr()`
- new functions: `nice_statnum()`
- new functions: `nice_descriptives()`

# wmisc 0.2.13

- reworked `agreement_analysis()`

# wmisc 0.2.11

- new functions: `check(), start_check(). end_check(), check_within(), check_in(), check_not()`

# wmisc 0.2.10

- new function: `progress_feedback.R()`

# wmisc 0.2.9

- new function: `fill_missing_l2()`: Fills in missing data for cases in a multilevel/ 
repeated measurement long format data frame. This is useful when you have a variable
on level 2 (e.g., gender) but this variable only has a valid value for one measurement time (the others are na). This function will fill in the nas with this one valid value.


# wmisc 0.2.8

New function to extend Rmarkdown pages:  
`create_blogsite()`: Helps to create a blog-like site for Rmarkdown webpages
`new_blog_entry()`: Sets up a new blog entry markdown page

# wmisc 0.2.5

- new function: `chi_test_table()`: Compares the proportions of a dichotomos variable in two groups for multiple variables and puts the results into a table. Like t_test_table() put für dichtomous variables.

# wmisc 0.2.4

- `t_test_table()`: new arguments: caption, bootstrap_options, and full_width

# wmisc 0.2.3

- New `reference_package_version()`
- `alpha_table()`: argument `VAR` changed to `scales`. New arguements `difficulty`: if TRUE reports item difficulties. `values` min and max value for each scale as a vector needed for calculating item difficulty.

