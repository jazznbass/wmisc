
- new functions: `change_values()`

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

