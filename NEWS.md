# interfacer 0.2.1

* Added a `NEWS.md` file to track changes to the package.
* Initial CRAN submission.

# interfacer 0.2.2

* Fixed `README.md` URL issue and DESCRIPTION typo.
* automated spell test

# interfacer 0.2.3

* This is a resubmission to fix issues identified in CRAN submission v0.2.2: 1) 
additional spaces in DESCRIPTION file. 2) functions missing return values. 3) 
example containing unexported method. 4) `if (FALSE)` stanza in examples for 
function that can only be used interactively.

# interfacer 0.2.4

* Minor enhancement of type coercion to support more consistent behaviour for
finding custom `type.XX` functions in downstream packages.
* Funding statement added to README.

# interfacer 0.3.0

* Consistency checking and recycling for non-dataframe parameters API added, 
with new vignette to explain.
* More consistent `type.XX` function behaviour.

# interfacer 0.3.1

* Documentation tidy-up.
* Minor change to formatting of `iface` printing.

# interfacer 0.3.2

* Specific support for `unique_id` columns (ids unique between dataframe grouping).
* Fix issue with `imapper` defaults always being applied.