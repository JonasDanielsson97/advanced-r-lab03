Generate documentation and NAMESPACE from roxygen comments:
`devtools::document()`

Creates README:
`usethis::use_readme_md()`

Add package dependency into `DESCRIPTION`:
`usethis::use_package("tidyverse")`

Add license:
`usethis::use_mit_license()`

Add way to add data as an `.rda`-file:
`usethis::use_data_raw()`
    `usethis::use_data(data)` usaly auto-generated from `usethis::use_data_raw()`
    and creates the .`rda`-datafile from whatever is in that script, or I guess environment.




Creates `.yaml`-file so that check works:
`usethis::use_github_action("check-standard")`

Set up `testthat`:
usethis::use_testthat()

Run tests:
`devtools::test()`

Run check
`devtools::check()`

load package:
`devtools::load_all()`

