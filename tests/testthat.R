library(testthat)
library(devtools)
library(assertive.reflection)

with_envvar(
  c(LANG = "en_US"),
  test_check("assertive.reflection")
)
