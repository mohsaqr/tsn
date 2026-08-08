test_that("runtime package declarations are base R only", {
  imports <- trimws(strsplit(packageDescription("tsn")$Imports, ",")[[1L]])
  expect_setequal(imports, c("graphics", "grDevices", "stats", "utils"))
})

test_that("source contains no forbidden for loops", {
  source_directory <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(source_directory))
  files <- list.files(source_directory,
                      pattern = "[.]R$", full.names = TRUE)
  source <- unlist(lapply(files, readLines, warn = FALSE), use.names = FALSE)
  expect_false(any(grepl("\\bfor\\s*\\(", source, perl = TRUE)))
})

test_that("shipped tests contain no developer-specific absolute paths", {
  tests_directory <- testthat::test_path(".")
  test_files <- list.files(
    tests_directory,
    pattern = "[.]R$",
    full.names = TRUE
  )
  test_source <- unlist(
    lapply(test_files, readLines, warn = FALSE),
    use.names = FALSE
  )
  developer_root <- paste0("/", "Users", "/")

  expect_false(any(grepl(developer_root, test_source, fixed = TRUE)))
})

test_that("CRAN metadata and source exclusions stay submission-ready", {
  source_root <- testthat::test_path("..", "..")
  ignore_path <- file.path(source_root, ".Rbuildignore")
  description_path <- file.path(source_root, "DESCRIPTION")
  license_path <- file.path(source_root, "LICENSE")
  skip_if_not(
    file.exists(ignore_path) &&
      file.exists(description_path) &&
      file.exists(license_path)
  )

  exclusions <- readLines(ignore_path, warn = FALSE)
  required_exclusions <- c(
    "^\\.DS_Store$",
    "^tests/\\.DS_Store$",
    "^tests/testthat/testthat-problems\\.rds$",
    "^tests/testthat/_problems$",
    "^tests/testthat/_snaps$",
    "^tsn_[0-9].*\\.tar\\.gz$"
  )
  description <- read.dcf(description_path)
  license <- readLines(license_path, warn = FALSE)

  expect_true(all(required_exclusions %in% exclusions))
  expect_setequal(
    trimws(strsplit(description[1L, "VignetteBuilder"], ",")[[1L]]),
    c("knitr", "rmarkdown")
  )
  expect_match(description[1L, "Authors@R"], "\"cph\"", fixed = TRUE)
  expect_false(any(grepl("tsn authors", license, fixed = TRUE)))
  expect_match(
    description[1L, "Description"],
    "<doi:10.1073/pnas.0709247105>",
    fixed = TRUE
  )
  expect_match(
    description[1L, "Description"],
    "<doi:10.1103/PhysRevE.80.046103>",
    fixed = TRUE
  )
})
