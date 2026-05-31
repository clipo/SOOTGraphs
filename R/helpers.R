# Pure data-transformation helpers for the SOOT aggregator.
# Sourced by app.R; unit-tested in tests/testthat/test-helpers.R.

library(dplyr)
library(tidyr)
library(stringr)
library(tibble)

ANS_LEVELS <- c("Not Applicable", "Very Low or Never", "Low",
                "Average", "High", "Very High or Always")
TOP_BOX_LEVELS <- c("High", "Very High or Always")

INSTRUCTOR_QUESTIONS <- c(
  "The instructor is well prepared for class.",
  "The instructor demonstrates a thorough knowledge of the subject.",
  "The instructor communicates his/her subject well.",
  "The instructor explains complex ideas clearly.",
  "The instructor stimulates my interest in the core subject.",
  "The instructor is receptive to questions.",
  "The instructor is available to help me outside of class.",
  "The instructor encourages me to think analytically.",
  "Overall, the instructor is an effective teacher."
)

# Split an uploaded filename into course and term tokens.
# Pattern: COURSE-SECTION_TERM.ext, split on "-", "_", or ".".
parse_course_term <- function(filename) {
  parts <- unlist(str_split(filename, "-|_|\\."))
  list(course = parts[1], term = parts[3])
}

# Convert raw term tokens (e.g. "FALL24", "SPG25") into a chronological
# ordered factor. Sort key = year * 100 + season rank, spring before fall.
order_terms <- function(term_vector) {
  season_rank <- c(WIN = 0, SPG = 2, SPR = 2, SUM = 6, FALL = 9, FAL = 9)
  uniq <- unique(term_vector)
  season <- str_extract(uniq, "^[A-Za-z]+") |> toupper()
  year <- as.integer(str_extract(uniq, "[0-9]+$"))
  rank <- ifelse(season %in% names(season_rank), season_rank[season], 5)
  key <- year * 100 + as.integer(rank)
  ordered_levels <- uniq[order(key)]
  factor(term_vector, levels = ordered_levels, ordered = TRUE)
}

# Read one uploaded SOOT CSV into the canonical long format.
read_soot_csv <- function(path, name) {
  raw <- tryCatch(read.csv(path, header = TRUE, stringsAsFactors = FALSE),
                  error = function(e) NULL)
  if (is.null(raw) || nrow(raw) == 0) {
    warning(sprintf("Skipping empty or unreadable file: %s", name))
    return(NULL)
  }
  ct <- parse_course_term(name)
  tibble::as_tibble(raw) |>
    mutate(course = ct$course,
           term = ct$term,
           ANS_TEXT = factor(ANS_TEXT, levels = ANS_LEVELS, ordered = TRUE),
           enrollment = NA_real_,
           respondents = NA_real_,
           response_rate = NA_real_,
           instructor = NA_character_) |>
    select(course, term, QUES_TEXT, ANS_TEXT, ANS_COUNT, ANS_PCT,
           enrollment, respondents, response_rate, instructor)
}

# Dispatch on file extension. XLSX support arrives in Phase B.
read_soot_file <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext == "csv") {
    read_soot_csv(path, name)
  } else if (ext %in% c("xlsx", "xls")) {
    stop("XLSX import not yet supported. Please upload the old-format CSV files for now.")
  } else {
    warning(sprintf("Skipping file with unrecognized extension: %s", name))
    NULL
  }
}
