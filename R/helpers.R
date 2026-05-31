# Pure data-transformation helpers for the SOOT aggregator.
# Sourced by app.R; unit-tested in tests/testthat/test-helpers.R.

library(dplyr)
library(tidyr)
library(stringr)

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
