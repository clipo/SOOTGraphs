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
