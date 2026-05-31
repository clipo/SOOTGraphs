# Pure data-transformation helpers for the SOOT aggregator.
# Sourced by app.R; unit-tested in tests/testthat/test-helpers.R.

library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(ggplot2)

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

# Per-course-per-group top-two-box from raw counts. Internal helper.
.top_two_box_by_course <- function(data, group_vars) {
  data |>
    group_by(across(all_of(c("course", group_vars)))) |>
    summarise(
      non_na = sum(ANS_COUNT[ANS_TEXT != "Not Applicable"]),
      top    = sum(ANS_COUNT[ANS_TEXT %in% TOP_BOX_LEVELS]),
      .groups = "drop"
    ) |>
    mutate(course_ttb = ifelse(non_na > 0, 100 * top / non_na, NA_real_))
}

# Top-two-box percentage per group, by weighting method.
#   weighting = "student": pool all counts, then compute the percentage.
#   weighting = "course":  per-course percentage, averaged equally across courses.
# Returns one row per group with: top_two_box, plus n (student) or n_courses (course).
compute_top_two_box <- function(data, group_vars, weighting = c("student", "course")) {
  weighting <- match.arg(weighting)
  if (weighting == "student") {
    data |>
      group_by(across(all_of(group_vars))) |>
      summarise(
        n = sum(ANS_COUNT[ANS_TEXT != "Not Applicable"]),
        top = sum(ANS_COUNT[ANS_TEXT %in% TOP_BOX_LEVELS]),
        .groups = "drop"
      ) |>
      mutate(top_two_box = ifelse(n > 0, 100 * top / n, NA_real_)) |>
      select(all_of(group_vars), top_two_box, n)
  } else {
    .top_two_box_by_course(data, group_vars) |>
      group_by(across(all_of(group_vars))) |>
      summarise(
        top_two_box = mean(course_ttb, na.rm = TRUE),
        n_courses = sum(!is.na(course_ttb)),
        .groups = "drop"
      ) |>
      mutate(top_two_box = ifelse(is.nan(top_two_box), NA_real_, top_two_box))
  }
}

# Rating distribution per group, computed from counts EXCLUDING "Not Applicable",
# so the colored bars reflect only students who rated. The N/A share is reported
# separately. Returns a list(dist = <per-answer percentages>, na_share = <per-group>).
compute_rating_distribution <- function(data, group_vars) {
  totals <- data |>
    group_by(across(all_of(group_vars))) |>
    summarise(total_all = sum(ANS_COUNT),
              na_count = sum(ANS_COUNT[ANS_TEXT == "Not Applicable"]),
              non_na = sum(ANS_COUNT[ANS_TEXT != "Not Applicable"]),
              .groups = "drop") |>
    mutate(na_pct = ifelse(total_all > 0, 100 * na_count / total_all, NA_real_))

  dist <- data |>
    filter(ANS_TEXT != "Not Applicable") |>
    group_by(across(all_of(c(group_vars, "ANS_TEXT")))) |>
    summarise(count = sum(ANS_COUNT), .groups = "drop") |>
    left_join(totals |> select(all_of(group_vars), non_na), by = group_vars) |>
    mutate(pct = ifelse(non_na > 0, 100 * count / non_na, NA_real_),
           ANS_TEXT = droplevels(factor(ANS_TEXT, levels = ANS_LEVELS, ordered = TRUE)))

  list(dist = dist,
       na_share = totals |> select(all_of(group_vars), na_count, na_pct))
}

# Horizontal bar of top-two-box % per question, in canonical order.
plot_summary_by_question <- function(ttb, weighting_label, n_label_col) {
  ttb <- ttb |>
    mutate(QUES_TEXT = factor(QUES_TEXT, levels = rev(INSTRUCTOR_QUESTIONS)),
           lbl = ifelse(is.na(top_two_box), "n/a",
                        sprintf("%.0f%% (%s=%d)", top_two_box,
                                ifelse("n" %in% names(ttb), "n", "courses"),
                                .data[[n_label_col]])))
  ggplot(ttb, aes(x = QUES_TEXT, y = top_two_box)) +
    geom_col(fill = viridisLite::viridis(1, begin = 0.4)) +
    geom_text(aes(label = lbl), hjust = -0.05, size = 3) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 109), breaks = seq(0, 100, 25)) +
    labs(x = "", y = "Top-two-box % (High or Very High)",
         title = sprintf("Summary Score by Question (%s)", weighting_label)) +
    theme(axis.text.y = element_text(size = 9))
}

# Small-multiple line of top-two-box % across terms, one panel per question.
plot_trends <- function(ttb_by_term, weighting_label) {
  ttb_by_term <- ttb_by_term |>
    mutate(term = order_terms(term),
           QUES_TEXT = factor(QUES_TEXT, levels = INSTRUCTOR_QUESTIONS))
  ggplot(ttb_by_term, aes(x = term, y = top_two_box, group = 1)) +
    geom_line(color = viridisLite::viridis(1, begin = 0.4)) +
    geom_point(color = viridisLite::viridis(1, begin = 0.4)) +
    facet_wrap(~QUES_TEXT, labeller = label_wrap_gen(width = 25)) +
    scale_y_continuous(limits = c(0, 100)) +
    labs(x = "", y = "Top-two-box %",
         title = sprintf("Summary Score Trends by Term (%s)", weighting_label)) +
    theme(axis.text.x = element_text(angle = -45, hjust = 0, size = 7),
          strip.text = element_text(size = 7))
}

# Bar of rating respondents per course (max over questions), grouped by term.
plot_response_counts <- function(data) {
  counts <- data |>
    group_by(course, term, QUES_TEXT) |>
    summarise(n = sum(ANS_COUNT[ANS_TEXT != "Not Applicable"]), .groups = "drop") |>
    group_by(course, term) |>
    summarise(respondents = max(n), .groups = "drop") |>
    mutate(term = order_terms(term))
  ggplot(counts, aes(x = course, y = respondents, fill = term)) +
    geom_col(position = position_dodge(preserve = "single")) +
    scale_fill_viridis_d() +
    labs(x = "", y = "Rating respondents", fill = "Term",
         title = "Response Counts by Course") +
    theme(axis.text.x = element_text(angle = -45, hjust = 0, size = 8))
}
