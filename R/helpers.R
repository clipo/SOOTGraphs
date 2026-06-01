# Pure data-transformation helpers for the SOOT aggregator.
# Sourced by app.R; unit-tested in tests/testthat/test-helpers.R.

library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(ggplot2)
library(readxl)
library(grid)

ANS_LEVELS <- c("Not Applicable", "Very Low or Never", "Low",
                "Average", "High", "Very High or Always")
TOP_BOX_LEVELS <- c("High", "Very High or Always")

CODE_TO_ANS <- c(`0` = "Not Applicable", `1` = "Very Low or Never",
                 `2` = "Low", `3` = "Average", `4` = "High",
                 `5` = "Very High or Always")

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

# Take the question text after the final " - " separator (the instructor-item
# pattern); return the trimmed string unchanged if there is no such separator.
strip_question_prefix <- function(x) {
  trimws(sub("^.* - ", "", x))
}

# Read one per-respondent XLSX export into the canonical long format.
# Only the 9 instructor questions are normalized (other questions use different
# answer scales). Blank/no-response cells are dropped; code 0 is "Not Applicable".
read_soot_xlsx <- function(path, name) {
  raw <- tryCatch(readxl::read_excel(path, sheet = "RawData"),
                  error = function(e) NULL)
  mapper <- tryCatch(readxl::read_excel(path, sheet = "QuestionMapper"),
                     error = function(e) NULL)
  if (is.null(raw) || is.null(mapper) || nrow(raw) == 0) {
    warning(sprintf("Skipping empty or unreadable XLSX: %s", name))
    return(NULL)
  }
  qmap <- tibble::tibble(col = mapper[[1]],
                         QUES_TEXT = strip_question_prefix(mapper[[2]])) |>
    filter(QUES_TEXT %in% INSTRUCTOR_QUESTIONS, col %in% names(raw))
  if (nrow(qmap) == 0) {
    warning(sprintf("No instructor questions found in XLSX: %s", name))
    return(NULL)
  }
  ct <- parse_course_term(name)
  meta_first <- function(col) if (col %in% names(raw)) raw[[col]][1] else NA

  long <- raw |>
    mutate(.rid = row_number()) |>
    select(.rid, all_of(qmap$col)) |>
    pivot_longer(-.rid, names_to = "col", values_to = "code") |>
    mutate(code = trimws(as.character(code))) |>
    filter(!is.na(code), code %in% names(CODE_TO_ANS)) |>
    left_join(qmap, by = "col") |>
    mutate(ANS_TEXT = unname(CODE_TO_ANS[code]))

  agg <- long |>
    group_by(QUES_TEXT, ANS_TEXT) |>
    summarise(ANS_COUNT = n(), .groups = "drop") |>
    group_by(QUES_TEXT) |>
    mutate(ANS_PCT = 100 * ANS_COUNT / sum(ANS_COUNT)) |>
    ungroup()

  agg |>
    mutate(course = ct$course,
           term = ct$term,
           ANS_TEXT = factor(ANS_TEXT, levels = ANS_LEVELS, ordered = TRUE),
           enrollment = as.numeric(meta_first("Enrollments")),
           respondents = as.numeric(meta_first("Respondents")),
           response_rate = as.numeric(meta_first("ResponseRate")),
           instructor = as.character(meta_first("InstructorName"))) |>
    select(course, term, QUES_TEXT, ANS_TEXT, ANS_COUNT, ANS_PCT,
           enrollment, respondents, response_rate, instructor)
}

# Dispatch on file extension: old aggregate CSV or new per-respondent XLSX.
read_soot_file <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext == "csv") {
    read_soot_csv(path, name)
  } else if (ext %in% c("xlsx", "xls")) {
    read_soot_xlsx(path, name)
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

# Heatmap of top-two-box % with questions (rows) by course (columns).
plot_question_course_heatmap <- function(data) {
  ttb <- compute_top_two_box(data, c("QUES_TEXT", "course"), "student") |>
    mutate(QUES_TEXT = factor(QUES_TEXT, levels = rev(INSTRUCTOR_QUESTIONS)))
  ggplot(ttb, aes(x = course, y = QUES_TEXT, fill = top_two_box)) +
    geom_tile(color = "white") +
    geom_text(aes(label = ifelse(is.na(top_two_box), "", sprintf("%.0f", top_two_box))),
              size = 3, color = "white") +
    scale_fill_viridis_c(limits = c(0, 100), name = "Top-two-box %") +
    labs(x = "", y = "", title = "Top-Two-Box % by Question and Course") +
    theme(axis.text.y = element_text(size = 8),
          axis.text.x = element_text(angle = -45, hjust = 0, size = 8))
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

# Mean response rate per course/term, dropping rows without a rate (CSV-sourced).
response_rate_by_course <- function(data) {
  data |>
    filter(!is.na(response_rate)) |>
    distinct(course, term, response_rate) |>
    group_by(course, term) |>
    summarise(response_rate = mean(response_rate), .groups = "drop")
}

# Bar of response rate per course (grouped by term). Renders a placeholder when
# no rate is available (e.g. only old-format CSV files were uploaded).
plot_response_rate <- function(data) {
  rr <- response_rate_by_course(data)
  if (nrow(rr) == 0) {
    return(ggplot() +
             annotate("text", x = 1, y = 1,
                      label = "Response rate not available\n(requires XLSX-format uploads)") +
             theme_void())
  }
  rr <- rr |> mutate(term = order_terms(term))
  ggplot(rr, aes(x = course, y = response_rate, fill = term)) +
    geom_col(position = position_dodge(preserve = "single")) +
    scale_fill_viridis_d() +
    scale_y_continuous(limits = c(0, 100)) +
    labs(x = "", y = "Response rate (%)", fill = "Term",
         title = "Response Rate by Course") +
    theme(axis.text.x = element_text(angle = -45, hjust = 0, size = 8))
}

# Expand an upload batch into a work list of individual SOOT files.
# Any .zip is extracted (recursively) and its .csv/.xlsx members added; other
# files pass through. macOS archive cruft (__MACOSX/, ._ files) is ignored.
# Returns a tibble(path, name) where name is the basename used downstream.
expand_uploads <- function(paths, names) {
  out_path <- character(0)
  out_name <- character(0)
  for (i in seq_along(paths)) {
    if (grepl("\\.zip$", names[i], ignore.case = TRUE)) {
      exdir <- tempfile("unzip_"); dir.create(exdir)
      tryCatch(utils::unzip(paths[i], exdir = exdir), error = function(e) NULL)
      inner <- list.files(exdir, pattern = "\\.(csv|xlsx)$", ignore.case = TRUE,
                          recursive = TRUE, full.names = TRUE)
      inner <- inner[!grepl("(^|/)(__MACOSX/|\\._)", inner)]
      for (f in inner) { out_path <- c(out_path, f); out_name <- c(out_name, basename(f)) }
    } else {
      out_path <- c(out_path, paths[i]); out_name <- c(out_name, names[i])
    }
  }
  tibble::tibble(path = out_path, name = out_name)
}

# Build a print-ready landscape PDF: a branded cover page then one page per chart.
# plots: named list of ggplot objects (names become page captions).
# meta:  list(courses, terms, instructor, n_courses, date); NULL/empty -> placeholder.
# logo_path: optional PNG embedded on the cover when present.
build_report_pdf <- function(file, plots, meta, logo_path = "www/BU-logo.png") {
  grDevices::pdf(file, paper = "USr", width = 11, height = 8.5, onefile = TRUE)
  on.exit(grDevices::dev.off())

  if (length(plots) == 0 || is.null(meta)) {
    grid::grid.newpage()
    grid::grid.text("No data processed yet.\nUpload SOOT files and click 'Process uploaded data' first.",
                    gp = grid::gpar(fontsize = 18))
    return(invisible(file))
  }

  # Cover page
  grid::grid.newpage()
  if (!is.null(logo_path) && file.exists(logo_path)) {
    img <- png::readPNG(logo_path)
    grid::grid.raster(img, x = 0.5, y = 0.82, height = grid::unit(0.9, "inches"))
  }
  grid::grid.text("Harpur College SOOT Aggregation Report", y = 0.66,
                  gp = grid::gpar(fontsize = 26, fontface = "bold", col = "#005A43"))
  instr <- meta$instructor
  instr <- if (is.null(instr) || all(is.na(instr))) NULL else paste(unique(stats::na.omit(instr)), collapse = ", ")
  lines <- c(
    if (!is.null(instr)) paste("Instructor:", instr),
    paste("Courses:", paste(sort(unique(meta$courses)), collapse = ", ")),
    paste("Terms:", paste(meta$terms, collapse = ", ")),
    paste("Generated:", format(meta$date, "%B %d, %Y"))
  )
  grid::grid.text(paste(lines, collapse = "\n"), y = 0.42,
                  gp = grid::gpar(fontsize = 13, col = "#5A5C5B"))
  grid::grid.text("Generated by the Harpur College SOOT Aggregator", y = 0.06,
                  gp = grid::gpar(fontsize = 9, col = "#9C9D9D"))

  # One page per chart
  for (nm in names(plots)) {
    print(plots[[nm]] +
            ggplot2::labs(caption = "Generated by the Harpur College SOOT Aggregator") +
            ggplot2::theme(plot.caption = ggplot2::element_text(color = "#9C9D9D", size = 8)))
  }
  invisible(file)
}

# Summary statistics for the post-upload confirmation panel.
upload_summary <- function(full, n_uploaded, n_skipped) {
  resp <- full |>
    group_by(course, term, QUES_TEXT) |>
    summarise(n = sum(ANS_COUNT), .groups = "drop") |>
    group_by(course, term) |>
    summarise(respondents = max(n), .groups = "drop")
  list(
    files_processed = n_uploaded - n_skipped,
    files_skipped   = n_skipped,
    n_courses       = length(unique(full$course)),
    n_terms         = length(unique(full$term)),
    respondents     = sum(resp$respondents)
  )
}

# Distinct non-missing instructor names present in the data (XLSX-sourced).
distinct_instructors <- function(full) {
  ins <- unique(full$instructor)
  ins[!is.na(ins)]
}

# Curated course-context questions: canonical CSV text, chart label order, group,
# and the XLSX numeric-code-to-label map (validated against the CSV label
# vocabulary; the Expected Grade letter map is an explicit inference).
CONTEXT_QUESTIONS <- list(
  list(text = "My interest in subject before course", group = "interest",
       labels = c("Low","Medium","High"),
       codes = c(`1`="Low", `2`="Medium", `3`="High")),
  list(text = "My interest in subject after course", group = "interest",
       labels = c("Low","Medium","High"),
       codes = c(`1`="Low", `2`="Medium", `3`="High")),
  list(text = "Difficulty (relative to other courses)", group = "demands",
       labels = c("Low","Medium","High"),
       codes = c(`1`="Low", `2`="Medium", `3`="High")),
  list(text = "Workload (relative to other courses)", group = "demands",
       labels = c("Low","Medium","High"),
       codes = c(`1`="Low", `2`="Medium", `3`="High")),
  list(text = "Usefulness of texts", group = "usefulness",
       labels = c("Low","Medium","High"),
       codes = c(`0`="Not Applicable", `1`="Low", `2`="Medium", `3`="High")),
  list(text = "Usefulness of homework assignments", group = "usefulness",
       labels = c("Low","Medium","High"),
       codes = c(`0`="Not Applicable", `1`="Low", `2`="Medium", `3`="High")),
  list(text = "Usefulness of lab assignments", group = "usefulness",
       labels = c("Low","Medium","High"),
       codes = c(`0`="Not Applicable", `1`="Low", `2`="Medium", `3`="High")),
  list(text = "Usefulness of examinations", group = "usefulness",
       labels = c("Low","Medium","High"),
       codes = c(`0`="Not Applicable", `1`="Low", `2`="Medium", `3`="High")),
  list(text = "Usefulness of class discussions", group = "usefulness",
       labels = c("Low","Medium","High"),
       codes = c(`0`="Not Applicable", `1`="Low", `2`="Medium", `3`="High")),
  list(text = "Expected Grade", group = "grade",
       labels = c("A","B","C","D","F","P","NP","Don't Know"),
       codes = c(`1`="A", `2`="B", `3`="C", `4`="D", `5`="F", `6`="P", `7`="NP", `8`="Don't Know"))
)

CONTEXT_TEXTS <- vapply(CONTEXT_QUESTIONS, `[[`, character(1), "text")

# A labeled empty-state placeholder for a context chart with no data.
context_empty_plot <- function(title) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 1, y = 1,
                      label = paste0(title, "\n(no data in the uploaded files)")) +
    ggplot2::theme_void()
}

# Read the curated context questions from a per-respondent XLSX, decoding numeric
# codes to labels via CONTEXT_QUESTIONS. Blank/unmapped codes are dropped.
read_context_xlsx <- function(path, name) {
  raw <- tryCatch(readxl::read_excel(path, sheet = "RawData"), error = function(e) NULL)
  mapper <- tryCatch(readxl::read_excel(path, sheet = "QuestionMapper"), error = function(e) NULL)
  if (is.null(raw) || is.null(mapper) || nrow(raw) == 0) return(NULL)
  ct <- parse_course_term(name)
  qmap <- tibble::tibble(col = mapper[[1]],
                         QUES_TEXT = sub("[.]$", "", strip_question_prefix(mapper[[2]]))) |>
    filter(QUES_TEXT %in% CONTEXT_TEXTS, col %in% names(raw))
  if (nrow(qmap) == 0) return(NULL)
  out <- list()
  for (k in seq_len(nrow(qmap))) {
    qtext <- qmap$QUES_TEXT[k]
    codes <- CONTEXT_QUESTIONS[[which(CONTEXT_TEXTS == qtext)[1]]]$codes
    vals <- trimws(as.character(raw[[qmap$col[k]]]))
    vals <- vals[!is.na(vals) & vals %in% names(codes)]
    if (length(vals) == 0) next
    tab <- as.data.frame(table(ANS_TEXT = unname(codes[vals])), stringsAsFactors = FALSE)
    out[[length(out) + 1]] <- tibble::tibble(
      course = ct$course, term = ct$term, QUES_TEXT = qtext,
      ANS_TEXT = tab$ANS_TEXT, count = as.integer(tab$Freq))
  }
  if (length(out) == 0) return(NULL)
  dplyr::bind_rows(out)
}

# Dispatch context reading on file extension.
read_context_file <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext == "csv") read_context_csv(path, name)
  else if (ext %in% c("xlsx", "xls")) read_context_xlsx(path, name)
  else NULL
}

# Read the curated context questions from a CSV (labels already present).
read_context_csv <- function(path, name) {
  raw <- tryCatch(read.csv(path, header = TRUE, stringsAsFactors = FALSE),
                  error = function(e) NULL)
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  ct <- parse_course_term(name)
  raw |>
    filter(QUES_TEXT %in% CONTEXT_TEXTS, ANS_COUNT > 0) |>
    transmute(course = ct$course, term = ct$term, QUES_TEXT,
              ANS_TEXT = as.character(ANS_TEXT), count = as.integer(ANS_COUNT)) |>
    tibble::as_tibble()
}

# Internal: pool counts for a set of questions into a percentage composition,
# with ANS_TEXT ordered by `levels`. Returns NULL if no rows.
.context_compose <- function(context, questions, levels, exclude = character(0)) {
  if (is.null(context)) return(NULL)
  d <- context |>
    filter(QUES_TEXT %in% questions, !ANS_TEXT %in% exclude)
  if (nrow(d) == 0) return(NULL)
  d |>
    group_by(QUES_TEXT, ANS_TEXT) |>
    summarise(count = sum(count), .groups = "drop") |>
    mutate(ANS_TEXT = factor(ANS_TEXT, levels = levels))
}

plot_interest_shift <- function(context) {
  qs <- c("My interest in subject before course", "My interest in subject after course")
  d <- .context_compose(context, qs, c("Low","Medium","High"))
  if (is.null(d)) return(context_empty_plot("Interest before vs after"))
  d <- d |>
    mutate(phase = factor(ifelse(grepl("before", QUES_TEXT), "Before", "After"),
                          levels = c("Before","After"))) |>
    group_by(phase) |> mutate(pct = 100 * count / sum(count)) |> ungroup()
  hi <- d |> filter(ANS_TEXT == "High")
  delta <- round(sum(hi$pct[hi$phase == "After"]) - sum(hi$pct[hi$phase == "Before"]), 0)
  ggplot2::ggplot(d, ggplot2::aes(phase, pct, fill = ANS_TEXT)) +
    ggplot2::geom_col() + ggplot2::scale_fill_viridis_d() +
    ggplot2::labs(x = "", y = "Percentage", fill = "Interest",
                  title = "Interest in the Subject, Before vs After",
                  subtitle = sprintf("Change in share at High interest: %+d points", delta))
}

plot_course_demands <- function(context) {
  qs <- c("Difficulty (relative to other courses)", "Workload (relative to other courses)")
  d <- .context_compose(context, qs, c("Low","Medium","High"))
  if (is.null(d)) return(context_empty_plot("Course demands"))
  d <- d |>
    mutate(item = factor(ifelse(grepl("Difficulty", QUES_TEXT), "Difficulty", "Workload"),
                         levels = c("Difficulty","Workload"))) |>
    group_by(item) |> mutate(pct = 100 * count / sum(count)) |> ungroup()
  ggplot2::ggplot(d, ggplot2::aes(item, pct, fill = ANS_TEXT)) +
    ggplot2::geom_col() + ggplot2::scale_fill_viridis_d() +
    ggplot2::labs(x = "", y = "Percentage", fill = "Relative to other courses",
                  title = "Course Demands: Difficulty and Workload")
}

plot_expected_grade <- function(context) {
  d <- .context_compose(context, "Expected Grade",
                        c("A","B","C","D","F","P","NP","Don't Know"))
  if (is.null(d)) return(context_empty_plot("Expected grades"))
  d <- d |> mutate(pct = 100 * count / sum(count))
  ggplot2::ggplot(d, ggplot2::aes(ANS_TEXT, pct)) +
    ggplot2::geom_col(fill = viridisLite::viridis(1, begin = 0.4)) +
    ggplot2::labs(x = "", y = "Percentage", title = "Expected Grade Distribution")
}

plot_material_usefulness <- function(context) {
  qs <- c("Usefulness of texts", "Usefulness of homework assignments",
          "Usefulness of lab assignments", "Usefulness of examinations",
          "Usefulness of class discussions")
  d <- .context_compose(context, qs, c("Low","Medium","High"), exclude = "Not Applicable")
  if (is.null(d)) return(context_empty_plot("Usefulness of course materials"))
  d <- d |>
    mutate(material = sub("^Usefulness of ", "", QUES_TEXT)) |>
    group_by(material) |> mutate(pct = 100 * count / sum(count)) |> ungroup()
  ggplot2::ggplot(d, ggplot2::aes(material, pct, fill = ANS_TEXT)) +
    ggplot2::geom_col() + ggplot2::scale_fill_viridis_d() +
    ggplot2::labs(x = "", y = "Percentage", fill = "Usefulness",
                  title = "Usefulness of Course Materials (rated students only)") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -30, hjust = 0, size = 8))
}
