# SOOT App Binghamton Redesign + Flexible Upload — Design Spec

Date: 2026-05-31
Status: Approved for planning
Project: SOOTGraphs (Harpur College SOOT Aggregator Shiny app)

## Purpose

The app works but looks unstyled and scrolls through eleven result sections in one
column. This work rebrands it with Binghamton University identity, reorganizes the
results into a sidebar + tabbed layout, and makes uploads flexible: users may
select individual CSV/XLSX files OR a single ZIP containing them. The instructions
are rewritten to explain the two file formats and the two upload paths.

No analysis or chart logic changes. This is UI, layout, branding, and upload
ingestion only.

## Brand reference

From Binghamton University brand guidelines:
- Primary deep green `#005A43` (dominant color).
- Accents: yellow-green `#CEDC00`, light green `#6CC24A`.
- Gray `#5A5C5B`, black `#000000`, white `#FFFFFF`.
- Official logotype `BU-logo.png`: must not be recreated, altered, or distorted;
  requires clear space; units may not create independent marks. Source image:
  `https://www.binghamton.edu/communications-and-marketing/img/logos/BU-logo.png`.

## Theming

Replace the bare `fluidPage` with a **bslib** Bootstrap 5 theme (`bslib::bs_theme`).
bslib ships as a Shiny dependency, so nothing new to deploy. Theme variables:
- `primary = "#005A43"`, `success = "#6CC24A"`.
- Base font: a clean sans-serif (system sans stack or a bundled Google sans via
  `bslib::font_google` if it loads cleanly; system sans is the safe default).
- Charts are unchanged; their viridis palette coexists with the green theme.

## Header

A page header (`bslib::card` or a styled `div`) with a **white background band**
containing:
- The official BU logo on the left, bundled at `www/BU-logo.png`, displayed
  unaltered at a readable height (about 48-60 px) with clear space around it.
- The text title "Harpur College SOOT Aggregator" in brand green.
- A thin green (`#005A43`) accent bar beneath the band.

Using the logo on white avoids any recoloring of the mark.

## Layout: sidebar + tabbed results

Use `bslib::page_sidebar` (or `page_fluid` + `layout_sidebar`).

**Sidebar** (stays in view): rewritten instructions, the `fileInput`, and the
`actionButton("process", ...)`.

**Main panel**: `bslib::navset_card_tab` with four tabs; each chart wrapped in a
card with its existing download buttons. Chart-to-tab assignment:

- **Overview**: Summary score by question (student-weighted) + (course-weighted) +
  the two-weightings explanatory paragraph; Overall Results Aggregated by Question
  (stacked).
- **By Term**: Results Aggregated by Term (stacked); Summary Trends by Term
  (student-weighted) + (course-weighted).
- **By Course**: Results Aggregated by Course (stacked); Question-by-Course
  heatmap.
- **Reliability**: Response Counts by Course; Response Rate by Course; Course
  Inventory downloads.

All existing `plotOutput`, `downloadButton`, and server output IDs are preserved;
only their container/placement in the UI changes. The server logic is unchanged
except for the upload-expansion step below.

## Flexible upload + ingestion

`fileInput("csvs", ..., multiple = TRUE, accept = c(".csv", ".xlsx", ".zip"))`.
The input id `csvs` is kept so existing server references work.

New tested helper in `R/helpers.R`:

```
expand_uploads(paths, names) -> tibble(path, name)
```

For each uploaded file:
- If its name ends in `.zip` (case-insensitive): extract to a fresh temp dir and
  add every contained `.csv`/`.xlsx` file (recursively; nested folders handled).
  Use the basename of each extracted file as its `name` so `parse_course_term`
  still works.
- Otherwise: pass through as `(path, name)`.

The server replaces the current per-upload loop with: build the work list via
`expand_uploads(input$csvs$datapath, input$csvs$name)`, then read each entry
through the existing `read_soot_file`. Empty/unsupported/zero-instructor entries
are skipped with warnings exactly as today. A zip containing no usable files
yields the existing "No usable data found" notification.

## Instructions copy (sidebar)

> Upload your SOOT results to generate aggregated graphs. The app accepts two
> formats: the older CSV export (one file per course, already tallied) and the
> newer XLSX export (one file per course, individual responses). You can mix both.
>
> Two ways to upload: select all your individual .csv or .xlsx files at once, or
> put them together in a single .zip file and upload that one zip. Do not rename
> the files. Then click Process uploaded data.
>
> Files are processed in your browser session and are not stored on the server.

## Edge cases

- A `.zip` plus loose files in one upload batch: both handled (zip expanded, loose
  files passed through).
- Nested directories inside the zip: recursive listing picks them up.
- A zip with no CSV/XLSX, or only empty files: skipped with warnings, then the
  "No usable data found" notification.
- Non-CSV/XLSX/zip files: skipped with a warning (existing dispatch behavior).
- macOS zip cruft (`__MACOSX/`, `.DS_Store`): ignored because only `.csv`/`.xlsx`
  extensions are collected.

## Testing

- `expand_uploads` unit tests: a zip built on the fly from existing fixtures
  (returns the contained files), a mixed loose-file + zip batch, a nested-folder
  zip, and a zip with macOS cruft (cruft ignored).
- The full ingestion still passes the existing mixed CSV+XLSX and sample-set
  checks.
- Visual verification: launch the app locally and use the Playwright browser tool
  to screenshot the branded header, sidebar, and each tab to confirm the redesign
  renders. (Charts themselves are already covered by Phase A/B tests.)

## Deployment note

The bundled `www/BU-logo.png` must be included in the rsconnect deploy
(`appFiles` must add `www/BU-logo.png` alongside `app.R` and `R/helpers.R`).
bslib is already an installed dependency; no new package to declare.

## Out of scope

- Any change to chart computations, weightings, or the canonical schema.
- Custom or reversed logo variants (logo used unaltered on white).
- Licensed brand fonts (a clean sans is sufficient).
