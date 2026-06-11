# vignettes/drug_mapping_diagram.R
# Diagram: all_drugs (windowed raw names) -> all_drugs_filtered (21 surviving
# categories) + independent tlfb list colored by match. All lists derived live
# from public.ctn0094data; nothing hard-coded. See
# docs/superpowers/plans/2026-06-11-drug-mapping-diagram.md
suppressPackageStartupMessages({
  library(public.ctn0094data)
  library(tidyverse)
  library(scales)
  library(grid)
})

# ---- window + grouping machinery (verbatim logic from building_analysis.R) ----
drug_map <- c(
  "Acetaminophen"="Analgesic","Codeine"="Opioid","Hydrocodone"="Opioid",
  "Hydromorphone"="Opioid","Merperidine"="Opioid","Morphine"="Opioid",
  "Nalbuphine"="Opioid","Opium"="Opioid","Oxycodone"="Opioid",
  "Oxymorphone"="Opioid","Propoxyphene"="Opioid","Tramadol"="Opioid",
  "Sedative-Hypnotic"="Sedatives","Barbiturate"="Sedatives",
  "Trazodone"="Antidepressant","Tryclic-Antidepressant"="Antidepressant",
  "Gabapentin"="Analgesic","Thc"="Cannabinoids","K2"="Cannabinoids",
  "Musclerelax"="Muscle Relaxant","Mdma"="MDMA/Hallucinogen",
  "Hallucinogen"="MDMA/Hallucinogen","Heavy Drinking"="Alcohol Heavy Amnt",
  "Light Drinking"="Alcohol Light Amnt","Alcohol"="Alcohol Missing Amnt"
)

day_zero_lookup <- screening_date |>
  select(who, day_zero) |>
  mutate(day_zero = replace_na(day_zero, 0L))

in_window <- function(df) {
  df |>
    left_join(day_zero_lookup, by = "who") |>
    mutate(day_zero = replace_na(day_zero, 0L)) |>  # defensive: participants absent from screening_date get NA from left_join
    filter(when >= day_zero - 28, when < day_zero)
}

# ---- all_drugs: windowed raw names + grouping ----
all_drugs_windowed <- all_drugs |>
  mutate(what_grouped = case_when(
    what %in% names(drug_map) ~ drug_map[as.character(what)],
    .default = as.character(what)
  )) |>
  in_window()

surviving <- all_drugs_windowed |>
  count(what_grouped) |>
  filter(n >= 10) |>
  arrange(desc(n))                      # filtered-box order: by event count

src_map <- all_drugs_windowed |>        # source `what` -> surviving category
  filter(what_grouped %in% surviving$what_grouped) |>
  distinct(what_grouped, what) |>
  mutate(what = as.character(what))

axed_raw <- all_drugs_windowed |>       # windowed names whose category was axed
  filter(!what_grouped %in% surviving$what_grouped) |>
  distinct(what) |>
  mutate(what = as.character(what)) |>
  pull(what) |>
  sort()

# ---- tlfb: windowed names with count >= 1 ----
tlfb_windowed <- tlfb |> in_window()
tlfb_drugs <- tlfb_windowed |>
  count(what, name = "events") |>
  filter(events >= 1) |>
  arrange(desc(events)) |>
  mutate(what = as.character(what))

# ---- INTEGRITY CHECKS: derived lists must match the tibbles ----
stopifnot(
  "all_drugs windowed names != 42"      = n_distinct(all_drugs_windowed$what) == 42L,
  "surviving categories != 21"          = nrow(surviving) == 21L,
  "grouped+axed source rows != 42"      = nrow(src_map) + length(axed_raw) == 42L,
  "tlfb windowed drugs != 25"           = nrow(tlfb_drugs) == 25L,
  "expected multi-source groups"        = setequal(
    src_map |> count(what_grouped) |> filter(n >= 2) |> pull(what_grouped),
    c("Opioid","Cannabinoids","MDMA/Hallucinogen","Analgesic"))
)
cat("Task 1 integrity checks PASSED\n")

# ---- dependency-free rounded rectangle (closed polygon path) ----
rounded_rect <- function(xmin, ymin, xmax, ymax, r, n = 16, group = "g") {
  r <- min(r, (xmax - xmin) / 2, (ymax - ymin) / 2)
  a <- seq(0, pi / 2, length.out = n)
  tr <- cbind(xmax - r + r * sin(a), ymax - r + r * cos(a))  # top-right
  br <- cbind(xmax - r + r * cos(a), ymin + r - r * sin(a))  # bottom-right
  bl <- cbind(xmin + r - r * sin(a), ymin + r - r * cos(a))  # bottom-left
  tl <- cbind(xmin + r - r * cos(a), ymax - r + r * sin(a))  # top-left
  m  <- rbind(tr, br, bl, tl)
  tibble(group = group, x = c(m[, 1], m[1, 1]), y = c(m[, 2], m[1, 2]))
}

# ---- layout constants ----
dy        <- 1                          # vertical spacing per drug row
box_w     <- 9                          # inner text width budget (data units)
col_x     <- c(all_drugs = 0, filtered = 16, tlfb = 32)  # left edge of each box
pad_x     <- 0.6                        # text inset from box left edge
n_rows    <- max(42, 25)                # tallest column governs box height
y_top     <- n_rows * dy                # first row baseline
box_top   <- y_top + 1.5
box_bot   <- y_top - (n_rows - 1) * dy - 1.5

# row y for the i-th item (1-based, top to bottom)
row_y <- function(i) y_top - (i - 1) * dy

stopifnot(
  "box height must exceed tallest list" =
    (box_top - box_bot) >= n_rows * dy
)
cat("Task 2 layout constants OK\n")

# ---- order surviving categories top->bottom, expand to source rows ----
cat_order <- surviving$what_grouped                     # 21, by descending n
src_blocks <- src_map |>
  left_join(
    tibble(what_grouped = cat_order, cat_rank = seq_along(cat_order)),
    by = "what_grouped"
  )

# all_drugs rows: grouped sources first (in category order), then axed names
ad_rows <- bind_rows(
  src_blocks |> mutate(kind = "grouped"),
  tibble(what = axed_raw, what_grouped = NA_character_, cat_rank = NA_integer_,
         kind = "axed")
) |>
  mutate(row = row_number(),
         y   = row_y(row),
         x   = col_x["all_drugs"] + pad_x)

# distinct color per surviving category (21), readable on white
pal <- setNames(hue_pal(l = 45, c = 100)(length(cat_order)), cat_order)

# highlight box bounds: enclose each category's contiguous source rows
hl_boxes <- ad_rows |>
  filter(kind == "grouped") |>
  group_by(what_grouped, cat_rank) |>
  summarize(ymin = min(y) - 0.45 * dy, ymax = max(y) + 0.45 * dy,
            .groups = "drop") |>
  mutate(xmin = col_x["all_drugs"] - 0.3,
         xmax = col_x["all_drugs"] + box_w + 0.3,
         color = pal[what_grouped]) |>
  arrange(cat_rank)

# filtered-box labels: centered on their source block, colored to match
filt_rows <- ad_rows |>
  filter(kind == "grouped") |>
  group_by(what_grouped, cat_rank) |>
  summarize(y = mean(y), .groups = "drop") |>
  mutate(x = col_x["filtered"] + pad_x, color = pal[what_grouped]) |>
  arrange(cat_rank)

cat("Task 3: ", nrow(ad_rows), " all_drugs rows, ",
    nrow(filt_rows), " filtered labels, ",
    nrow(hl_boxes), " highlight boxes\n", sep = "")
stopifnot(nrow(ad_rows) == 42L, nrow(filt_rows) == 21L, nrow(hl_boxes) == 21L)

# ---- tlfb green/red: clean 1-to-1 match to a surviving category = green ----
# Overridable. `match_cat` = the filtered category a tlfb drug maps to (NA = none).
# Borderline reds (commented): THC/K2 collapse into Cannabinoids; Alcohol splits
# into 3 alcohol categories; Hallucinogen is subsumed by MDMA/Hallucinogen.
tlfb_match <- tribble(
  ~what,               ~match_cat,
  "Heroin",            "Heroin",
  "Opioid",            "Opioid",
  "Cocaine",           "Cocaine",
  "Methadone",         "Methadone",
  "Benzodiazepine",    "Benzodiazepine",
  "Amphetamine",       "Amphetamine",
  "Buprenorphine",     "Buprenorphine",
  "Sedatives",         "Sedatives",
  "Muscle Relaxant",   "Muscle Relaxant",
  "Mdma/Hallucinogen", "MDMA/Hallucinogen",
  "Analgesic",         "Analgesic",
  "Pcp",               "Pcp",
  "Antiemetic",        "Antiemetic"
)

tlfb_pos <- tlfb_drugs |>
  left_join(tlfb_match, by = "what") |>
  mutate(is_match = !is.na(match_cat),
         color    = if_else(is_match, "#1a9c3b", "#d62728"))  # green / red

# every listed match must be a real surviving category
stopifnot(all(na.omit(tlfb_pos$match_cat) %in% cat_order))

# green rows align to their filtered category's y; reds fill leftover rows
green_y <- filt_rows |> select(what_grouped, fy = y)
tlfb_green <- tlfb_pos |>
  filter(is_match) |>
  left_join(green_y, by = c("match_cat" = "what_grouped")) |>
  mutate(y = fy)

used_y <- sort(unique(tlfb_green$y), decreasing = TRUE)
all_y  <- row_y(seq_len(n_rows))
free_y <- sort(setdiff(round(all_y, 6), round(used_y, 6)), decreasing = TRUE)
tlfb_red <- tlfb_pos |>
  filter(!is_match) |>
  mutate(y = free_y[seq_len(n())])

tlfb_rows <- bind_rows(tlfb_green, tlfb_red) |>
  mutate(x = col_x["tlfb"] + pad_x)

stopifnot(nrow(tlfb_rows) == 25L, sum(tlfb_rows$is_match) == 13L)
cat("Task 4: ", sum(tlfb_rows$is_match), " green / ",
    sum(!tlfb_rows$is_match), " red tlfb rows\n", sep = "")
