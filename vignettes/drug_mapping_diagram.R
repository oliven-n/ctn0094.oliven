# vignettes/drug_mapping_diagram.R
# Diagram: all_drugs (windowed raw names) -> all_drugs_filtered (21 surviving
# categories) + independent tlfb list colored by match. All lists derived live
# from public.ctn0094data; nothing hard-coded. See
# docs/superpowers/plans/2026-06-11-drug-mapping-diagram.md
suppressPackageStartupMessages({
  library(public.ctn0094data)
  library(tidyverse)
  library(scales)
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
    c("Opioid","Cannabinoids","MDMA/Hallucinogen","Analgesic")),
  "a drug maps to multiple categories"  = all(count(src_map, what)$n == 1L)
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
n_rows    <- max(n_distinct(all_drugs_windowed$what), nrow(tlfb_drugs))  # tallest column governs box height
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
  src_blocks |> arrange(cat_rank) |> mutate(kind = "grouped"),
  tibble(what = axed_raw, what_grouped = NA_character_, cat_rank = NA_integer_,
         kind = "axed")
) |>
  mutate(row = row_number(),
         y   = row_y(row),
         x   = col_x["all_drugs"] + pad_x)

# distinct color per surviving category (21), readable on white
pal <- setNames(hue_pal(l = 45, c = 100)(length(cat_order)), cat_order)

# pharmacological opioids that are not in the "Opioid" all_drugs_filtered category
# (they map to their own categories: Heroin, Methadone, Buprenorphine, Suboxone, Fentanyl)
pharma_opioids_outside_group <- c("Heroin", "Methadone", "Buprenorphine", "Suboxone", "Fentanyl")

stopifnot(
  "pharma_opioids_outside_group: some entries absent from all_drugs_windowed" =
    all(pharma_opioids_outside_group %in% as.character(all_drugs_windowed$what)),
  "pharma_opioids_outside_group: some entries are in drug_map (map to a group, not themselves)" =
    !any(pharma_opioids_outside_group %in% names(drug_map))
)

ad_rows <- ad_rows |>
  mutate(
    text_color = if_else(kind == "grouped", pal[what_grouped], "black"),
    asterisk   = what %in% pharma_opioids_outside_group,
    label      = if_else(asterisk, paste0(what, " *"), what)
  )

# highlight boxes only for multi-source groups (2+ raw drugs mapping to same category)
multi_src <- src_map |> count(what_grouped) |> filter(n >= 2) |> pull(what_grouped)
hl_boxes <- ad_rows |>
  filter(kind == "grouped", what_grouped %in% multi_src) |>
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
  summarize(y = if (n() == 2L) max(y) else mean(y), .groups = "drop") |>
  mutate(x = col_x["filtered"] + pad_x, color = pal[what_grouped]) |>
  arrange(cat_rank)

cat("Task 3: ", nrow(ad_rows), " all_drugs rows, ",
    nrow(filt_rows), " filtered labels, ",
    nrow(hl_boxes), " highlight boxes\n", sep = "")
stopifnot(nrow(ad_rows) == 42L, nrow(filt_rows) == 21L,
          nrow(hl_boxes) == length(multi_src))

# ---- tlfb relationship-type coloring (5-category system) ----
# same:         semantically equivalent — same drugs, same effective name
# tlfb_grouped: tlfb consolidates multiple adf categories into one
# tlfb_finer:   tlfb is more specific than an adf category (subset)
# partial:      same category name, different underlying drug composition
# tlfb_only:    no adf counterpart (different instrument or n<10 axed)
REL_COLORS <- c(
  same         = "#1E3A5F",   # very dark navy
  tlfb_grouped = "#7A3B00",   # dark burnt sienna
  tlfb_finer   = "#1A5C36",   # dark forest green
  partial      = "#5C1F7A",   # dark grape
  tlfb_only    = "#000000"    # black
)

tlfb_rel <- tribble(
  ~what,               ~align_cat,              ~rel_type,
  # same: semantically equivalent
  "Methadone",         "Methadone",             "same",
  "Benzodiazepine",    "Benzodiazepine",        "same",
  "Pcp",               "Pcp",                   "same",
  "Antiemetic",        "Antiemetic",            "same",
  "Sedatives",         "Sedatives",             "same",
  "Mdma/Hallucinogen", "MDMA/Hallucinogen",     "tlfb_finer",
  "Muscle Relaxant",   "Muscle Relaxant",       "same",
  # Analgesic: 1-to-1 match in both adf and tlfb; Muscle Relaxant is its own tlfb category
  "Analgesic",         "Analgesic",             "same",

  # partial: same category name, different drug composition
  # tlfb "Heroin" absorbs Opium (in adf Opium→"Opioid"; in tlfb Opium→"Heroin")
  "Heroin",            "Heroin",                "partial",
  # tlfb "Opioid" includes drugs like Fentanyl; adf "Opioid" includes Opium — different sets
  "Opioid",            "Opioid",                "partial",

  # tlfb_grouped: tlfb consolidates multiple adf categories into one
  # Cocaine absorbs Crack (Crack → Cocaine in tlfb; Crack is own adf category)
  "Cocaine",           "Cocaine",               "tlfb_grouped",
  # Amphetamine absorbs Methamphetamine (Methamphetamine → Amphetamine in tlfb)
  "Amphetamine",       "Amphetamine",           "tlfb_grouped",
  # Buprenorphine absorbs Suboxone (Suboxone → Buprenorphine in tlfb)
  "Buprenorphine",     "Buprenorphine",         "tlfb_grouped",
  # Alcohol: tlfb consolidates 3 adf alcohol categories (Heavy Amnt / Light Amnt / Missing Amnt) into one
  "Alcohol",           "Alcohol Missing Amnt",  "tlfb_grouped",

  # tlfb_finer: tlfb tracks a specific subset of a broader adf category
  # adf "Cannabinoids" = {Thc, K2}; tlfb "THC" is more specific
  "THC",               "Cannabinoids",          "tlfb_finer",
  # K2 is also a subset of adf "Cannabinoids"; stacked below THC via row_number()
  "K2",                "Cannabinoids",          "tlfb_finer",
  # Hallucinogen: subset of adf MDMA/Hallucinogen (same as THC→Cannabinoids)
  "Hallucinogen",      "MDMA/Hallucinogen",     "tlfb_finer",
  # Remaining drugs: not in all_drugs drug_map or axed (n<10) in all_drugs_filtered
  "Cathinones",        NA,                      "tlfb_only",
  "Antibiotic",        NA,                      "tlfb_only",
  "Antidepressant",    NA,                      "tlfb_only",
  "Antipsychotic",     NA,                      "tlfb_only",
  "Methylphenidate",   NA,                      "tlfb_only",
  "Unknown",           NA,                      "tlfb_only",
  "Antihistamine",     NA,                      "tlfb_only",
  "Inhalant",          NA,                      "tlfb_only"
)

# every align_cat (non-NA) must be a real adf surviving category
stopifnot(all(na.omit(tlfb_rel$align_cat) %in% cat_order))
# every tlfb drug must be in the rel table
stopifnot(
  "tlfb_rel is missing entries" = all(tlfb_drugs$what %in% tlfb_rel$what)
)

# Text colors for tlfb labels:
#   same     → inherits matched adf pal color (vivid, same as the filtered-box label)
#   grouped/finer/partial → three distinct readable grays
#   tlfb_only → black
TLFB_TEXT_COLORS <- c(
  tlfb_grouped = "#404040",
  tlfb_finer   = "#707070",
  partial      = "#969696",
  tlfb_only    = "#000000"
)

tlfb_pos <- tlfb_drugs |>
  left_join(tlfb_rel, by = "what") |>
  mutate(rel_color = case_when(
    rel_type == "same" ~ pal[align_cat],
    .default = TLFB_TEXT_COLORS[rel_type]
  ))

# ---- tlfb positions ----
# Category-aligned entries (same, tlfb_grouped, tlfb_finer, partial — where align_cat set)
aligned_y <- filt_rows |> select(what_grouped, fy = y)
tlfb_aligned <- tlfb_pos |>
  filter(!is.na(align_cat)) |>
  left_join(aligned_y, by = c("align_cat" = "what_grouped")) |>
  arrange(align_cat, desc(events)) |>
  group_by(align_cat) |>
  mutate(y = fy - (row_number() - 1L) * dy) |>
  ungroup()

# tlfb_only drugs that correspond to axed all_drugs categories → align with ad_rows y
# tlfb "Antidepressant" spans two source drugs; use their mean y
tlfb_only_raw_map <- c(       # tlfb name -> raw all_drugs name
  Antibiotic       = "Antibiotic",
  Antihistamine    = "Antihistamine",
  Antipsychotic    = "Antipsychotic",
  Cathinones       = "Cathinones",
  Inhalant         = "Inhalant",
  Methylphenidate  = "Methylphenidate",
  Unknown          = "Unknown"
)
antidep_y <- mean(ad_rows$y[ad_rows$what %in% c("Trazodone", "Tryclic-Antidepressant")])

axed_align_y <- ad_rows |>
  filter(what %in% tlfb_only_raw_map) |>
  select(raw = what, y) |>
  mutate(what = names(tlfb_only_raw_map)[match(raw, tlfb_only_raw_map)]) |>
  select(what, y) |>
  bind_rows(tibble(what = "Antidepressant", y = antidep_y))

tlfb_only_all <- tlfb_pos |> filter(rel_type == "tlfb_only")
tlfb_only_axed <- tlfb_only_all |>
  inner_join(axed_align_y, by = "what")
tlfb_only_free_pool <- tlfb_only_all |>
  filter(!what %in% axed_align_y$what)

# free_y: row positions not taken by any aligned or axed-aligned entry
used_y  <- sort(unique(round(c(tlfb_aligned$y, tlfb_only_axed$y), 6)), decreasing = TRUE)
all_y   <- row_y(seq_len(n_rows))
free_y  <- sort(setdiff(round(all_y, 6), used_y), decreasing = TRUE)

stopifnot("more unaligned tlfb rows than free y-positions" =
            nrow(tlfb_only_free_pool) <= length(free_y))
tlfb_only_free <- tlfb_only_free_pool |>
  mutate(y = free_y[seq_len(n())])

tlfb_rows <- bind_rows(tlfb_aligned, tlfb_only_axed, tlfb_only_free) |>
  mutate(x = col_x["tlfb"] + pad_x)

stopifnot(nrow(tlfb_rows) == 25L,
          all(!is.na(tlfb_rows$rel_type)),
          all(tlfb_rows$rel_type %in% names(REL_COLORS)))
cat("Task 4: tlfb relationship-type checks PASSED — ",
    nrow(tlfb_aligned), " aligned, ", nrow(tlfb_only_axed), " axed-aligned, ",
    nrow(tlfb_only_free), " free\n", sep = "")

# ---- arrow system: tlfb <-> adf and extra cross-category arrows ----

# Helper: left edge of adf filtered box (arrow start) and tlfb box (arrow end)
ADF_R  <- col_x["filtered"] + box_w + 1.1   # right of filtered panel
TLFB_L <- col_x["tlfb"] - 1.1               # left of tlfb panel

# 1. Two-way for "same" rel_type only
tlfb_arrows_both <- tlfb_rows |>
  filter(!is.na(align_cat), rel_type == "same") |>
  left_join(filt_rows |> select(what_grouped, adf_y = y),
            by = c("align_cat" = "what_grouped")) |>
  transmute(x = TLFB_L, xend = ADF_R, y = y, yend = adf_y)

# 2. One-way (adf → tlfb) for non-same aligned entries, excluding tlfb "Alcohol"
#    (Alcohol gets three explicit arrows below; tlfb_only has no arrows)
tlfb_arrows_one <- tlfb_rows |>
  filter(!is.na(align_cat),
         !rel_type %in% c("same", "tlfb_only"),
         what != "Alcohol") |>
  left_join(filt_rows |> select(what_grouped, adf_y = y),
            by = c("align_cat" = "what_grouped")) |>
  rename(tlfb_y = y) |>  # avoid transmute self-reference: y = adf_y then yend = y would shadow
  transmute(x = ADF_R, xend = TLFB_L, y = adf_y, yend = tlfb_y)

# 3. Extra partial arrows: adf Fentanyl and Suboxone → tlfb Opioid
opioid_tlfb_y <- tlfb_rows$y[tlfb_rows$what == "Opioid"]
stopifnot(length(opioid_tlfb_y) == 1L)
extra_partial_arrows <- filt_rows |>
  filter(what_grouped %in% c("Fentanyl", "Suboxone")) |>
  transmute(x = ADF_R, xend = TLFB_L, y = y, yend = opioid_tlfb_y,
            color = pal[what_grouped])

# 4. Alcohol: three one-way arrows from each adf alcohol category → tlfb Alcohol
alcohol_tlfb_y <- tlfb_rows$y[tlfb_rows$what == "Alcohol"]
stopifnot(length(alcohol_tlfb_y) == 1L)
alcohol_arrows <- filt_rows |>
  filter(what_grouped %in% c("Alcohol Heavy Amnt", "Alcohol Light Amnt", "Alcohol Missing Amnt")) |>
  transmute(x = ADF_R, xend = TLFB_L, y = y, yend = alcohol_tlfb_y)

# 5. Opium dotted arrow: adf Opioid → tlfb Heroin (Opium maps into tlfb Heroin)
opioid_adf_y  <- filt_rows$y[filt_rows$what_grouped == "Opioid"]
heroin_tlfb_y <- tlfb_rows$y[tlfb_rows$what == "Heroin"]
stopifnot(length(opioid_adf_y) == 1L, length(heroin_tlfb_y) == 1L)
opium_arrow <- tibble(
  x = ADF_R, xend = TLFB_L,
  y = opioid_adf_y, yend = heroin_tlfb_y,
  # label near tlfb end, offset above so it clears the arrow line
  lbl_x = ADF_R + 0.7 * (TLFB_L - ADF_R),
  lbl_y = opioid_adf_y + 0.7 * (heroin_tlfb_y - opioid_adf_y) + 0.65,
  color = pal["Opioid"]
)

# 6. Extra grouped arrows: adf Crack → tlfb Cocaine, adf Methamphetamine → tlfb Amphetamine
cocaine_tlfb_y <- tlfb_rows$y[tlfb_rows$what == "Cocaine"]
amphet_tlfb_y  <- tlfb_rows$y[tlfb_rows$what == "Amphetamine"]
stopifnot(length(cocaine_tlfb_y) == 1L, length(amphet_tlfb_y) == 1L)
extra_grouped_arrows <- bind_rows(
  filt_rows |> filter(what_grouped == "Crack") |>
    transmute(x = ADF_R, xend = TLFB_L, y = y, yend = cocaine_tlfb_y),
  filt_rows |> filter(what_grouped == "Methamphetamine") |>
    transmute(x = ADF_R, xend = TLFB_L, y = y, yend = amphet_tlfb_y)
)

# ---- legend / key (positioned to right of tlfb panel) ----
key_x_left  <- col_x["tlfb"] + box_w + 3       # left edge of key box
key_x_dot   <- key_x_left + 0.5                 # dot center x
key_x_txt   <- key_x_left + 1.5                 # label text start x
key_x_right <- key_x_left + 18                  # right edge (wide enough to prevent overflow)

tab_h       <- 1.8                              # blue header tab height
key_tab_top <- box_top - 0.5                    # top of entire key box
key_tab_bot <- key_tab_top - tab_h              # bottom of blue tab / top of white area
key_y0      <- key_tab_bot - 0.8               # first legend entry y
key_yd      <- 1.6                              # spacing (leaves room for 2-line labels)
key_y_note  <- key_y0 - length(REL_COLORS) * key_yd   # asterisk note y
key_y_bot   <- key_y_note - 1.5                # bottom of key box

# same: gradient-filled circle (drawn separately below)
# tlfb_grouped / tlfb_finer / partial: matching TLFB_TEXT_COLORS grays; tlfb_only: black
legend_df <- tibble(
  rel_type = names(REL_COLORS),
  label = c(
    "same: same drugs, same name",
    "tlfb_grouped: tlfb lumps adf categories",
    "tlfb_finer: tlfb more specific than adf",
    "partial: near match with grouping/\nungrouping inconsistencies",
    "tlfb_only {black}: no adf counterpart"
  ),
  text_col   = c("black",  TLFB_TEXT_COLORS["tlfb_grouped"], TLFB_TEXT_COLORS["tlfb_finer"],
                 TLFB_TEXT_COLORS["partial"], "#000000"),
  fill_col   = c("white",  TLFB_TEXT_COLORS["tlfb_grouped"], TLFB_TEXT_COLORS["tlfb_finer"],
                 TLFB_TEXT_COLORS["partial"], "#000000"),
  border_col = c("black",  TLFB_TEXT_COLORS["tlfb_grouped"], TLFB_TEXT_COLORS["tlfb_finer"],
                 TLFB_TEXT_COLORS["partial"], "#000000"),
  pt_shape   = c(21L, 19L, 19L, 19L, 19L),
  x = key_x_dot,
  y = key_y0 - (seq_len(length(REL_COLORS)) - 1) * key_yd
)

# Key box: square-cornered, white fill, black border
key_box_df <- tibble(
  x = c(key_x_left, key_x_right, key_x_right, key_x_left, key_x_left),
  y = c(key_y_bot,  key_y_bot,   key_tab_top,  key_tab_top,  key_y_bot),
  group = "key_box"
)
# Blue header tab polygon (top band of key box)
key_tab_df <- tibble(
  x = c(key_x_left, key_x_right, key_x_right, key_x_left, key_x_left),
  y = c(key_tab_bot, key_tab_bot, key_tab_top, key_tab_top, key_tab_bot),
  group = "key_tab"
)
key_title_df <- tibble(
  x = (key_x_left + key_x_right) / 2,
  y = (key_tab_bot + key_tab_top) / 2,
  label = "Key"
)
key_note_df <- tibble(
  x = key_x_txt, y = key_y_note,
  label = "* = opioid kept separate from Opioid group"
)

# Gradient circle for "same" legend entry (pink center → blue edge)
dot_r          <- 0.35
theta_seq      <- seq(0, 2 * pi, length.out = 65)
grad_dot_df    <- tibble(
  x = key_x_dot + dot_r * cos(theta_seq),
  y = legend_df$y[1] + dot_r * sin(theta_seq),
  group = "grad_dot"
)
pink_blue_grad <- grid::radialGradient(
  colours = c("deeppink", "mediumpurple", "royalblue"),
  stops   = c(0, 0.5, 1),
  cx1 = 0.5, cy1 = 0.5, r1 = 0,
  cx2 = 0.5, cy2 = 0.5, r2 = 0.5
)

# ---- OVERFLOW GUARDS ----
char_w   <- 0.16                          # approx data-units per char at base_size 11
longest  <- max(nchar(c(ad_rows$label, filt_rows$what_grouped, tlfb_rows$what)))
stopifnot(
  "a label is wider than its box" = longest * char_w <= box_w,
  "all_drugs rows below box floor" = min(ad_rows$y)   >= box_bot + 0.5,
  "all_drugs rows above box top"   = max(ad_rows$y)   <= box_top - 0.5,
  "tlfb rows out of box"           = all(tlfb_rows$y  >= box_bot + 0.5 &
                                         tlfb_rows$y  <= box_top - 0.5),
  "highlight box exceeds panel"    = all(hl_boxes$ymax <= box_top - 0.3 &
                                         hl_boxes$ymin >= box_bot + 0.3)
)
cat("Task 5 overflow guards PASSED (longest label = ", longest, " chars)\n", sep = "")

# ---- outer box polygons (3 panels) ----
panels <- bind_rows(
  rounded_rect(col_x["all_drugs"]-1, box_bot, col_x["all_drugs"]+box_w+1, box_top, r=0.8, group="all_drugs"),
  rounded_rect(col_x["filtered"]-1,  box_bot, col_x["filtered"]+box_w+1,  box_top, r=0.8, group="filtered"),
  rounded_rect(col_x["tlfb"]-1,      box_bot, col_x["tlfb"]+box_w+1,      box_top, r=0.8, group="tlfb")
)

# ---- highlight box polygons ----
hl_poly <- pmap_dfr(hl_boxes, function(what_grouped, cat_rank, ymin, ymax, xmin, xmax, color) {
  rounded_rect(xmin, ymin, xmax, ymax, r = 0.4, group = what_grouped) |>
    mutate(color = color, fill_color = alpha(color, 0.15))
})

# ---- arrows: highlight box right edge -> filtered label left ----
arrows_df <- hl_boxes |>
  left_join(filt_rows |> select(what_grouped, fy = y), by = "what_grouped") |>
  transmute(x = xmax + 0.1, xend = col_x["filtered"] - 0.4,
            y = (ymin + ymax) / 2, yend = fy)

titles_df <- tibble(
  label = c("all_drugs", "all_drugs_filtered", "tlfb"),
  x = c(col_x["all_drugs"]+box_w/2, col_x["filtered"]+box_w/2, col_x["tlfb"]+box_w/2),
  y = box_top + 1.2
)
fig_title_df <- tibble(
  label = "Cross-Study Comparison of Drug Feature Aggregation",
  x = (col_x["all_drugs"] + col_x["tlfb"] + box_w) / 2,
  y = box_top + 3.2
)

p <- ggplot() +
  # Panel backgrounds
  geom_polygon(data = panels, aes(x, y, group = group),
               fill = "#FFF4DC", color = "black", linewidth = 0.8) +
  # Highlight boxes (multi-source groups in all_drugs)
  geom_polygon(data = hl_poly, aes(x, y, group = group, color = I(color), fill = I(fill_color)),
               linewidth = 0.9) +
  # Arrows: highlight box → filtered label
  geom_segment(data = arrows_df, aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
               linewidth = 0.6, color = "grey20") +
  # Arrows: tlfb ↔ adf (two-way, same rel_type only) — black
  geom_segment(data = tlfb_arrows_both,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(ends = "both", length = unit(0.15, "cm"), type = "closed"),
               linewidth = 0.5, color = "black") +
  # Arrows: adf → tlfb (one-way, non-same aligned entries) — black
  geom_segment(data = tlfb_arrows_one,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(ends = "last", length = unit(0.15, "cm"), type = "closed"),
               linewidth = 0.5, color = "black") +
  # Extra partial arrows: adf Fentanyl/Suboxone → tlfb Opioid — drug-specific adf color
  geom_segment(data = extra_partial_arrows,
               aes(x = x, y = y, xend = xend, yend = yend, color = I(color)),
               arrow = arrow(ends = "last", length = unit(0.15, "cm"), type = "closed"),
               linewidth = 0.5) +
  # Extra grouped arrows: adf Crack/Methamphetamine → their tlfb targets — black
  geom_segment(data = extra_grouped_arrows,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(ends = "last", length = unit(0.15, "cm"), type = "closed"),
               linewidth = 0.5, color = "black") +
  # Alcohol arrows: three adf categories → tlfb Alcohol — black
  geom_segment(data = alcohol_arrows,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(ends = "last", length = unit(0.15, "cm"), type = "closed"),
               linewidth = 0.5, color = "black") +
  # Opium dotted arrow: adf Opioid → tlfb Heroin — adf Opioid color
  geom_segment(data = opium_arrow,
               aes(x = x, y = y, xend = xend, yend = yend, color = I(color)),
               arrow = arrow(ends = "last", length = unit(0.15, "cm"), type = "closed"),
               linetype = "dashed", linewidth = 0.5) +
  geom_text(data = opium_arrow,
            aes(x = lbl_x, y = lbl_y, label = "(Opium)", color = I(color)),
            size = 2.4, family = "Courier") +
  # Drug name labels: all_drugs (left), filtered (middle), tlfb (right)
  geom_text(data = ad_rows,
            aes(x, y, label = label, color = I(text_color)),
            hjust = 0, size = 3.1, family = "Courier", fontface = "bold") +
  geom_text(data = filt_rows, aes(x, y, label = what_grouped, color = I(color)),
            hjust = 0, size = 3.3, family = "Courier", fontface = "bold") +
  geom_text(data = tlfb_rows, aes(x, y, label = what, color = I(rel_color)),
            hjust = 0, size = 3.1, family = "Courier", fontface = "bold") +
  geom_text(data = titles_df, aes(x, y, label = label),
            size = 5.5, fontface = "bold") +
  # Key box: white fill, black border, square corners
  geom_polygon(data = key_box_df, aes(x, y, group = group),
               fill = "white", color = NA) +
  # Blue header tab
  geom_polygon(data = key_tab_df, aes(x, y, group = group),
               fill = "#2E5FAC", color = NA) +
  # Black outer border (drawn on top so it overlays both white box and blue tab)
  geom_polygon(data = key_box_df, aes(x, y, group = group),
               fill = NA, color = "black", linewidth = 0.8) +
  geom_segment(aes(x = key_x_left, xend = key_x_right,
                   y = key_tab_bot,  yend = key_tab_bot),
               color = "black", linewidth = 0.8) +
  # "Key" label in white on blue tab
  geom_text(data = key_title_df, aes(x, y, label = label),
            size = 4.5, fontface = "bold", color = "white") +
  # Gradient circle for "same" entry
  geom_polygon(data = grad_dot_df, aes(x, y, group = group),
               fill = pink_blue_grad, color = "black", linewidth = 0.4) +
  # Legend dots for non-"same" entries
  geom_point(data = legend_df |> filter(rel_type != "same"),
             aes(x, y, color = I(border_col), fill = I(fill_col), shape = I(pt_shape)),
             size = 2.5) +
  geom_text(data = legend_df, aes(x + 1, y, label = label, color = I(text_col)),
            hjust = 0, size = 2.6, family = "Courier", lineheight = 0.85) +
  # Key asterisk note
  geom_text(data = key_note_df, aes(x, y, label = label),
            hjust = 0, size = 2.4, family = "Courier", color = "black") +
  # Figure title
  geom_text(data = fig_title_df, aes(x, y, label = label),
            size = 5.5, fontface = "bold") +
  coord_equal(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(20, 20, 20, 20))

print(p)

dir.create("vignettes/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("vignettes/figures/drug_mapping_diagram.png", p,
       width = 17, height = 11, dpi = 200, bg = "white")
ggsave("vignettes/figures/drug_mapping_diagram.pdf", p,
       width = 17, height = 11, bg = "white")
cat("Task 6: wrote vignettes/figures/drug_mapping_diagram.{png,pdf}\n")

# No-key variant for slides: strip the 9 key layers (indices 15–23 in p$layers),
# keep main content (1–14) and figure title (24).
p_nokey <- p
p_nokey$layers <- p_nokey$layers[c(1:14, 24)]
ggsave("vignettes/figures/drug_mapping_diagram_nokey.png", p_nokey,
       width = 17, height = 11, dpi = 200, bg = "white")
cat("Task 6b: wrote vignettes/figures/drug_mapping_diagram_nokey.png\n")
