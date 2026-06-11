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
  summarize(y = mean(y), .groups = "drop") |>
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
  same         = "#4682B4",   # steelblue
  tlfb_grouped = "#D06010",   # orange
  tlfb_finer   = "#2C9A50",   # green
  partial      = "#9060C0",   # purple
  tlfb_only    = "#CC4444"    # red
)

tlfb_rel <- tribble(
  ~what,               ~align_cat,              ~rel_type,
  # same: semantically equivalent
  "Methadone",         "Methadone",             "same",
  "Benzodiazepine",    "Benzodiazepine",        "same",
  "Pcp",               "Pcp",                   "same",
  "Antiemetic",        "Antiemetic",            "same",
  "Sedatives",         "Sedatives",             "same",
  "Mdma/Hallucinogen", "MDMA/Hallucinogen",     "same",
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

tlfb_pos <- tlfb_drugs |>
  left_join(tlfb_rel, by = "what") |>
  mutate(rel_color = REL_COLORS[rel_type])

aligned_y <- filt_rows |> select(what_grouped, fy = y)
tlfb_aligned <- tlfb_pos |>
  filter(!is.na(align_cat)) |>
  left_join(aligned_y, by = c("align_cat" = "what_grouped")) |>
  arrange(align_cat, desc(events)) |>           # within group, higher-event entry is first
  group_by(align_cat) |>
  mutate(y = fy - (row_number() - 1L) * dy) |> # first entry at fy, next at fy-1, etc.
  ungroup()

used_y  <- sort(unique(tlfb_aligned$y), decreasing = TRUE)
all_y   <- row_y(seq_len(n_rows))
free_y  <- sort(setdiff(round(all_y, 6), round(used_y, 6)), decreasing = TRUE)

tlfb_unaligned <- tlfb_pos |>
  filter(is.na(align_cat)) |>
  mutate(y = free_y[seq_len(n())])

tlfb_rows <- bind_rows(tlfb_aligned, tlfb_unaligned) |>
  mutate(x = col_x["tlfb"] + pad_x)

stopifnot(nrow(tlfb_rows) == 25L,
          all(!is.na(tlfb_rows$rel_type)),
          all(tlfb_rows$rel_type %in% names(REL_COLORS)))
cat("Task 4: tlfb relationship-type checks PASSED — ",
    nrow(tlfb_aligned), " aligned, ", nrow(tlfb_unaligned), " unaligned\n", sep = "")

# ---- two-way arrows between tlfb and adf (aligned pairs only) ----
tlfb_arrows <- tlfb_rows |>
  filter(!is.na(align_cat)) |>
  left_join(filt_rows |> select(what_grouped, adf_y = y),
            by = c("align_cat" = "what_grouped")) |>
  transmute(
    x    = col_x["tlfb"] - 1.1,
    xend = col_x["filtered"] + box_w + 1.1,
    y    = y,
    yend = adf_y
  )

# ---- legend ----
legend_df <- tibble(
  rel_type = names(REL_COLORS),
  label = c(
    "same: same drugs, same name",
    "tlfb_grouped: tlfb lumps adf categories",
    "tlfb_finer: tlfb more specific than adf",
    "partial: same name, different drugs",
    "tlfb_only: no adf counterpart"
  ),
  color = unname(REL_COLORS),
  x = col_x["tlfb"] - 1,
  y = box_bot - 1.2 - (seq_len(length(REL_COLORS)) - 1) * 1.1
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

p <- ggplot() +
  geom_polygon(data = panels, aes(x, y, group = group),
               fill = "#FFF4DC", color = "black", linewidth = 0.8) +
  geom_polygon(data = hl_poly, aes(x, y, group = group, color = I(color), fill = I(fill_color)),
               linewidth = 0.9) +
  geom_segment(data = arrows_df, aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
               linewidth = 0.6, color = "grey20") +
  geom_segment(data = tlfb_arrows,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(ends = "both", length = unit(0.15, "cm"), type = "closed"),
               linewidth = 0.5, color = "grey30") +
  geom_text(data = ad_rows,
            aes(x, y, label = label, color = I(text_color)),
            hjust = 0, size = 3.1, family = "Courier", fontface = "bold") +
  geom_text(data = filt_rows, aes(x, y, label = what_grouped, color = I(color)),
            hjust = 0, size = 3.3, family = "Courier", fontface = "bold") +
  geom_text(data = tlfb_rows, aes(x, y, label = what, color = I(rel_color)),
            hjust = 0, size = 3.1, family = "Courier", fontface = "bold") +
  geom_text(data = titles_df, aes(x, y, label = label),
            size = 5.5, fontface = "bold") +
  geom_point(data = legend_df, aes(x, y, color = I(color)), size = 2.5) +
  geom_text(data = legend_df, aes(x + 0.5, y, label = label, color = I(color)),
            hjust = 0, size = 2.6, family = "Courier") +
  coord_equal(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(20, 20, 20, 20))

print(p)

dir.create("vignettes/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("vignettes/figures/drug_mapping_diagram.png", p,
       width = 13, height = 11, dpi = 200, bg = "white")
ggsave("vignettes/figures/drug_mapping_diagram.pdf", p,
       width = 13, height = 11, bg = "white")
cat("Task 6: wrote vignettes/figures/drug_mapping_diagram.{png,pdf}\n")
