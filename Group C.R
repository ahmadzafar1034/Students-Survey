# ============================================================================
# GROUP C  |  HIERARCHICAL & CATEGORICAL ANALYSIS
# ----------------------------------------------------------------------------
# Treemaps, mosaic compositions, university rankings, field-group breakdowns,
# nested donuts, bubble maps, and alluvial flows.
#
# Every number is PRINTED on the chart (no hovering needed).
# Tables use plain English.
# Outputs go DIRECTLY into the current working directory.
# Only PNG visuals and CSV tables are produced.
# Dependencies: tidyverse, readxl, janitor, scales, treemapify
# ============================================================================

library(tidyverse)
library(readxl)
library(janitor)
library(scales)

if (!requireNamespace("treemapify", quietly = TRUE)) install.packages("treemapify")
library(treemapify)

set.seed(2026)

# ---------------------------------------------------------------- palette ---
INK   <- "#14202E"
MUTED <- "#7A8794"
GRIDC <- "#E4E8EC"
LOW   <- "#B3202C"
MID   <- "#E8B33C"
HIGH  <- "#12715A"
NEGC  <- "#2C5F8A"
POSC  <- "#B3202C"

sect_pal  <- c(Public = "#2C5F8A", Private = "#D9772E")
band_pal  <- c(Poor = "#8C1720", `Needs work` = "#D46A5A",
               Good = "#5FA88E", `Very good` = "#12715A")
field_pal <- c("#2C5F8A", "#D9772E", "#12715A", "#8C4A83",
               "#D4A03C", "#47808C", "#C75B3F", "#6B8E5A",
               "#B0614E", "#5A7BA0", "#A89F40", "#7B5C9E")

theme_adv <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(
      plot.title       = element_text(face = "bold", size = base * 1.45,
                                      colour = INK, margin = margin(b = 4)),
      plot.subtitle    = element_text(size = base * 0.92, colour = MUTED,
                                      margin = margin(b = 14)),
      plot.caption     = element_text(size = base * 0.74, colour = MUTED,
                                      hjust = 0, margin = margin(t = 12)),
      axis.title       = element_text(size = base * 0.85, colour = MUTED),
      axis.text        = element_text(colour = INK),
      panel.grid.major = element_line(colour = GRIDC, linewidth = 0.35),
      panel.grid.minor = element_blank(),
      plot.margin      = margin(18, 26, 14, 18),
      legend.text      = element_text(size = base * 0.78),
      legend.title     = element_text(size = base * 0.82, colour = MUTED)
    )
}

sv <- function(n, p, w, h) {
  ggsave(paste0(n, ".png"), p, width = w, height = h,
         dpi = 320, bg = "white", limitsize = FALSE)
}

# ============================================================================
# 1  LOAD, CLEAN, BUILD INDICES
# ============================================================================

dat <- read_excel("Book1.xlsx") %>% clean_names()

dat <- dat %>%
  select(-matches("name|respondent_id|timestamp|email|phone|enrollment|roll_no|student_id",
                  ignore.case = TRUE))

dat <- dat[rowMeans(is.na(dat)) <= 0.6, ]
dat <- dat[!duplicated(dat), ]

norm_txt <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x %in% c("", "na", "n/a", "-", "none", "nil")] <- NA
  x
}

# ---- detect key columns ---------------------------------------------------
sector_col <- grep("sector|institution_type", names(dat), value = TRUE)[1]
gender_col <- grep("gender|sex", names(dat), value = TRUE)[1]
univ_col   <- grep("university_reported|university|institution", names(dat), value = TRUE)[1]
field_col  <- grep("field_group|discipline|faculty|program|field", names(dat), value = TRUE)[1]
age_col    <- grep("^q.*age|^age", names(dat), value = TRUE)[1]

if (!is.na(sector_col)) {
  s <- norm_txt(dat[[sector_col]])
  s[grepl("govt|government|public|federal", s)] <- "Public"
  s[grepl("pvt|private", s)] <- "Private"
  s[!s %in% c("Public", "Private")] <- NA
  dat$sector <- s
} else { dat$sector <- NA }

if (!is.na(gender_col)) {
  g <- norm_txt(dat[[gender_col]])
  g[g %in% c("m", "male")] <- "Male"
  g[g %in% c("f", "female")] <- "Female"
  g[!g %in% c("Male", "Female")] <- NA
  dat$gender <- g
} else { dat$gender <- NA }

if (!is.na(univ_col)) {
  dat$university <- str_to_title(str_squish(as.character(dat[[univ_col]])))
  dat$university[dat$university %in% c("", "Na", "N/A")] <- NA
} else { dat$university <- NA }

if (!is.na(field_col)) {
  dat$field <- str_to_title(str_squish(as.character(dat[[field_col]])))
  dat$field[dat$field %in% c("", "Na", "N/A")] <- NA
} else { dat$field <- NA }

if (!is.na(age_col)) {
  dat$age <- suppressWarnings(as.numeric(dat[[age_col]]))
  dat$age[dat$age < 15 | dat$age > 60] <- NA
} else { dat$age <- NA }

# ---- Likert ----------------------------------------------------------------
lk <- c("strongly disagree" = 1, "disagree" = 2, "neutral" = 3,
        "neither agree nor disagree" = 3, "agree" = 4, "strongly agree" = 5,
        "very dissatisfied" = 1, "dissatisfied" = 2, "satisfied" = 4,
        "very satisfied" = 5, "very poor" = 1, "poor" = 2, "average" = 3,
        "good" = 4, "very good" = 5, "excellent" = 5, "fair" = 3,
        "never" = 1, "rarely" = 2, "sometimes" = 3, "often" = 4, "always" = 5,
        "not at all" = 1, "slightly" = 2, "moderately" = 3, "very" = 4,
        "extremely" = 5, "1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5)

to_num <- function(x) {
  v <- if (is.numeric(x)) x else unname(lk[tolower(trimws(as.character(x)))])
  v <- as.numeric(v); v[!is.na(v) & (v < 1 | v > 5)] <- NA; v
}

raw_likert <- grep("^q[0-9]+_[0-9]+_", names(dat), value = TRUE)
raw_likert <- raw_likert[!grepl("_num$", raw_likert)]
for (cl in raw_likert) {
  cv <- to_num(dat[[cl]])
  if (sum(!is.na(cv)) >= 30) dat[[paste0(cl, "_num")]] <- cv
}

num_cols <- grep("_num$", names(dat), value = TRUE)

# ---- indices ---------------------------------------------------------------
blocks <- split(num_cols, str_extract(num_cols, "^q[0-9]+_[0-9]+"))
blocks <- blocks[sapply(blocks, length) >= 2]

label_for <- function(cols, pref) {
  stem <- cols %>% str_remove("_num$") %>% str_remove(paste0("^", pref, "_[0-9]+_"))
  w <- unlist(str_split(stem, "_")); w <- w[nchar(w) > 3]
  if (!length(w)) return(toupper(pref))
  str_to_title(names(sort(table(w), decreasing = TRUE))[1])
}

row_index <- function(d, cols) {
  m <- suppressWarnings(apply(as.matrix(d[, cols, drop = FALSE]), 2, as.numeric))
  if (is.null(dim(m))) m <- matrix(m, ncol = 1)
  o <- rowMeans(m, na.rm = TRUE) * 20
  o[rowSums(!is.na(m)) == 0] <- NA_real_
  pmin(100, pmax(0, o))
}

idx_tbl <- tibble(prefix = names(blocks)) %>%
  mutate(label = map2_chr(blocks, prefix, label_for), var = paste0("idx_", prefix))
for (i in seq_len(nrow(idx_tbl)))
  dat[[idx_tbl$var[i]]] <- row_index(dat, blocks[[idx_tbl$prefix[i]]])
dat$idx_overall <- row_index(dat, num_cols)
idx_tbl <- bind_rows(idx_tbl, tibble(prefix = "all", label = "Overall", var = "idx_overall"))
idx_tbl <- idx_tbl[sapply(idx_tbl$var, function(v) sum(!is.na(dat[[v]]))) >= 50, ]
idx_tbl <- idx_tbl[order(-sapply(idx_tbl$var, function(v) mean(dat[[v]], na.rm = TRUE))), ]

IDX <- idx_tbl$var
LAB <- setNames(idx_tbl$label, idx_tbl$var)

rate <- function(m) case_when(m >= 80 ~ "Very good", m >= 60 ~ "Good",
                              m >= 40 ~ "Needs work", TRUE ~ "Poor")
bandf <- function(v) cut(v, c(-Inf, 40, 60, 80, Inf),
                         labels = c("Poor", "Needs work", "Good", "Very good"))

# ============================================================================
# TABLE C1  UNIVERSITY RANKINGS
# ============================================================================

if (sum(!is.na(dat$university)) >= 50) {
  
  TC1 <- dat %>%
    filter(!is.na(university), !is.na(idx_overall)) %>%
    group_by(University = university) %>%
    summarise(Students = n(), `Average score` = round(mean(idx_overall), 1),
              Rating = rate(mean(idx_overall)),
              `Lowest score` = round(min(idx_overall), 1),
              `Highest score` = round(max(idx_overall), 1),
              .groups = "drop") %>%
    filter(Students >= 5) %>%
    arrange(desc(`Average score`)) %>%
    mutate(Rank = row_number()) %>%
    select(Rank, everything()) %>%
    mutate(`What this means` = paste0("Ranked #", Rank, " with ", Students,
                                      " students. Average ", `Average score`,
                                      "/100 (", Rating, ")."))
  
  write.csv(TC1, "TC1_university_rankings.csv", row.names = FALSE)
}

# ============================================================================
# TABLE C2  FIELD GROUP COMPARISON
# ============================================================================

if (sum(!is.na(dat$field)) >= 50) {
  
  TC2 <- dat %>%
    filter(!is.na(field), !is.na(idx_overall)) %>%
    group_by(`Field of study` = field) %>%
    summarise(Students = n(), `Average score` = round(mean(idx_overall), 1),
              Rating = rate(mean(idx_overall)),
              `Spread (SD)` = round(sd(idx_overall), 1),
              .groups = "drop") %>%
    filter(Students >= 10) %>%
    arrange(desc(`Average score`)) %>%
    mutate(Rank = row_number(),
           `What this means` = paste0("Students in ", `Field of study`,
                                      " gave an average of ", `Average score`,
                                      "/100 (", Rating, ").")) %>%
    select(Rank, everything())
  
  write.csv(TC2, "TC2_field_group_comparison.csv", row.names = FALSE)
}

# ============================================================================
# TABLE C3  SECTOR × GENDER CROSSTAB
# ============================================================================

if (sum(!is.na(dat$sector) & !is.na(dat$gender)) >= 50) {
  
  TC3 <- dat %>%
    filter(!is.na(sector), !is.na(gender), !is.na(idx_overall)) %>%
    group_by(Sector = sector, Gender = gender) %>%
    summarise(Students = n(), `Average score` = round(mean(idx_overall), 1),
              Rating = rate(mean(idx_overall)),
              .groups = "drop") %>%
    mutate(`Share of total` = paste0(round(100 * Students / sum(Students)), "%"),
           `What this means` = paste0(Gender, " students in ", Sector,
                                      " universities average ", `Average score`,
                                      "/100 (", Rating, "). They make up ",
                                      `Share of total`, " of all respondents."))
  
  write.csv(TC3, "TC3_sector_gender_crosstab.csv", row.names = FALSE)
}

# ============================================================================
# TABLE C4  CATEGORY DISTRIBUTION SUMMARY
# ============================================================================

TC4 <- tribble(
  ~Category, ~Column, ~Type,
  "Sector", "sector", "group",
  "Gender", "gender", "group",
  "Field of study", "field", "group",
  "University", "university", "group"
)

TC4_out <- map_dfr(seq_len(nrow(TC4)), function(i) {
  cl <- TC4$Column[i]
  if (!cl %in% names(dat)) return(NULL)
  v <- dat[[cl]]; v <- v[!is.na(v)]
  if (length(v) < 10) return(NULL)
  tb <- sort(table(v), decreasing = TRUE)
  tibble(Category = TC4$Category[i],
         `Total responses` = length(v),
         `Missing` = sum(is.na(dat[[cl]])),
         `Unique values` = length(tb),
         `Most common` = paste0(names(tb)[1], " (", tb[1], " students)"),
         `Second most common` = ifelse(length(tb) >= 2,
                                       paste0(names(tb)[2], " (", tb[2], " students)"), "-"),
         `Smallest group` = paste0(names(tb)[length(tb)], " (", tb[length(tb)], " students)"))
})
write.csv(TC4_out, "TC4_category_distribution.csv", row.names = FALSE)

# ============================================================================
# TABLE C5  SATISFACTION SEGMENTS
# ============================================================================

if (sum(!is.na(dat$idx_overall)) >= 50) {
  
  TC5 <- dat %>%
    filter(!is.na(idx_overall)) %>%
    mutate(Segment = as.character(bandf(idx_overall))) %>%
    group_by(Segment) %>%
    summarise(Students = n(), `Average score` = round(mean(idx_overall), 1),
              `Average age` = round(mean(age, na.rm = TRUE), 1),
              `% Female` = round(100 * sum(gender == "Female", na.rm = TRUE) /
                                   sum(!is.na(gender)), 1),
              `% Public sector` = round(100 * sum(sector == "Public", na.rm = TRUE) /
                                          sum(!is.na(sector)), 1),
              .groups = "drop") %>%
    mutate(`Share` = paste0(round(100 * Students / sum(Students)), "%"),
           Segment = factor(Segment, levels = c("Poor", "Needs work", "Good", "Very good"))) %>%
    arrange(Segment) %>%
    mutate(`What this means` = paste0(Share, " of students (", Students,
                                      ") fall in the ", Segment, " segment."))
  
  write.csv(TC5, "TC5_satisfaction_segments.csv", row.names = FALSE)
}

# ============================================================================
# TABLE C6  HOW TO READ THESE
# ============================================================================

TC6 <- tribble(
  ~Term, ~`Plain English meaning`,
  "Rank", "Position from highest average score (#1) to lowest.",
  "Students", "How many people from this university / field / group answered the survey.",
  "Average score", "All students' scores for a group added up and divided by the count. Out of 100.",
  "Rating", "A word for the score: 80+ Very good, 60-79 Good, 40-59 Needs work, below 40 Poor.",
  "Spread (SD)", "How far apart students' answers are. Low = everyone feels similarly; high = big disagreements.",
  "Share of total", "What percentage of all respondents come from this group.",
  "Segment", "Students grouped by their overall score: Poor (<40), Needs work (40-59), Good (60-79), Very good (80+).",
  "Field of study", "The academic discipline the student is enrolled in.",
  "Treemap", "A chart where each rectangle's SIZE shows how many students, and its COLOUR shows their average score."
)
write.csv(TC6, "TC6_how_to_read_group_c.csv", row.names = FALSE)

# ============================================================================
# C1  UNIVERSITY TREEMAP
# ============================================================================

if (sum(!is.na(dat$university)) >= 50) {
  
  tm <- dat %>%
    filter(!is.na(university), !is.na(idx_overall)) %>%
    group_by(University = university) %>%
    summarise(n = n(), m = mean(idx_overall), .groups = "drop") %>%
    filter(n >= 5) %>%
    arrange(desc(n)) %>%
    slice_head(n = 25) %>%
    mutate(lbl = paste0(str_trunc(University, 28), "\n",
                        n, " students  |  ", sprintf("%.0f", m), "/100"))
  
  c1 <- ggplot(tm, aes(area = n, fill = m, label = lbl)) +
    geom_treemap(colour = "white", linewidth = 2) +
    geom_treemap_text(colour = "white", place = "centre", size = 9,
                      fontface = "bold", reflow = TRUE, padding.x = unit(4, "pt"),
                      padding.y = unit(4, "pt")) +
    scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, midpoint = 55,
                         limits = c(0, 100), name = "Average\nscore") +
    labs(title = "University Treemap",
         subtitle = "Rectangle size = number of students. Colour = average satisfaction score (green = high, red = low)",
         caption = "Top 25 universities with 5+ respondents") +
    theme_adv() +
    theme(legend.position = "right")
  
  sv("C1_university_treemap", c1, 14, 10)
}

# ============================================================================
# C2  SECTOR × GENDER MOSAIC
# ============================================================================

if (sum(!is.na(dat$sector) & !is.na(dat$gender)) >= 50) {
  
  mos <- dat %>%
    filter(!is.na(sector), !is.na(gender), !is.na(idx_overall)) %>%
    group_by(sector, gender) %>%
    summarise(n = n(), m = round(mean(idx_overall), 1), .groups = "drop") %>%
    group_by(sector) %>%
    mutate(sec_n = sum(n), sec_w = sec_n / sum(dat$sector == first(sector), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(pct = round(100 * n / sum(n)),
           x_left = ifelse(sector == sort(unique(sector))[1], 0,
                           sum(n[sector == sort(unique(sector))[1]]) / sum(n)),
           lbl = paste0(sector, " / ", gender, "\n",
                        n, " students (", pct, "%)\n",
                        "Score: ", m, "/100"))
  
  tot <- sum(mos$n)
  rects <- mos %>%
    group_by(sector) %>%
    mutate(sw = sum(n) / tot) %>%
    arrange(sector, gender) %>%
    mutate(yh = n / sum(n),
           ymax = cumsum(yh),
           ymin = ymax - yh) %>%
    ungroup() %>%
    group_by(sector) %>%
    mutate(xmin = ifelse(sector == sort(unique(mos$sector))[1], 0,
                         sum(mos$n[mos$sector == sort(unique(mos$sector))[1]]) / tot),
           xmax = xmin + sw) %>%
    ungroup()
  
  c2 <- ggplot(rects) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = m),
              colour = "white", linewidth = 1.8) +
    geom_text(aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = lbl),
              colour = "white", fontface = "bold", size = 4, lineheight = 1.15) +
    scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, midpoint = 55,
                         limits = c(0, 100), name = "Average\nscore") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = "Sector × Gender Mosaic",
         subtitle = "Width shows each sector's share. Height shows gender split within. Colour is the average score",
         x = NULL, y = NULL) +
    theme_adv() +
    theme(axis.text = element_blank(), panel.grid = element_blank())
  
  sv("C2_sector_gender_mosaic", c2, 11, 8)
}

# ============================================================================
# C3  TOP 15 UNIVERSITY RANKING LOLLIPOP
# ============================================================================

if (exists("TC1") && nrow(TC1) >= 5) {
  
  top <- TC1 %>% slice_head(n = 15) %>%
    mutate(University = fct_reorder(str_trunc(University, 35), `Average score`))
  
  c3 <- ggplot(top, aes(`Average score`, University)) +
    geom_segment(aes(x = 0, xend = `Average score`, y = University, yend = University),
                 colour = "grey75", linewidth = 0.9) +
    geom_point(aes(fill = `Average score`), shape = 21, size = 10,
               colour = "white", stroke = 1.2) +
    geom_text(aes(label = sprintf("%.0f", `Average score`)),
              colour = "white", size = 3, fontface = "bold") +
    geom_text(aes(x = `Average score` + 3,
                  label = paste0(Students, " students  |  ", Rating)),
              hjust = 0, size = 3, colour = MUTED) +
    scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, midpoint = 55,
                         limits = c(0, 100), guide = "none") +
    scale_x_continuous(limits = c(0, 115), breaks = seq(0, 100, 20), expand = c(0, 0)) +
    labs(title = "Top 15 Universities by Student Satisfaction",
         subtitle = "Each dot shows the average score. The label gives student count and rating",
         x = "Average score (0-100)", y = NULL,
         caption = "Only universities with 5+ respondents are included") +
    theme_adv() +
    theme(panel.grid.major.y = element_blank(),
          axis.text.y = element_text(face = "bold", size = 10))
  
  sv("C3_university_ranking", c3, 13, 0.62 * nrow(top) + 3.2)
}

# ============================================================================
# C4  FIELD GROUP PERFORMANCE
# ============================================================================

if (sum(!is.na(dat$field)) >= 50) {
  
  fd <- dat %>%
    filter(!is.na(field), !is.na(idx_overall)) %>%
    group_by(Field = field) %>%
    summarise(n = n(), m = mean(idx_overall),
              se = sd(idx_overall) / sqrt(n()),
              .groups = "drop") %>%
    filter(n >= 10) %>%
    mutate(lo = m - 1.96 * se, hi = m + 1.96 * se,
           Field = fct_reorder(Field, m))
  
  gm <- mean(fd$m)
  
  c4 <- ggplot(fd, aes(m, Field)) +
    geom_vline(xintercept = gm, colour = MUTED, linetype = "22", linewidth = 0.5) +
    geom_linerange(aes(xmin = lo, xmax = hi), colour = "grey65", linewidth = 1.2) +
    geom_point(aes(fill = m), shape = 21, size = 9,
               colour = "white", stroke = 1.1) +
    geom_text(aes(label = sprintf("%.0f", m)), colour = "white",
              size = 3, fontface = "bold") +
    geom_text(aes(x = hi + 2, label = paste0("n=", n)),
              hjust = 0, size = 3, colour = MUTED) +
    scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, midpoint = 55,
                         limits = c(0, 100), guide = "none") +
    scale_x_continuous(limits = c(0, 108), breaks = seq(0, 100, 20), expand = c(0, 0)) +
    labs(title = "Satisfaction by Field of Study",
         subtitle = paste0("Dot = average score, bar = 95% confidence interval. Dashed line = grand mean (",
                           sprintf("%.0f", gm), ")"),
         x = "Average score (0-100)", y = NULL,
         caption = "Only fields with 10+ respondents shown") +
    theme_adv() +
    theme(panel.grid.major.y = element_blank(),
          axis.text.y = element_text(face = "bold", size = 11))
  
  sv("C4_field_group_performance", c4, 12, 0.62 * nrow(fd) + 3.2)
}

# ============================================================================
# C5  SATISFACTION SEGMENT DONUT
# ============================================================================

if (sum(!is.na(dat$idx_overall)) >= 50) {
  
  seg <- dat %>%
    filter(!is.na(idx_overall)) %>%
    mutate(Band = as.character(bandf(idx_overall))) %>%
    count(Band) %>%
    mutate(pct = round(100 * n / sum(n), 1),
           Band = factor(Band, levels = c("Very good", "Good", "Needs work", "Poor"))) %>%
    arrange(Band) %>%
    mutate(ymax = cumsum(pct), ymin = lag(ymax, default = 0),
           mid = (ymin + ymax) / 2,
           lbl = paste0(Band, "\n", pct, "%\n(", n, " students)"))
  
  c5 <- ggplot(seg, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.4, fill = Band)) +
    geom_rect(colour = "white", linewidth = 1.5) +
    geom_text(aes(x = 3.2, y = mid, label = lbl), colour = "white",
              fontface = "bold", size = 3.6, lineheight = 1.1) +
    annotate("text", x = 0, y = 50, label = paste0(sum(seg$n), "\nstudents"),
             size = 6, fontface = "bold", colour = INK, lineheight = 1.1) +
    coord_polar(theta = "y") +
    xlim(c(0, 4.5)) +
    scale_fill_manual(values = c(`Very good` = HIGH, Good = "#5FA88E",
                                 `Needs work` = "#D46A5A", Poor = LOW)) +
    labs(title = "Overall Satisfaction Segments",
         subtitle = "What share of students fall in each satisfaction band",
         caption = "Very good (80+) | Good (60-79) | Needs work (40-59) | Poor (<40)") +
    theme_void(base_size = 12) +
    theme(plot.title    = element_text(face = "bold", size = 17, colour = INK, hjust = 0.5),
          plot.subtitle = element_text(size = 11, colour = MUTED, hjust = 0.5,
                                       margin = margin(b = 10)),
          plot.caption  = element_text(size = 9, colour = MUTED, hjust = 0.5,
                                       margin = margin(t = 10)),
          legend.position = "none",
          plot.margin = margin(18, 18, 14, 18))
  
  sv("C5_satisfaction_donut", c5, 9, 9)
}

# ============================================================================
# C6  SECTOR × FIELD HEATMAP WITH VALUES IN EVERY CELL
# ============================================================================

if (sum(!is.na(dat$sector) & !is.na(dat$field)) >= 50) {
  
  hm <- dat %>%
    filter(!is.na(sector), !is.na(field), !is.na(idx_overall)) %>%
    group_by(Sector = sector, Field = field) %>%
    summarise(n = n(), m = round(mean(idx_overall), 1), .groups = "drop") %>%
    filter(n >= 5)
  
  if (nrow(hm) >= 4) {
    hm <- hm %>% mutate(lbl = paste0(sprintf("%.0f", m), "\n(n=", n, ")"))
    
    c6 <- ggplot(hm, aes(Sector, Field, fill = m)) +
      geom_tile(colour = "white", linewidth = 1.2) +
      geom_text(aes(label = lbl, colour = m > 65), fontface = "bold",
                size = 3.8, lineheight = 1.1) +
      scale_colour_manual(values = c("grey20", "white"), guide = "none") +
      scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, midpoint = 55,
                           limits = c(0, 100), name = "Average\nscore") +
      labs(title = "Average Score by Sector and Field of Study",
           subtitle = "Each cell shows the average score and number of respondents",
           x = NULL, y = NULL,
           caption = "Only cells with 5+ respondents shown") +
      theme_adv() +
      theme(axis.text.x = element_text(face = "bold", size = 12),
            axis.text.y = element_text(face = "bold", size = 10),
            panel.grid = element_blank())
    
    sv("C6_sector_field_heatmap", c6, 10, 0.6 * n_distinct(hm$Field) + 3.5)
  }
}

# ============================================================================
# C7  STACKED BANDS BY SECTOR
# ============================================================================

if (sum(!is.na(dat$sector) & !is.na(dat$idx_overall)) >= 50) {
  
  sb <- dat %>%
    filter(!is.na(sector), !is.na(idx_overall)) %>%
    mutate(Band = as.character(bandf(idx_overall)),
           Band = factor(Band, levels = c("Poor", "Needs work", "Good", "Very good"))) %>%
    count(sector, Band) %>%
    group_by(sector) %>%
    mutate(pct = round(100 * n / sum(n))) %>%
    ungroup()
  
  c7 <- ggplot(sb, aes(pct, sector, fill = Band)) +
    geom_col(width = 0.62, colour = "white", linewidth = 0.5) +
    geom_text(aes(label = ifelse(pct >= 5, paste0(pct, "%"), "")),
              position = position_stack(vjust = 0.5),
              colour = "white", size = 4.2, fontface = "bold") +
    scale_fill_manual(values = band_pal, name = NULL) +
    scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(x, "%")) +
    labs(title = "Satisfaction Breakdown: Public vs Private",
         subtitle = "What share of students in each sector rate their experience as Poor, Needs work, Good, or Very good",
         x = "Share of students", y = NULL) +
    theme_adv() +
    theme(panel.grid.major.y = element_blank(),
          axis.text.y = element_text(face = "bold", size = 14),
          legend.position = "bottom", legend.text = element_text(size = 11))
  
  sv("C7_bands_by_sector", c7, 12, 5.5)
}

# ============================================================================
# C8  BUBBLE CHART: UNIVERSITIES BY SIZE AND SCORE
# ============================================================================

if (sum(!is.na(dat$university)) >= 50) {
  
  bub <- dat %>%
    filter(!is.na(university), !is.na(idx_overall)) %>%
    group_by(University = university) %>%
    summarise(n = n(), m = mean(idx_overall),
              spread = sd(idx_overall), .groups = "drop") %>%
    filter(n >= 8) %>%
    arrange(desc(n)) %>%
    slice_head(n = 20)
  
  c8 <- ggplot(bub, aes(n, m)) +
    geom_point(aes(size = n, fill = m), shape = 21,
               colour = "white", stroke = 1.2, alpha = 0.92) +
    geom_text(aes(label = sprintf("%.0f", m)), size = 2.8,
              colour = "white", fontface = "bold") +
    geom_text(aes(label = str_trunc(University, 22), y = m + 2.8),
              size = 2.6, colour = INK, fontface = "bold") +
    scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, midpoint = 55,
                         limits = c(0, 100), name = "Score") +
    scale_size_continuous(range = c(6, 22), name = "Students") +
    scale_x_continuous(expand = expansion(mult = 0.12)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    labs(title = "University Bubble Map",
         subtitle = "Bubble size = number of students. Vertical position and colour = average score",
         x = "Number of respondents", y = "Average score (0-100)",
         caption = "Top 20 universities with 8+ respondents") +
    theme_adv() +
    theme(legend.position = "right")
  
  sv("C8_university_bubbles", c8, 13, 9)
}

# ============================================================================
# C9  ALLUVIAL FLOW: SECTOR → SATISFACTION → GENDER
# ============================================================================

if (sum(!is.na(dat$sector) & !is.na(dat$gender) & !is.na(dat$idx_overall)) >= 50) {
  
  al <- dat %>%
    filter(!is.na(sector), !is.na(gender), !is.na(idx_overall)) %>%
    mutate(Band = as.character(bandf(idx_overall))) %>%
    count(sector, Band, gender) %>%
    filter(n >= 3)
  
  from_sect <- al %>%
    group_by(sector) %>% summarise(total = sum(n), .groups = "drop") %>%
    arrange(desc(total)) %>%
    mutate(ymax = cumsum(total), ymin = lag(ymax, default = 0),
           mid = (ymin + ymax) / 2)
  
  c9 <- ggplot() +
    geom_col(data = al %>% mutate(
      Band = factor(Band, levels = c("Very good", "Good", "Needs work", "Poor"))),
      aes(x = sector, y = n, fill = Band), width = 0.55,
      colour = "white", linewidth = 0.4) +
    geom_text(data = al %>% mutate(
      Band = factor(Band, levels = c("Very good", "Good", "Needs work", "Poor"))),
      aes(x = sector, y = n, label = ifelse(n >= 15, n, "")),
      position = position_stack(vjust = 0.5),
      colour = "white", fontface = "bold", size = 3.8) +
    scale_fill_manual(values = c(`Very good` = HIGH, Good = "#5FA88E",
                                 `Needs work` = "#D46A5A", Poor = LOW),
                      name = "Satisfaction") +
    labs(title = "Satisfaction Composition by Sector",
         subtitle = "Each bar shows how students in that sector distribute across satisfaction bands",
         x = NULL, y = "Number of students") +
    theme_adv() +
    theme(axis.text.x = element_text(face = "bold", size = 14),
          legend.position = "bottom", legend.text = element_text(size = 11))
  
  sv("C9_sector_satisfaction_flow", c9, 10, 8)
}

# ============================================================================
# C10  AGE GROUP × SATISFACTION
# ============================================================================

if (sum(!is.na(dat$age) & !is.na(dat$idx_overall)) >= 50) {
  
  dat$age_grp <- cut(dat$age, breaks = c(0, 19, 21, 23, 25, 100),
                     labels = c("17-19", "20-21", "22-23", "24-25", "26+"))
  
  ag <- dat %>%
    filter(!is.na(age_grp), !is.na(idx_overall)) %>%
    group_by(`Age group` = age_grp) %>%
    summarise(n = n(), m = mean(idx_overall),
              se = sd(idx_overall) / sqrt(n()), .groups = "drop") %>%
    filter(n >= 10) %>%
    mutate(lo = m - 1.96 * se, hi = m + 1.96 * se)
  
  c10 <- ggplot(ag, aes(`Age group`, m, group = 1)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = NEGC, alpha = 0.15) +
    geom_line(colour = NEGC, linewidth = 1.4) +
    geom_point(aes(fill = m), shape = 21, size = 10,
               colour = "white", stroke = 1.2) +
    geom_text(aes(label = sprintf("%.0f", m)), colour = "white",
              size = 3.2, fontface = "bold") +
    geom_text(aes(y = m + 4, label = paste0("n=", n)),
              size = 3, colour = MUTED) +
    scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, midpoint = 55,
                         limits = c(0, 100), guide = "none") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    labs(title = "Satisfaction Across Age Groups",
         subtitle = "Line shows average score by age band, shading shows 95% confidence interval",
         x = "Age group", y = "Average score (0-100)") +
    theme_adv() +
    theme(axis.text.x = element_text(face = "bold", size = 12))
  
  sv("C10_age_group_satisfaction", c10, 11, 7.5)
}

# ============================================================================
# C11  FIELD GROUP TREEMAP
# ============================================================================

if (sum(!is.na(dat$field)) >= 50) {
  
  ftm <- dat %>%
    filter(!is.na(field), !is.na(idx_overall)) %>%
    group_by(Field = field) %>%
    summarise(n = n(), m = mean(idx_overall), .groups = "drop") %>%
    filter(n >= 5) %>%
    mutate(lbl = paste0(Field, "\n", n, " students\n", sprintf("%.0f", m), "/100"))
  
  if (nrow(ftm) >= 2) {
    c11 <- ggplot(ftm, aes(area = n, fill = m, label = lbl)) +
      geom_treemap(colour = "white", linewidth = 2) +
      geom_treemap_text(colour = "white", place = "centre", size = 10,
                        fontface = "bold", reflow = TRUE,
                        padding.x = unit(5, "pt"), padding.y = unit(5, "pt")) +
      scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, midpoint = 55,
                           limits = c(0, 100), name = "Average\nscore") +
      labs(title = "Field of Study Treemap",
           subtitle = "Rectangle size = student count. Colour = average satisfaction score",
           caption = "Only fields with 5+ respondents shown") +
      theme_adv() +
      theme(legend.position = "right")
    
    sv("C11_field_treemap", c11, 12, 9)
  }
}

# ============================================================================
# C12  MULTI-INDEX COMPARISON BY SECTOR (PAIRED BAR)
# ============================================================================

if (sum(!is.na(dat$sector)) >= 50 && length(IDX) >= 3) {
  
  mi <- map_dfr(IDX, function(v) {
    dat %>% filter(!is.na(sector), !is.na(.data[[v]])) %>%
      group_by(Sector = sector) %>%
      summarise(m = round(mean(.data[[v]]), 1), .groups = "drop") %>%
      mutate(Index = LAB[[v]])
  }) %>%
    mutate(Index = factor(Index, levels = rev(unname(LAB[IDX]))))
  
  c12 <- ggplot(mi, aes(m, Index, fill = Sector)) +
    geom_col(position = position_dodge(width = 0.72), width = 0.62,
             colour = "white", linewidth = 0.4) +
    geom_text(aes(label = sprintf("%.0f", m)),
              position = position_dodge(width = 0.72), hjust = -0.2,
              size = 3.3, fontface = "bold", colour = INK) +
    scale_fill_manual(values = sect_pal, name = NULL) +
    scale_x_continuous(limits = c(0, 110), breaks = seq(0, 100, 20), expand = c(0, 0)) +
    labs(title = "Every Index Compared: Public vs Private",
         subtitle = "Side-by-side bars with the average score printed at the end of each",
         x = "Average score (0-100)", y = NULL) +
    theme_adv() +
    theme(panel.grid.major.y = element_blank(),
          axis.text.y = element_text(face = "bold", size = 10),
          legend.position = "top", legend.text = element_text(size = 12))
  
  sv("C12_multi_index_by_sector", c12, 13, 0.55 * length(IDX) + 3.8)
}

TC4_out