# ============================================================================
# GROUP D  |  RELATIONSHIPS & SCATTER ANALYSIS
# ----------------------------------------------------------------------------
# Scatter plots, 3D visualization, bubble charts, density contours,
# scatter matrices, marginal distributions, correlation networks,
# and multi-dimensional index relationships.
#
# Every number is PRINTED on the chart (no hovering needed).
# Tables use plain English.
# Outputs go DIRECTLY into the current working directory.
# Only PNG visuals and CSV tables are produced.
# Dependencies: tidyverse, readxl, janitor, scales, rgl (for 3D)
# ============================================================================

library(tidyverse)
library(readxl)
library(janitor)
library(scales)

if (!requireNamespace("rgl", quietly = TRUE)) install.packages("rgl", dependencies = TRUE)
if (!requireNamespace("plot3D", quietly = TRUE)) install.packages("plot3D", dependencies = TRUE)

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

# ============================================================================
# TABLE D1  INDEX PAIR CORRELATIONS
# ============================================================================

if (length(IDX) >= 2) {
  
  TD1 <- map_dfr(seq_along(IDX), function(i) {
    map_dfr(seq(i+1, length(IDX)), function(j) {
      v1 <- dat[[IDX[i]][!is.na(dat[[IDX[i]]])
                         v2 <- dat[[IDX[j]][!is.na(dat[[IDX[j]]])
                                            both <- complete.cases(dat[, c(IDX[i], IDX[j])])
                                            if (sum(both) < 20) return(NULL)
                                            r <- cor(dat[both, IDX[i]], dat[both, IDX[j]])
                                            tt <- cor.test(dat[both, IDX[i]], dat[both, IDX[j]])
                                            tibble(
                                              `Index 1` = LAB[[IDX[i]]],
                                              `Index 2` = LAB[[IDX[j]]],
                                              `Correlation (r)` = round(r, 3),
                                              `Strength` = case_when(abs(r) >= .7 ~ "Very strong",
                                                                     abs(r) >= .5 ~ "Strong",
                                                                     abs(r) >= .3 ~ "Moderate",
                                                                     abs(r) >= .1 ~ "Weak",
                                                                     TRUE ~ "Very weak"),
                                              `p-value` = signif(tt$p.value, 4),
                                              `Students (n)` = sum(both),
                                              `What this means` = case_when(
                                                abs(r) >= .7 ~ paste0(LAB[[IDX[i]]], " and ", LAB[[IDX[j]]],
                                                                      " move together very closely across students."),
                                                abs(r) >= .5 ~ paste0("These two indices are closely linked; ",
                                                                      "better performance on one usually means better on the other."),
                                                abs(r) >= .3 ~ "There is a moderate connection between these two topics.",
                                                TRUE ~ "These topics are largely independent; students who rate one high don't necessarily rate the other high.")
                                            )
    })
  })
  
  write.csv(TD1, "TD1_index_pair_correlations.csv", row.names = FALSE)
}

# ============================================================================
# TABLE D2  AGE VS SATISFACTION
# ============================================================================

if (sum(!is.na(dat$age) & !is.na(dat$idx_overall)) >= 30) {
  
  TD2 <- dat %>%
    filter(!is.na(age), !is.na(idx_overall)) %>%
    summarise(
      `Age range` = paste0(round(min(age)), "-", round(max(age))),
      `Students` = n(),
      `Average satisfaction` = round(mean(idx_overall), 1),
      `Oldest age` = round(max(age)),
      `Youngest age` = round(min(age)),
      `Correlation with age` = round(cor(age, idx_overall), 3),
      `What this means` = paste0("Age correlates ",
                                 ifelse(cor(age, idx_overall) > 0, "positively", "negatively"),
                                 " with satisfaction (r = ", round(cor(age, idx_overall), 3), "). ",
                                 ifelse(abs(cor(age, idx_overall)) < .2, "The relationship is very weak.",
                                        ifelse(abs(cor(age, idx_overall)) < .4, "There is a mild relationship.",
                                               "There is a meaningful relationship.")))
    )
  
  write.csv(TD2, "TD2_age_satisfaction_correlation.csv", row.names = FALSE)
}

# ============================================================================
# TABLE D3  SECTOR × INDEX CORRELATIONS
# ============================================================================

if (sum(!is.na(dat$sector)) >= 50 && length(IDX) >= 2) {
  
  TD3 <- map_dfr(unique(dat$sector[!is.na(dat$sector)]), function(s) {
    d <- dat[dat$sector == s, ]
    idx1 <- IDX[1]; idx2 <- IDX[2]
    both <- complete.cases(d[, c(idx1, idx2)])
    if (sum(both) < 10) return(NULL)
    r <- cor(d[both, idx1], d[both, idx2])
    tt <- cor.test(d[both, idx1], d[both, idx2])
    tibble(Sector = s, `Students` = sum(both),
           `Correlation` = round(r, 3),
           `Strength` = case_when(abs(r) >= .5 ~ "Strong",
                                  abs(r) >= .3 ~ "Moderate",
                                  TRUE ~ "Weak"),
           `p-value` = signif(tt$p.value, 4))
  })
  
  if (nrow(TD3)) write.csv(TD3, "TD3_sector_index_correlations.csv", row.names = FALSE)
}

# ============================================================================
# TABLE D4  HOW TO READ GROUP D
# ============================================================================

TD4 <- tribble(
  ~Term, ~`Plain English meaning`,
  "Scatter plot", "Each dot represents one student. The dot's position horizontally and vertically shows their two scores.",
  "Regression line", "The best-fit line through the dots, showing the overall trend.",
  "Correlation (r)", "A number from -1 to +1 showing how closely two things move together. 0 means no relationship; 1 means perfect positive; -1 means perfect negative.",
  "Very strong correlation", "If one goes up, the other almost always goes up (or down). The dots cluster tightly around a line.",
  "Strong correlation", "General pattern but with scatter. Knowing one score gives useful information about the other.",
  "Moderate correlation", "A weaker relationship; the two aren't clearly linked but there is some tendency.",
  "Weak correlation", "Almost no visible pattern; the two topics are largely independent.",
  "p-value", "Whether the correlation is real or just chance. Below 0.05 = real and reliable.",
  "Bubble chart", "A scatter plot where the size of each dot shows a third piece of information.",
  "Density contour", "Lines connecting points with equal frequency; shows where dots cluster most densely.",
  "3D scatter", "A scatter plot with three dimensions: horizontal (X), vertical (Y), and depth (Z) axes."
)
write.csv(TD4, "TD4_how_to_read_group_d.csv", row.names = FALSE)

# ============================================================================
# D1  FIRST TWO INDICES: BASIC SCATTER WITH REGRESSION
# ============================================================================

if (length(IDX) >= 2) {
  
  d1_data <- dat %>%
    filter(!is.na(.data[[IDX[1]]]), !is.na(.data[[IDX[2]]])) %>%
    select(x = all_of(IDX[1]), y = all_of(IDX[2])) %>%
    slice_sample(n = min(nrow(dat), 800))
  
  if (nrow(d1_data) >= 30) {
    
    fit1 <- lm(y ~ x, data = d1_data)
    pred1 <- predict(fit1, interval = "confidence", level = 0.95)
    d1_fit <- cbind(d1_data, pred1) %>% arrange(x)
    r1 <- cor(d1_data$x, d1_data$y)
    
    d1 <- ggplot(d1_data, aes(x, y)) +
      geom_point(aes(fill = y), shape = 21, size = 3, colour = "white",
                 stroke = 0.6, alpha = 0.75) +
      geom_line(data = d1_fit, aes(y = fit), colour = INK, linewidth = 1.2) +
      geom_ribbon(data = d1_fit, aes(ymin = lwr, ymax = upr), alpha = 0.15,
                  fill = INK) +
      annotate("label", x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5,
               label = paste0("r = ", round(r1, 3), "\nn = ", nrow(d1_data)),
               size = 4, fontface = "bold", colour = INK,
               fill = alpha("white", 0.92), label.size = 0.25) +
      scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, midpoint = 55,
                           limits = c(0, 100), guide = "none") +
      scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      labs(title = paste0("Scatter: ", LAB[[IDX[1]]], " vs ", LAB[[IDX[2]]]),
           subtitle = "Each dot is a student. The line is the best-fit trend, shaded area is 95% confidence",
           x = LAB[[IDX[1]]], y = LAB[[IDX[2]]]) +
      theme_adv() +
      theme(panel.grid = element_line(colour = GRIDC, linewidth = 0.3))
    
    sv("D1_scatter_indices_1_2", d1, 11, 9.5)
  }
}

# ============================================================================
# D2  INDEX 1 VS AGE
# ============================================================================

if (sum(!is.na(dat$age) & !is.na(dat[[IDX[1]]])) >= 30) {
  
  d2_data <- dat %>%
    filter(!is.na(age), !is.na(.data[[IDX[1]]])) %>%
    select(x = age, y = all_of(IDX[1])) %>%
    slice_sample(n = min(nrow(.), 800))
  
  fit2 <- lm(y ~ x, data = d2_data)
  pred2 <- predict(fit2, interval = "confidence", level = 0.95)
  d2_fit <- cbind(d2_data, pred2) %>% arrange(x)
  r2 <- cor(d2_data$x, d2_data$y)
  
  d2 <- ggplot(d2_data, aes(x, y)) +
    geom_point(aes(fill = y), shape = 21, size = 3.2, colour = "white",
               stroke = 0.6, alpha = 0.75) +
    geom_line(data = d2_fit, aes(y = fit), colour = NEGC, linewidth = 1.2) +
    geom_ribbon(data = d2_fit, aes(ymin = lwr, ymax = upr), alpha = 0.12,
                fill = NEGC) +
    annotate("label", x = max(d2_data$x) * 0.95, y = min(d2_data$y) * 1.05,
             hjust = 1, vjust = 0,
             label = paste0("r = ", round(r2, 3), "\nSlope = ",
                            round(coef(fit2)[2], 3), " per year"),
             size = 3.8, fontface = "bold", colour = INK,
             fill = alpha("white", 0.92), label.size = 0.25) +
    scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, midpoint = 55,
                         limits = c(0, 100), guide = "none") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    labs(title = paste0(LAB[[IDX[1]]], " by Age"),
         subtitle = "Each dot is a student. The slope of the line shows if older/younger students are more satisfied",
         x = "Age (years)", y = LAB[[IDX[1]]]) +
    theme_adv() +
    theme(panel.grid = element_line(colour = GRIDC, linewidth = 0.3))
  
  sv("D2_scatter_age_index", d2, 11, 8.5)
}

# ============================================================================
# D3  FIRST INDEX VS SECOND INDEX WITH SECTOR COLOURING
# ============================================================================

if (length(IDX) >= 2 && sum(!is.na(dat$sector)) >= 50) {
  
  d3_data <- dat %>%
    filter(!is.na(.data[[IDX[1]]]), !is.na(.data[[IDX[2]]]),
           !is.na(sector)) %>%
    select(x = all_of(IDX[1]), y = all_of(IDX[2]), sector) %>%
    slice_sample(n = min(nrow(.), 700))
  
  d3 <- ggplot(d3_data, aes(x, y, colour = sector, fill = sector)) +
    geom_point(shape = 21, size = 3.5, stroke = 0.8, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.08, linewidth = 1.1) +
    scale_colour_manual(values = c(Public = "#2C5F8A", Private = "#D9772E"), name = NULL) +
    scale_fill_manual(values = c(Public = "#2C5F8A", Private = "#D9772E"), name = NULL) +
    scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    labs(title = paste0(LAB[[IDX[1]]], " vs ", LAB[[IDX[2]]], " by Sector"),
         subtitle = "Each dot is one student, coloured by sector. Lines show separate trends for each sector",
         x = LAB[[IDX[1]]], y = LAB[[IDX[2]]]) +
    theme_adv() +
    theme(panel.grid = element_line(colour = GRIDC, linewidth = 0.3),
          legend.position = "top", legend.text = element_text(size = 12))
  
  sv("D3_scatter_by_sector", d3, 11, 9)
}

# ============================================================================
# D4  BUBBLE CHART: 3 INDICES AT ONCE
# ============================================================================

if (length(IDX) >= 3) {
  
  d4_data <- dat %>%
    filter(!is.na(.data[[IDX[1]]]), !is.na(.data[[IDX[2]]]),
           !is.na(.data[[IDX[3]]])) %>%
    select(x = all_of(IDX[1]), y = all_of(IDX[2]), z = all_of(IDX[3])) %>%
    slice_sample(n = min(nrow(.), 500))
  
  d4 <- ggplot(d4_data, aes(x, y)) +
    geom_point(aes(size = z, fill = z), shape = 21, colour = "white",
               stroke = 0.8, alpha = 0.78) +
    scale_size_continuous(range = c(2, 14), name = LAB[[IDX[3]]], breaks = seq(0, 100, 25)) +
    scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, midpoint = 55,
                         limits = c(0, 100), name = LAB[[IDX[3]]], breaks = seq(0, 100, 25)) +
    scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    labs(title = paste0("Bubble Chart: Three Indices"),
         subtitle = paste0("X-axis = ", LAB[[IDX[1]]], ", Y-axis = ", LAB[[IDX[2]]], 
                           ", Bubble size and colour = ", LAB[[IDX[3]]]),
         x = LAB[[IDX[1]]], y = LAB[[IDX[2]]]) +
    theme_adv() +
    theme(panel.grid = element_line(colour = GRIDC, linewidth = 0.3),
          legend.position = "right")
  
  sv("D4_bubble_3_indices", d4, 12, 9.5)
}

# ============================================================================
# D5  DENSITY CONTOUR: CONCENTRATION MAP
# ============================================================================

if (length(IDX) >= 2) {
  
  d5_data <- dat %>%
    filter(!is.na(.data[[IDX[1]]]), !is.na(.data[[IDX[2]]])) %>%
    select(x = all_of(IDX[1]), y = all_of(IDX[2]]))
  
  if (nrow(d5_data) >= 50) {
    
    d5 <- ggplot(d5_data, aes(x, y)) +
      geom_point(alpha = 0.15, size = 1.5, colour = INK) +
      geom_density_2d_filled(contour_var = "ndensity", alpha = 0.7) +
      scale_fill_viridis_d(option = "plasma", name = "Density") +
      scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      labs(title = paste0("Density Contours: ", LAB[[IDX[1]]], " vs ", LAB[[IDX[2]]]),
           subtitle = "Where do students cluster? Darker regions = more students with that combination of scores",
           x = LAB[[IDX[1]]], y = LAB[[IDX[2]]]) +
      theme_adv() +
      theme(panel.grid = element_line(colour = GRIDC, linewidth = 0.3))
    
    sv("D5_density_contours", d5, 11, 9.5)
  }
}

# ============================================================================
# D6  SCATTER MATRIX (PAIRS PLOT) - SUBSET
# ============================================================================

if (length(IDX) >= 4) {
  
  subset_idx <- IDX[1:min(4, length(IDX))]
  d6_data <- dat[, subset_idx, drop = FALSE]
  d6_data <- d6_data[complete.cases(d6_data), ]
  
  if (nrow(d6_data) >= 50) {
    
    d6_list <- list()
    for (i in seq_along(subset_idx)) {
      for (j in seq_along(subset_idx)) {
        if (i == j) {
          # Diagonal: density
          p <- ggplot(d6_data, aes(x = .data[[subset_idx[i]]])) +
            geom_density(fill = HIGH, alpha = 0.6, colour = INK, linewidth = 0.5) +
            scale_x_continuous(limits = c(0, 100), breaks = c(0, 50, 100)) +
            theme_minimal(base_size = 9) +
            theme(panel.grid = element_blank(),
                  axis.title = element_blank(),
                  axis.text.y = element_blank())
        } else if (i < j) {
          # Upper triangle: empty
          p <- ggplot() + theme_void()
        } else {
          # Lower triangle: scatter
          p <- ggplot(d6_data, aes(x = .data[[subset_idx[j]]], y = .data[[subset_idx[i]]])) +
            geom_point(alpha = 0.3, size = 1.2, colour = NEGC) +
            geom_smooth(method = "lm", se = FALSE, colour = LOW, linewidth = 0.6) +
            scale_x_continuous(limits = c(0, 100), breaks = c(0, 50, 100)) +
            scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100)) +
            theme_minimal(base_size = 9) +
            theme(panel.grid = element_line(colour = "grey95", linewidth = 0.2),
                  axis.title = element_blank(),
                  axis.text = element_text(size = 7))
        }
        d6_list[[paste0(i, "_", j)]] <- p
      }
    }
    
    combined_d6 <- do.call(gridExtra::grid.arrange,
                           c(d6_list, ncol = length(subset_idx)))
    
    ggplot2::ggsave("D6_scatter_matrix.png", combined_d6, width = 11, height = 11,
                    dpi = 320, bg = "white")
  }
}

# ============================================================================
# D7  MARGINAL SCATTER: WITH HISTOGRAMS ON EDGES
# ============================================================================

if (length(IDX) >= 2) {
  
  d7_data <- dat %>%
    filter(!is.na(.data[[IDX[1]]]), !is.na(.data[[IDX[2]]])) %>%
    select(x = all_of(IDX[1]), y = all_of(IDX[2]])) %>%
    slice_sample(n = min(nrow(.), 600))
  
  if (nrow(d7_data) >= 50) {
    
    # Main scatter
    pscatter <- ggplot(d7_data, aes(x, y)) +
      geom_point(aes(fill = y), shape = 21, size = 2.5, colour = "white",
                 stroke = 0.5, alpha = 0.7) +
      geom_smooth(method = "loess", se = TRUE, alpha = 0.12, colour = INK,
                  linewidth = 0.9) +
      scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, midpoint = 55,
                           limits = c(0, 100), guide = "none") +
      scale_x_continuous(limits = c(0, 100)) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_adv(11) +
      theme(axis.title.x = element_blank(),
            panel.grid = element_line(colour = GRIDC, linewidth = 0.3))
    
    # X margin
    px <- ggplot(d7_data, aes(x)) +
      geom_histogram(bins = 25, fill = NEGC, colour = "white", linewidth = 0.2) +
      scale_x_continuous(limits = c(0, 100)) +
      theme_minimal() +
      theme(axis.text = element_blank(), axis.title = element_blank(),
            panel.grid = element_blank())
    
    # Y margin
    py <- ggplot(d7_data, aes(y)) +
      geom_histogram(bins = 25, fill = NEGC, colour = "white", linewidth = 0.2) +
      scale_x_continuous(limits = c(0, 100)) +
      coord_flip() +
      theme_minimal() +
      theme(axis.text = element_blank(), axis.title = element_blank(),
            panel.grid = element_blank())
    
    d7_combined <- gridExtra::grid.arrange(
      px, gridExtra::arrangeGrob(), py, pscatter,
      ncol = 2, nrow = 2, widths = c(3, 1), heights = c(1, 3))
    
    ggplot2::ggsave("D7_marginal_scatter.png", d7_combined, width = 11, height = 10,
                    dpi = 320, bg = "white")
  }
}

# ============================================================================
# D8  HEXBIN DENSITY HEATMAP
# ============================================================================

if (length(IDX) >= 2) {
  
  d8_data <- dat %>%
    filter(!is.na(.data[[IDX[1]]]), !is.na(.data[[IDX[2]]])) %>%
    select(x = all_of(IDX[1]), y = all_of(IDX[2]]))
  
  if (nrow(d8_data) >= 100) {
    
    if (!requireNamespace("hexbin", quietly = TRUE)) install.packages("hexbin")
    library(hexbin)
    
    d8 <- ggplot(d8_data, aes(x, y)) +
      geom_hex(aes(fill = after_stat(count)), bins = 20, colour = "white", linewidth = 0.3) +
      scale_fill_gradient(low = "#F7F7F7", high = HIGH, name = "Count") +
      scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      labs(title = paste0("Hexbin Density: ", LAB[[IDX[1]]], " vs ", LAB[[IDX[2]]]),
           subtitle = "Hexagons show where students cluster. Darker = more students",
           x = LAB[[IDX[1]]], y = LAB[[IDX[2]]]) +
      theme_adv() +
      theme(panel.grid = element_blank())
    
    sv("D8_hexbin_density", d8, 11, 9.5)
  }
}

# ============================================================================
# D9  CORRELATION STRENGTH RANKING
# ============================================================================

if (length(IDX) >= 2) {
  
  corr_data <- tibble(
    Pair = character(),
    Correlation = numeric(),
    Strength = character(),
    Interpretation = character()
  )
  
  for (i in seq_along(IDX)) {
    for (j in seq(i+1, length(IDX))) {
      both <- complete.cases(dat[, c(IDX[i], IDX[j])])
      if (sum(both) >= 20) {
        r <- cor(dat[both, IDX[i]], dat[both, IDX[j]])
        str_val <- case_when(
          abs(r) >= .7 ~ "Very strong", abs(r) >= .5 ~ "Strong",
          abs(r) >= .3 ~ "Moderate", abs(r) >= .1 ~ "Weak", TRUE ~ "Very weak"
        )
        interp <- ifelse(r > 0, "Positive", "Negative")
        corr_data <- bind_rows(corr_data, tibble(
          Pair = paste0(LAB[[IDX[i]]], " ↔ ", LAB[[IDX[j]]]),
          Correlation = round(r, 3),
          Strength = str_val,
          Interpretation = interp,
          Students = sum(both)
        ))
      }
    }
  }
  
  if (nrow(corr_data)) {
    corr_data <- corr_data %>% arrange(desc(abs(Correlation)))
    write.csv(corr_data, "TD5_correlation_rankings.csv", row.names = FALSE)
  }
}

# ============================================================================
# D10  MULTI-INDEX SCATTER: INDEX 1 VS INDEX 3
# ============================================================================

if (length(IDX) >= 3) {
  
  d10_data <- dat %>%
    filter(!is.na(.data[[IDX[1]]]), !is.na(.data[[IDX[3]]])) %>%
    select(x = all_of(IDX[1]), y = all_of(IDX[3]])) %>%
    slice_sample(n = min(nrow(.), 700))
  
  if (nrow(d10_data) >= 30) {
    
    fit10 <- lm(y ~ x, data = d10_data)
    pred10 <- predict(fit10, interval = "confidence", level = 0.95)
    d10_fit <- cbind(d10_data, pred10) %>% arrange(x)
    r10 <- cor(d10_data$x, d10_data$y)
    
    d10 <- ggplot(d10_data, aes(x, y)) +
      geom_point(aes(fill = x), shape = 21, size = 3.2, colour = "white",
                 stroke = 0.6, alpha = 0.75) +
      geom_line(data = d10_fit, aes(y = fit), colour = POSC, linewidth = 1.2) +
      geom_ribbon(data = d10_fit, aes(ymin = lwr, ymax = upr), alpha = 0.15,
                  fill = POSC) +
      annotate("label", x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5,
               label = paste0("r = ", round(r10, 3), "\nn = ", nrow(d10_data)),
               size = 4, fontface = "bold", colour = INK,
               fill = alpha("white", 0.92), label.size = 0.25) +
      scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, midpoint = 55,
                           limits = c(0, 100), guide = "none") +
      scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      labs(title = paste0("Scatter: ", LAB[[IDX[1]]], " vs ", LAB[[IDX[3]]]),
           subtitle = "Each dot is a student. The line is the best-fit trend, shaded area is 95% confidence",
           x = LAB[[IDX[1]]], y = LAB[[IDX[3]]]) +
      theme_adv() +
      theme(panel.grid = element_line(colour = GRIDC, linewidth = 0.3))
    
    sv("D10_scatter_indices_1_3", d10, 11, 9.5)
  }
}

# ============================================================================
# D11  SCATTER COLORED BY OVERALL SATISFACTION
# ============================================================================

if (length(IDX) >= 2 && sum(!is.na(dat$idx_overall)) >= 50) {
  
  d11_data <- dat %>%
    filter(!is.na(.data[[IDX[1]]]), !is.na(.data[[IDX[2]]]),
           !is.na(idx_overall)) %>%
    select(x = all_of(IDX[1]), y = all_of(IDX[2]]), z = idx_overall) %>%
    slice_sample(n = min(nrow(.), 700))
  
  d11 <- ggplot(d11_data, aes(x, y)) +
    geom_point(aes(fill = z), shape = 21, size = 3.5, colour = "white",
               stroke = 0.7, alpha = 0.8) +
    scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, midpoint = 55,
                         limits = c(0, 100), name = "Overall\nSatisfaction") +
    scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    labs(title = paste0(LAB[[IDX[1]]], " vs ", LAB[[IDX[2]]], " (colored by Overall Satisfaction)"),
         subtitle = "Each dot's colour shows the student's overall satisfaction level",
         x = LAB[[IDX[1]]], y = LAB[[IDX[2]]]) +
    theme_adv() +
    theme(panel.grid = element_line(colour = GRIDC, linewidth = 0.3),
          legend.position = "right")
  
  sv("D11_scatter_overall_color", d11, 12, 9.5)
}

# ============================================================================
# D12  INDEX PAIRS RANKED BY CORRELATION STRENGTH
# ============================================================================

if (exists("corr_data") && nrow(corr_data) >= 3) {
  
  d12_data <- corr_data %>%
    slice_head(n = 12) %>%
    mutate(Pair = str_trunc(Pair, 40),
           Pair = fct_reorder(Pair, Correlation))
  
  d12 <- ggplot(d12_data, aes(Correlation, Pair)) +
    geom_col(aes(fill = Correlation), width = 0.68) +
    geom_vline(xintercept = c(0.3, 0.5, 0.7), colour = MUTED,
               linetype = "22", linewidth = 0.4, alpha = 0.6) +
    geom_text(aes(label = sprintf("%.3f", Correlation)), hjust = -0.1,
              size = 3.8, fontface = "bold", colour = INK) +
    scale_fill_gradient2(low = NEGC, mid = "grey88", high = POSC,
                         midpoint = 0, limits = c(-1, 1), guide = "none") +
    scale_x_continuous(limits = c(-1.1, 1.1), breaks = seq(-1, 1, 0.2)) +
    labs(title = "Top 12 Index Correlations",
         subtitle = "Strongest relationships between different topics. Vertical lines mark weak (0.3), moderate (0.5), and strong (0.7) thresholds",
         x = "Correlation strength (r)", y = NULL) +
    theme_adv() +
    theme(panel.grid.major.y = element_blank(),
          axis.text.y = element_text(face = "bold", size = 10))
  
  sv("D12_correlation_ranking", d12, 12, 0.6 * nrow(d12_data) + 3.4)
}

TD4