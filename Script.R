# ============================================================================
# GROUP A  |  ADVANCED INDEX ANALYTICS (CLEANED DATA VERSION)
# ============================================================================
# This version loads from Book1_CLEANED_NO_NAMES.csv (no personal data)
# Run REMOVE_ALL_NAMES_COMPLETE.R first to generate the cleaned CSV
# ============================================================================

library(tidyverse)
library(scales)

set.seed(2026)

INK <- "#14202E"; MUTED <- "#7A8794"; GRIDC <- "#E4E8EC"
LOW <- "#B3202C"; MID <- "#E8B33C"; HIGH <- "#12715A"

theme_adv <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(
      plot.title = element_text(face = "bold", size = base * 1.45, colour = INK),
      plot.subtitle = element_text(size = base * 0.92, colour = MUTED, margin = margin(b = 14)),
      plot.caption = element_text(size = base * 0.72, colour = MUTED, hjust = 0, margin = margin(t = 12)),
      axis.title = element_text(size = base * 0.85, colour = MUTED),
      axis.text = element_text(colour = INK),
      panel.grid.major = element_line(colour = GRIDC, linewidth = 0.35),
      panel.grid.minor = element_blank(),
      plot.margin = margin(18, 22, 14, 18)
    )
}

sv <- function(n, p, w, h) {
  ggsave(paste0(n, ".png"), p, width = w, height = h, dpi = 320, bg = "white", limitsize = FALSE)
}

# ============================================================================
# LOAD CLEANED DATA (no personal identifiers)
# ============================================================================

dat <- read.csv("Book1_CLEANED_NO_NAMES.csv")

# ============================================================================
# CONVERT LIKERT SCALES
# ============================================================================

lk <- c(
  "strongly disagree" = 1, "disagree" = 2, "neutral" = 3,
  "neither agree nor disagree" = 3, "agree" = 4, "strongly agree" = 5,
  "very dissatisfied" = 1, "dissatisfied" = 2, "satisfied" = 4,
  "very satisfied" = 5, "very poor" = 1, "poor" = 2, "average" = 3,
  "good" = 4, "very good" = 5, "excellent" = 5, "fair" = 3,
  "never" = 1, "rarely" = 2, "sometimes" = 3, "often" = 4, "always" = 5,
  "not at all" = 1, "slightly" = 2, "moderately" = 3, "very" = 4,
  "extremely" = 5, "1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5
)

to_num <- function(x) {
  v <- if (is.numeric(x)) x else unname(lk[tolower(trimws(as.character(x)))])
  v <- as.numeric(v)
  v[!is.na(v) & (v < 1 | v > 5)] <- NA
  v
}

num_cols <- grep("_num$", names(dat), value = TRUE)
if (length(num_cols) == 0) {
  raw_likert <- grep("^q[0-9]+_[0-9]+_", names(dat), value = TRUE)
  for (cl in raw_likert) {
    cv <- to_num(dat[[cl]])
    if (sum(!is.na(cv)) >= 30) dat[[paste0(cl, "_num")]] <- cv
  }
  num_cols <- grep("_num$", names(dat), value = TRUE)
}

# ============================================================================
# BUILD INDICES
# ============================================================================

prefix_of <- function(x) str_extract(x, "^q[0-9]+_[0-9]+")
blocks <- split(num_cols, prefix_of(num_cols))
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
# FIND DEMOGRAPHIC FACTORS
# ============================================================================

find_factor <- function(patterns, min_lv = 3, max_lv = 10) {
  cand <- setdiff(grep(paste(patterns, collapse = "|"), names(dat), value = TRUE),
                  c(num_cols, IDX, raw_likert))
  for (cl in cand) {
    v <- tolower(trimws(as.character(dat[[cl]])))
    tb <- table(v); tb <- tb[tb >= 25]
    if (length(tb) >= min_lv && length(tb) <= max_lv) {
      out <- v; out[!out %in% names(tb)] <- NA
      return(list(col = cl, values = str_to_title(out)))
    }
  }
  NULL
}

FCT <- find_factor(c("semester", "year", "province", "region", "discipline", "faculty"))
if (!is.null(FCT)) dat$grp_factor <- FCT$values else dat$grp_factor <- NA

sector_col <- grep("sector|institution_type", names(dat), value = TRUE)[1]
gender_col <- grep("gender|sex", names(dat), value = TRUE)[1]

# ============================================================================
# TABLE 1: DESCRIPTIVE STATISTICS
# ============================================================================

T1 <- map_dfr(IDX, function(v) {
  x <- dat[[v]][!is.na(dat[[v]])]
  se <- sd(x) / sqrt(length(x))
  tibble(Index = LAB[[v]], N = length(x), Mean = round(mean(x), 2), SD = round(sd(x), 2),
         SE = round(se, 3), CI_low = round(mean(x) - 1.96 * se, 2),
         CI_high = round(mean(x) + 1.96 * se, 2), Min = round(min(x), 1),
         Q1 = round(quantile(x, .25), 1), Median = round(median(x), 1),
         Q3 = round(quantile(x, .75), 1), Max = round(max(x), 1),
         IQR = round(IQR(x), 2), CV_pct = round(sd(x) / mean(x) * 100, 1),
         Skewness = round(mean((x - mean(x))^3) / sd(x)^3, 3),
         Kurtosis = round(mean((x - mean(x))^4) / sd(x)^4 - 3, 3))
}) %>% arrange(desc(Mean))
write.csv(T1, "T1_descriptive_statistics.csv", row.names = FALSE)

# ============================================================================
# TABLE 2: PERFORMANCE BANDS
# ============================================================================

bandf <- function(v) cut(v, c(-Inf, 40, 60, 80, Inf),
                         labels = c("Low", "Moderate", "High", "Very High"))

T2 <- map_dfr(IDX, function(v) {
  b <- bandf(dat[[v]]); b <- b[!is.na(b)]
  tb <- as.integer(table(b)); pc <- round(100 * tb / sum(tb), 1)
  tibble(Index = LAB[[v]], N = sum(tb),
         Low_n = tb[1], Low_pct = pc[1],
         Moderate_n = tb[2], Moderate_pct = pc[2],
         High_n = tb[3], High_pct = pc[3],
         VeryHigh_n = tb[4], VeryHigh_pct = pc[4])
})
write.csv(T2, "T2_performance_bands.csv", row.names = FALSE)

# ============================================================================
# TABLE 3: CORRELATION MATRIX
# ============================================================================

CM <- cor(dat[, IDX], use = "pairwise.complete.obs")
dimnames(CM) <- list(LAB[IDX], LAB[IDX])
write.csv(as.data.frame(round(CM, 3)) %>% rownames_to_column("Index"),
          "T3_correlation_matrix.csv", row.names = FALSE)

# ============================================================================
# VISUAL 1: BULLET CHART
# ============================================================================

d1 <- T1 %>% mutate(Index = factor(Index, levels = rev(Index)))
gm <- mean(T1$Mean)

v1 <- ggplot(d1, aes(y = Index)) +
  annotate("rect", xmin = 0, xmax = 40, ymin = -Inf, ymax = Inf, fill = LOW, alpha = 0.055) +
  annotate("rect", xmin = 40, xmax = 60, ymin = -Inf, ymax = Inf, fill = MID, alpha = 0.055) +
  annotate("rect", xmin = 60, xmax = 80, ymin = -Inf, ymax = Inf, fill = HIGH, alpha = 0.045) +
  annotate("rect", xmin = 80, xmax = 100, ymin = -Inf, ymax = Inf, fill = HIGH, alpha = 0.10) +
  geom_vline(xintercept = gm, colour = INK, linetype = "22", linewidth = 0.5) +
  geom_linerange(aes(xmin = Q1, xmax = Q3), colour = "grey72", linewidth = 4.2) +
  geom_linerange(aes(xmin = CI_low, xmax = CI_high), colour = INK, linewidth = 1.1) +
  geom_point(aes(x = Mean, fill = Mean), shape = 21, size = 6.4, colour = "white", stroke = 1.1) +
  geom_text(aes(x = Mean, label = sprintf("%.1f", Mean)), colour = "white", size = 2.6, fontface = "bold") +
  scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, midpoint = 55, limits = c(0, 100), guide = "none") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0, 0)) +
  labs(title = "Index Performance Bullet Chart",
       subtitle = paste0("Point = mean, dark bar = 95% CI, grey bar = IQR. Dashed line = grand mean (", sprintf("%.1f", gm), ")"),
       x = "Score (0-100)", y = NULL,
       caption = "Bands: Low <40 | Moderate 40-60 | High 60-80 | Very High 80+") +
  theme_adv() +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(face = "bold"))

sv("V1_bullet_performance", v1, 12, 0.62 * nrow(d1) + 3.2)

# ============================================================================
# VISUAL 2: RIDGELINE DISTRIBUTION CASCADE
# ============================================================================

ord <- rev(T1$Index)
ridge <- map_dfr(seq_along(ord), function(i) {
  v <- IDX[match(ord[i], LAB[IDX])]
  x <- dat[[v]][!is.na(dat[[v]])]
  if (length(x) < 30) return(NULL)
  d <- density(x, from = 0, to = 100, n = 256, adjust = 1.1)
  h <- d$y / max(d$y) * 0.92
  tibble(row = i, Index = ord[i], x = c(d$x, rev(d$x)),
         y = c(i + h, rep(i, length(h))), mu = mean(x))
})

ridge_pts <- ridge %>% group_by(row, Index) %>% summarise(mu = first(mu), .groups = "drop")

v2 <- ggplot() +
  geom_polygon(data = ridge, aes(x, y, group = row, fill = mu), colour = "white",
               linewidth = 0.45, alpha = 0.93) +
  geom_segment(data = ridge_pts, aes(x = mu, xend = mu, y = row, yend = row + 0.9),
               colour = INK, linewidth = 0.45, linetype = "22") +
  geom_point(data = ridge_pts, aes(mu, row), colour = INK, size = 1.9) +
  scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, midpoint = 55, limits = c(0, 100), name = "Mean") +
  scale_y_continuous(breaks = ridge_pts$row, labels = ridge_pts$Index,
                     expand = expansion(add = c(0.25, 1.05))) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0, 0)) +
  labs(title = "Distribution Cascade",
       subtitle = "Kernel density of every index, ordered by mean. Dashed rule marks the mean",
       x = "Score (0-100)", y = NULL) +
  theme_adv() +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(face = "bold"))

sv("V2_ridgeline_cascade", v2, 12, 0.68 * nrow(ridge_pts) + 3)

# ============================================================================
# VISUAL 3: CORRELATION HEATMAP
# ============================================================================

hm <- as.data.frame(as.table(CM)) %>% set_names(c("X", "Y", "r")) %>%
  mutate(X = factor(X, levels = unname(LAB[IDX])),
         Y = factor(Y, levels = rev(unname(LAB[IDX])))) %>%
  filter(as.integer(X) + as.integer(Y) <= length(IDX) + 1)

v3 <- ggplot(hm, aes(X, Y, fill = r)) +
  geom_tile(colour = "white", linewidth = 0.9) +
  geom_text(aes(label = sprintf("%.2f", r), colour = abs(r) > 0.55), size = 3, fontface = "bold") +
  scale_colour_manual(values = c("grey25", "white"), guide = "none") +
  scale_fill_gradient2(low = "#2166AC", mid = "#F7F8FA", high = "#B2182B", midpoint = 0,
                       limits = c(-1, 1), name = "r") +
  labs(title = "Correlation Structure",
       subtitle = "Rows and columns ordered by clustering",
       x = NULL, y = NULL) +
  coord_fixed() +
  theme_adv() +
  theme(axis.text.x = element_text(angle = 42, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"), panel.grid = element_blank())

sv("V3_correlation_matrix", v3, 11.5, 10)

T1