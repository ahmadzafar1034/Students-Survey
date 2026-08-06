# ============================================================================
# GROUP B  |  STATISTICAL ANALYSIS
# ----------------------------------------------------------------------------
# Inference layer: item diagnostics, group testing with effect sizes,
# ANOVA, bootstrap estimation, driver regression, distributional comparison.
#
# Outputs are written DIRECTLY into the current working directory.
# Only PNG visuals and CSV tables are produced.
# Dependencies: tidyverse, readxl, janitor, scales  (+ stats, base)
# ============================================================================

library(tidyverse)
library(readxl)
library(janitor)
library(scales)

set.seed(2026)

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
      plot.caption     = element_text(size = base * 0.72, colour = MUTED,
                                      hjust = 0, margin = margin(t = 12)),
      axis.title       = element_text(size = base * 0.85, colour = MUTED),
      axis.text        = element_text(colour = INK),
      panel.grid.major = element_line(colour = GRIDC, linewidth = 0.35),
      panel.grid.minor = element_blank(),
      plot.margin      = margin(18, 22, 14, 18),
      legend.title     = element_text(size = base * 0.8, colour = MUTED),
      legend.text      = element_text(size = base * 0.78)
    )
}

sv <- function(name, plot, w, h) {
  ggsave(paste0(name, ".png"), plot, width = w, height = h,
         dpi = 320, bg = "white", limitsize = FALSE)
}

# ============================================================================
# 1  LOAD, CLEAN, BUILD INDICES  (self-contained, mirrors Group A)
# ============================================================================

dat <- read_excel("Book1.xlsx") %>% clean_names()
dat <- dat[rowMeans(is.na(dat)) <= 0.6, ]
dat <- dat[!duplicated(dat), ]

norm_txt <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x %in% c("", "na", "n/a", "-", "none", "nil")] <- NA
  x
}

sector_col <- grep("sector", names(dat), value = TRUE)[1]
gender_col <- grep("gender|sex", names(dat), value = TRUE)[1]

if (!is.na(sector_col)) {
  s <- norm_txt(dat[[sector_col]])
  s[grepl("govt|government|public", s)] <- "Public"
  s[grepl("pvt|private", s)] <- "Private"
  s[!s %in% c("Public", "Private")] <- NA
  dat[[sector_col]] <- s
}
if (!is.na(gender_col)) {
  g <- norm_txt(dat[[gender_col]])
  g[g %in% c("m", "male")] <- "Male"
  g[g %in% c("f", "female")] <- "Female"
  g[!g %in% c("Male", "Female")] <- NA
  dat[[gender_col]] <- g
}

lk <- c(
  "strongly disagree" = 1, "disagree" = 2, "neutral" = 3,
  "neither agree nor disagree" = 3, "agree" = 4, "strongly agree" = 5,
  "very dissatisfied" = 1, "dissatisfied" = 2, "satisfied" = 4,
  "very satisfied" = 5, "very poor" = 1, "poor" = 2, "average" = 3,
  "good" = 4, "very good" = 5, "excellent" = 5, "fair" = 3,
  "never" = 1, "rarely" = 2, "sometimes" = 3, "often" = 4, "always" = 5,
  "not at all" = 1, "slightly" = 2, "moderately" = 3,
  "very" = 4, "extremely" = 5,
  "1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5
)

to_num <- function(x) {
  v <- if (is.numeric(x)) x else unname(lk[tolower(trimws(as.character(x)))])
  v <- as.numeric(v)
  v[!is.na(v) & (v < 1 | v > 5)] <- NA
  v
}

raw_likert <- grep("^q[0-9]+_[0-9]+_", names(dat), value = TRUE)
raw_likert <- raw_likert[!grepl("_num$", raw_likert)]
for (cl in raw_likert) {
  cv <- to_num(dat[[cl]])
  if (sum(!is.na(cv)) >= 30) dat[[paste0(cl, "_num")]] <- cv
}

num_cols  <- grep("_num$", names(dat), value = TRUE)
prefix_of <- function(x) str_extract(x, "^q[0-9]+_[0-9]+")
blocks    <- split(num_cols, prefix_of(num_cols))
blocks    <- blocks[sapply(blocks, length) >= 2]

label_for <- function(cols, pref) {
  stem  <- cols %>% str_remove("_num$") %>%
    str_remove(paste0("^", pref, "_[0-9]+_"))
  words <- unlist(str_split(stem, "_"))
  words <- words[nchar(words) > 3]
  if (!length(words)) return(toupper(pref))
  paste0(str_to_title(names(sort(table(words), decreasing = TRUE))[1]),
         " (", toupper(pref), ")")
}

item_label <- function(x) {
  x %>% str_remove("_num$") %>% str_remove("^q[0-9]+_[0-9]+_[0-9]+_") %>%
    str_replace_all("_", " ") %>% str_to_sentence() %>% str_trunc(42)
}

row_index <- function(d, cols) {
  m <- suppressWarnings(apply(as.matrix(d[, cols, drop = FALSE]), 2, as.numeric))
  if (is.null(dim(m))) m <- matrix(m, ncol = 1)
  n_ok <- rowSums(!is.na(m))
  out  <- rowMeans(m, na.rm = TRUE) * 20
  out[n_ok == 0] <- NA_real_
  pmin(100, pmax(0, out))
}

idx_tbl <- tibble(prefix = names(blocks)) %>%
  mutate(label = map2_chr(blocks, prefix, label_for),
         var   = paste0("idx_", prefix))
for (i in seq_len(nrow(idx_tbl))) {
  dat[[idx_tbl$var[i]]] <- row_index(dat, blocks[[idx_tbl$prefix[i]]])
}
dat$idx_overall <- row_index(dat, num_cols)
idx_tbl <- bind_rows(idx_tbl,
                     tibble(prefix = "all", label = "Overall Index",
                            var = "idx_overall"))
idx_tbl <- idx_tbl[sapply(idx_tbl$var, function(v) sum(!is.na(dat[[v]]))) >= 50, ]
idx_tbl <- idx_tbl[order(-sapply(idx_tbl$var,
                                 function(v) mean(dat[[v]], na.rm = TRUE))), ]

IDX <- idx_tbl$var
LAB <- setNames(idx_tbl$label, idx_tbl$var)

# ---------------------------------------------------- demographic detection --
find_factor <- function(patterns, min_lv = 3, max_lv = 10) {
  cand <- unique(unlist(lapply(patterns, function(p)
    grep(p, names(dat), value = TRUE))))
  cand <- setdiff(cand, c(num_cols, IDX, raw_likert))
  for (cl in cand) {
    v <- norm_txt(dat[[cl]])
    tb <- table(v)
    tb <- tb[tb >= 25]
    if (length(tb) >= min_lv && length(tb) <= max_lv) {
      out <- v; out[!out %in% names(tb)] <- NA
      return(list(col = cl, values = str_to_title(out)))
    }
  }
  NULL
}

FCT <- find_factor(c("semester", "year_of_study", "province", "region",
                     "discipline", "faculty", "program", "degree", "field"))
if (!is.null(FCT)) dat$grp_factor <- FCT$values else dat$grp_factor <- NA

# ============================================================================
# TB1  ITEM-LEVEL DIAGNOSTICS
# ============================================================================

alpha_of <- function(m) {
  m <- m[complete.cases(m), , drop = FALSE]
  if (nrow(m) < 10 || ncol(m) < 2) return(NA_real_)
  k <- ncol(m)
  k / (k - 1) * (1 - sum(apply(m, 2, var)) / var(rowSums(m)))
}

TB1 <- map_dfr(names(blocks), function(p) {
  cols <- blocks[[p]]
  m <- as.matrix(dat[, cols, drop = FALSE])
  mc <- m[complete.cases(m), , drop = FALSE]
  if (nrow(mc) < 20) return(NULL)
  full_a <- alpha_of(mc)
  map_dfr(seq_along(cols), function(j) {
    rest <- rowSums(mc[, -j, drop = FALSE])
    tibble(
      Scale        = label_for(cols, p),
      Item         = item_label(cols[j]),
      Variable     = cols[j],
      N            = sum(!is.na(dat[[cols[j]]])),
      Mean         = round(mean(dat[[cols[j]]], na.rm = TRUE), 3),
      SD           = round(sd(dat[[cols[j]]], na.rm = TRUE), 3),
      Item_Total_r = round(cor(mc[, j], rest), 3),
      Alpha_if_dropped = round(alpha_of(mc[, -j, drop = FALSE]), 3),
      Scale_Alpha  = round(full_a, 3),
      Flag         = ifelse(cor(mc[, j], rest) < 0.30, "Weak item", "")
    )
  })
})
if (nrow(TB1)) write.csv(TB1, "TB1_item_diagnostics.csv", row.names = FALSE)

# ============================================================================
# TB2  GROUP COMPARISON WITH EFFECT SIZES AND FDR CORRECTION
# ============================================================================

compare_groups <- function(gcol, gname) {
  if (is.na(gcol) || !gcol %in% names(dat)) return(NULL)
  d <- dat[!is.na(dat[[gcol]]), ]
  lv <- sort(unique(d[[gcol]]))
  if (length(lv) != 2) return(NULL)
  out <- map_dfr(IDX, function(v) {
    a <- d[[v]][d[[gcol]] == lv[1]]; a <- a[!is.na(a)]
    b <- d[[v]][d[[gcol]] == lv[2]]; b <- b[!is.na(b)]
    if (length(a) < 10 || length(b) < 10) return(NULL)
    tt <- t.test(a, b)
    sp <- sqrt(((length(a) - 1) * var(a) + (length(b) - 1) * var(b)) /
                 (length(a) + length(b) - 2))
    d_val <- (mean(a) - mean(b)) / sp
    se_d  <- sqrt((length(a) + length(b)) / (length(a) * length(b)) +
                    d_val^2 / (2 * (length(a) + length(b))))
    tibble(Comparison = gname, Index = LAB[[v]],
           Level_A = lv[1], N_A = length(a), Mean_A = round(mean(a), 2),
           Level_B = lv[2], N_B = length(b), Mean_B = round(mean(b), 2),
           Difference = round(mean(a) - mean(b), 2),
           t = round(unname(tt$statistic), 3),
           df = round(unname(tt$parameter), 1),
           p_value = tt$p.value,
           Cohens_d = round(d_val, 3),
           d_low = round(d_val - 1.96 * se_d, 3),
           d_high = round(d_val + 1.96 * se_d, 3),
           Magnitude = case_when(abs(d_val) < .2 ~ "Negligible",
                                 abs(d_val) < .5 ~ "Small",
                                 abs(d_val) < .8 ~ "Medium",
                                 TRUE ~ "Large"))
  })
  if (!nrow(out)) return(NULL)
  out %>% mutate(p_FDR = p.adjust(p_value, "BH"),
                 Significant = ifelse(p_FDR < .05, "Yes", "No"),
                 p_value = signif(p_value, 4),
                 p_FDR = signif(p_FDR, 4))
}

TB2 <- bind_rows(compare_groups(sector_col, "Sector"),
                 compare_groups(gender_col, "Gender"))
if (nrow(TB2)) write.csv(TB2, "TB2_group_tests_effect_sizes.csv", row.names = FALSE)

# ============================================================================
# TB3  ONE-WAY ANOVA ACROSS A MULTI-LEVEL FACTOR
# ============================================================================

TB3 <- NULL
if (!all(is.na(dat$grp_factor))) {
  TB3 <- map_dfr(IDX, function(v) {
    d <- dat[!is.na(dat$grp_factor) & !is.na(dat[[v]]), ]
    if (n_distinct(d$grp_factor) < 3 || nrow(d) < 60) return(NULL)
    fit <- aov(d[[v]] ~ factor(d$grp_factor))
    s   <- summary(fit)[[1]]
    ss_b <- s[["Sum Sq"]][1]; ss_w <- s[["Sum Sq"]][2]
    kw  <- kruskal.test(d[[v]] ~ factor(d$grp_factor))
    tibble(Index = LAB[[v]], N = nrow(d),
           Groups = n_distinct(d$grp_factor),
           df_between = s[["Df"]][1], df_within = s[["Df"]][2],
           F_stat = round(s[["F value"]][1], 3),
           p_value = s[["Pr(>F)"]][1],
           Eta_sq = round(ss_b / (ss_b + ss_w), 4),
           Omega_sq = round((ss_b - s[["Df"]][1] * s[["Mean Sq"]][2]) /
                              (ss_b + ss_w + s[["Mean Sq"]][2]), 4),
           Kruskal_p = signif(kw$p.value, 4))
  })
  if (!is.null(TB3) && nrow(TB3)) {
    TB3 <- TB3 %>% mutate(p_FDR = signif(p.adjust(p_value, "BH"), 4),
                          p_value = signif(p_value, 4),
                          Significant = ifelse(p_FDR < .05, "Yes", "No"))
    write.csv(TB3, "TB3_anova_by_group.csv", row.names = FALSE)
  }
}

# ============================================================================
# TB4  DRIVER REGRESSION
# ============================================================================

outcome  <- IDX[1]
preds    <- setdiff(IDX, c(outcome, "idx_overall"))
TB4 <- NULL; fit <- NULL

if (length(preds) >= 2) {
  md <- dat[, c(outcome, preds), drop = FALSE]
  if (!is.na(sector_col)) md$Sector <- factor(dat[[sector_col]])
  if (!is.na(gender_col)) md$Gender <- factor(dat[[gender_col]])
  md <- md[complete.cases(md), , drop = FALSE]
  
  if (nrow(md) >= 60) {
    zsc <- function(x) (x - mean(x)) / sd(x)
    mdz <- md
    mdz[[outcome]] <- zsc(mdz[[outcome]])
    for (p in preds) mdz[[p]] <- zsc(mdz[[p]])
    
    fit <- lm(as.formula(paste0("`", outcome, "` ~ .")), data = mdz)
    sm  <- summary(fit)
    ci  <- confint(fit)
    
    TB4 <- tibble(
      Term = rownames(sm$coefficients),
      Beta = round(sm$coefficients[, 1], 4),
      SE   = round(sm$coefficients[, 2], 4),
      t    = round(sm$coefficients[, 3], 3),
      p_value = signif(sm$coefficients[, 4], 4),
      CI_low  = round(ci[, 1], 4),
      CI_high = round(ci[, 2], 4)
    ) %>%
      mutate(Label = ifelse(Term %in% names(LAB), LAB[Term], Term),
             Significant = ifelse(p_value < .05, "Yes", "No")) %>%
      filter(Term != "(Intercept)") %>%
      arrange(desc(abs(Beta)))
    
    write.csv(TB4, "TB4_driver_regression.csv", row.names = FALSE)
    
    write.csv(tibble(
      Outcome = LAB[[outcome]], N = nrow(md),
      Predictors = length(coef(fit)) - 1,
      R_squared = round(sm$r.squared, 4),
      Adj_R_squared = round(sm$adj.r.squared, 4),
      F_stat = round(unname(sm$fstatistic[1]), 3),
      Residual_SE = round(sm$sigma, 4)
    ), "TB5_regression_model_fit.csv", row.names = FALSE)
  }
}

# ============================================================================
# TB6  BOOTSTRAP CONFIDENCE INTERVALS
# ============================================================================

boot_ci <- function(x, R = 2000) {
  x <- x[!is.na(x)]
  bs <- replicate(R, mean(sample(x, length(x), replace = TRUE)))
  c(mean(x), quantile(bs, c(.025, .975)), sd(bs))
}

TB6 <- map_dfr(IDX, function(v) {
  b <- boot_ci(dat[[v]])
  tibble(Index = LAB[[v]], N = sum(!is.na(dat[[v]])),
         Mean = round(b[1], 2),
         Boot_CI_low = round(b[2], 2), Boot_CI_high = round(b[3], 2),
         Boot_SE = round(b[4], 3),
         CI_width = round(b[3] - b[2], 2))
}) %>% arrange(desc(Mean))
write.csv(TB6, "TB6_bootstrap_intervals.csv", row.names = FALSE)

# ============================================================================
# TB7  DISTRIBUTION DIAGNOSTICS
# ============================================================================

TB7 <- map_dfr(IDX, function(v) {
  x <- dat[[v]][!is.na(dat[[v]])]
  sw <- if (length(x) >= 20) shapiro.test(sample(x, min(5000, length(x)))) else NULL
  q <- quantile(x, c(.25, .75)); iqr <- IQR(x)
  tibble(Index = LAB[[v]], N = length(x),
         Skewness = round(mean((x - mean(x))^3) / sd(x)^3, 3),
         Kurtosis = round(mean((x - mean(x))^4) / sd(x)^4 - 3, 3),
         Shapiro_W = if (is.null(sw)) NA else round(unname(sw$statistic), 4),
         Shapiro_p = if (is.null(sw)) NA else signif(sw$p.value, 4),
         Outliers_low  = sum(x < q[1] - 1.5 * iqr),
         Outliers_high = sum(x > q[2] + 1.5 * iqr),
         Outlier_pct = round(100 * (sum(x < q[1] - 1.5 * iqr) +
                                      sum(x > q[2] + 1.5 * iqr)) / length(x), 2),
         Shape = case_when(abs(mean((x - mean(x))^3) / sd(x)^3) < .5 ~ "Approx. symmetric",
                           mean((x - mean(x))^3) / sd(x)^3 <= -.5 ~ "Left skewed",
                           TRUE ~ "Right skewed"))
})
write.csv(TB7, "TB7_distribution_diagnostics.csv", row.names = FALSE)

# ============================================================================
# B1  ITEM-LEVEL DIVERGING LIKERT CHART
# ============================================================================

top_block <- names(which.max(sapply(blocks, length)))
bi <- blocks[[top_block]]
if (length(bi) > 16) {
  bi <- bi[order(-sapply(bi, function(c) sum(!is.na(dat[[c]]))))][1:16]
}

lik <- map_dfr(bi, function(c) {
  v <- dat[[c]][!is.na(dat[[c]])]
  tibble(Item = item_label(c), resp = factor(v, levels = 1:5)) %>%
    count(Item, resp, .drop = FALSE) %>%
    mutate(pct = 100 * n / sum(n))
})

lev_lab <- c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")
lik <- lik %>%
  mutate(Level = factor(lev_lab[as.integer(resp)], levels = lev_lab),
         signed = case_when(as.integer(resp) <= 2 ~ -pct,
                            as.integer(resp) == 3 ~ -pct / 2,
                            TRUE ~ pct))

ord_item <- lik %>% filter(as.integer(resp) >= 4) %>%
  group_by(Item) %>% summarise(agree = sum(pct), .groups = "drop") %>%
  arrange(agree)
lik$Item <- factor(lik$Item, levels = ord_item$Item)

b1 <- ggplot(lik, aes(signed, Item, fill = Level)) +
  geom_col(width = 0.76, colour = "white", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = INK, linewidth = 0.6) +
  scale_fill_manual(values = c("Strongly disagree" = "#8C1720",
                               "Disagree" = "#D46A5A",
                               "Neutral" = "#D9D9D9",
                               "Agree" = "#5FA88E",
                               "Strongly agree" = "#12715A")) +
  scale_x_continuous(labels = function(x) paste0(abs(round(x)), "%")) +
  labs(title = "Item-Level Response Profile",
       subtitle = paste0("Diverging Likert chart for ", label_for(blocks[[top_block]], top_block),
                         ". Neutral is split across the centre line"),
       x = "Share of responses", y = NULL, fill = NULL,
       caption = "Items ordered by total agreement") +
  theme_adv() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        axis.text.y = element_text(size = 9))

sv("B1_likert_diverging", b1, 12.5, 0.42 * n_distinct(lik$Item) + 3.6)

# ============================================================================
# B2  EFFECT SIZE FOREST PLOT
# ============================================================================

if (!is.null(TB2) && nrow(TB2)) {
  fp <- TB2 %>%
    mutate(Index = fct_reorder(Index, Cohens_d),
           Sig = ifelse(Significant == "Yes", "FDR p < .05", "Not significant"))
  
  b2 <- ggplot(fp, aes(Cohens_d, Index)) +
    annotate("rect", xmin = -0.2, xmax = 0.2, ymin = -Inf, ymax = Inf,
             fill = "grey85", alpha = 0.45) +
    geom_vline(xintercept = 0, colour = INK, linewidth = 0.55) +
    geom_vline(xintercept = c(-0.5, 0.5), colour = MUTED,
               linetype = "22", linewidth = 0.4) +
    geom_linerange(aes(xmin = d_low, xmax = d_high, colour = Sig),
                   linewidth = 0.9) +
    geom_point(aes(fill = Cohens_d, shape = Sig), size = 4.2,
               colour = "white", stroke = 1) +
    facet_wrap(~Comparison, scales = "free_y") +
    scale_shape_manual(values = c("FDR p < .05" = 21, "Not significant" = 22)) +
    scale_colour_manual(values = c("FDR p < .05" = INK,
                                   "Not significant" = "grey70")) +
    scale_fill_gradient2(low = NEGC, mid = "grey85", high = POSC,
                         midpoint = 0, guide = "none") +
    labs(title = "Effect Size Forest Plot",
         subtitle = "Cohen's d with 95% confidence intervals. Grey band marks the negligible range (|d| < 0.2)",
         x = "Cohen's d", y = NULL, colour = NULL, shape = NULL,
         caption = "Dashed rules at |d| = 0.5 separate small from medium effects. p-values are FDR adjusted") +
    theme_adv() +
    theme(strip.text = element_text(face = "bold", colour = INK),
          panel.grid.major.y = element_blank(),
          legend.position = "bottom")
  
  sv("B2_effect_size_forest", b2, 13, 0.42 * nrow(fp) + 3.4)
}

# ============================================================================
# B3  GROUP MEANS WITH CONFIDENCE INTERVALS ACROSS A FACTOR
# ============================================================================

if (!all(is.na(dat$grp_factor))) {
  gm <- map_dfr(IDX, function(v) {
    dat %>% filter(!is.na(grp_factor), !is.na(.data[[v]])) %>%
      group_by(Group = grp_factor) %>%
      summarise(n = n(), m = mean(.data[[v]]),
                se = sd(.data[[v]]) / sqrt(n()), .groups = "drop") %>%
      filter(n >= 20) %>%
      mutate(Index = LAB[[v]], lo = m - 1.96 * se, hi = m + 1.96 * se)
  })
  
  if (nrow(gm)) {
    grand <- gm %>% group_by(Index) %>% summarise(g = mean(m), .groups = "drop")
    gm <- gm %>% left_join(grand, by = "Index") %>%
      mutate(Index = factor(Index, levels = unname(LAB[IDX])))
    
    b3 <- ggplot(gm, aes(m, Group)) +
      geom_vline(data = grand, aes(xintercept = g),
                 colour = MUTED, linetype = "22", linewidth = 0.45) +
      geom_linerange(aes(xmin = lo, xmax = hi), colour = "grey65",
                     linewidth = 0.8) +
      geom_point(aes(fill = m - g), shape = 21, size = 4,
                 colour = "white", stroke = 0.9) +
      facet_wrap(~Index, scales = "free_x", ncol = 3) +
      scale_fill_gradient2(low = NEGC, mid = "grey88", high = POSC,
                           midpoint = 0, name = "Deviation\nfrom mean") +
      labs(title = "Group Means with 95% Confidence Intervals",
           subtitle = paste0("Segmented by ", str_to_title(str_replace_all(FCT$col, "_", " ")),
                             ". Dashed line is the index grand mean"),
           x = "Mean score (0-100)", y = NULL,
           caption = "Only groups with at least 20 respondents are shown") +
      theme_adv(11) +
      theme(strip.text = element_text(face = "bold", size = 9.5, colour = INK),
            panel.grid.major.y = element_blank(),
            panel.spacing = unit(0.9, "lines"))
    
    sv("B3_group_means_ci", b3, 13, 2.4 * ceiling(length(IDX) / 3) + 2.6)
  }
}

# ============================================================================
# B4  DRIVER REGRESSION COEFFICIENT PLOT
# ============================================================================

if (!is.null(TB4) && nrow(TB4)) {
  cp <- TB4 %>%
    mutate(Label = fct_reorder(Label, Beta),
           Sig = ifelse(Significant == "Yes", "p < .05", "n.s."))
  
  b4 <- ggplot(cp, aes(Beta, Label)) +
    geom_vline(xintercept = 0, colour = INK, linewidth = 0.55) +
    geom_linerange(aes(xmin = CI_low, xmax = CI_high, colour = Sig),
                   linewidth = 1) +
    geom_point(aes(fill = Beta, alpha = Sig), shape = 21, size = 4.6,
               colour = "white", stroke = 1) +
    geom_text(aes(label = sprintf("%+.2f", Beta),
                  x = CI_high + 0.02), hjust = 0, size = 2.9,
              colour = MUTED, fontface = "bold") +
    scale_colour_manual(values = c("p < .05" = INK, "n.s." = "grey72")) +
    scale_alpha_manual(values = c("p < .05" = 1, "n.s." = 0.45), guide = "none") +
    scale_fill_gradient2(low = NEGC, mid = "grey88", high = POSC,
                         midpoint = 0, guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.08, 0.16))) +
    labs(title = paste0("Drivers of ", LAB[[outcome]]),
         subtitle = "Standardised regression coefficients with 95% CI. All variables z-scored, so betas are comparable",
         x = "Standardised beta", y = NULL, colour = NULL,
         caption = paste0("Model R-squared = ",
                          round(summary(fit)$r.squared, 3),
                          " | n = ", length(fit$residuals))) +
    theme_adv() +
    theme(panel.grid.major.y = element_blank(),
          axis.text.y = element_text(face = "bold"),
          legend.position = "bottom")
  
  sv("B4_driver_coefficients", b4, 12, 0.5 * nrow(cp) + 3.4)
  
  diag_df <- tibble(fitted = fitted(fit), resid = resid(fit),
                    std = rstandard(fit))
  qq <- tibble(theo = qqnorm(diag_df$std, plot.it = FALSE)$x,
               samp = qqnorm(diag_df$std, plot.it = FALSE)$y)
  
  b4b <- ggplot(qq, aes(theo, samp)) +
    geom_abline(slope = 1, intercept = 0, colour = LOW,
                linetype = "22", linewidth = 0.6) +
    geom_point(alpha = 0.28, size = 1.5, colour = NEGC) +
    labs(title = "Regression Residual Normality",
         subtitle = "Normal Q-Q plot of standardised residuals. Points on the dashed line indicate normality",
         x = "Theoretical quantiles", y = "Standardised residuals") +
    theme_adv()
  
  sv("B4b_residual_qq", b4b, 8.5, 7)
}

# ============================================================================
# B5  ITEM DIFFICULTY VS DISCRIMINATION
# ============================================================================

if (!is.null(TB1) && nrow(TB1)) {
  b5 <- ggplot(TB1, aes(Mean, Item_Total_r)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0.30,
             fill = LOW, alpha = 0.06) +
    geom_hline(yintercept = 0.30, colour = LOW, linetype = "22",
               linewidth = 0.5) +
    geom_vline(xintercept = 3, colour = MUTED, linetype = "22",
               linewidth = 0.45) +
    geom_point(aes(fill = Scale, size = N), shape = 21,
               colour = "white", stroke = 0.8, alpha = 0.9) +
    geom_text(data = filter(TB1, Item_Total_r < 0.30),
              aes(label = Item), size = 2.5, hjust = -0.09,
              colour = LOW, fontface = "bold") +
    scale_size_continuous(range = c(2.5, 7), guide = "none") +
    scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
    labs(title = "Item Difficulty against Discrimination",
         subtitle = "Horizontal axis is the item mean, vertical axis is the corrected item-total correlation",
         x = "Item mean (1-5)", y = "Item-total correlation", fill = "Scale",
         caption = "Shaded zone marks items below the conventional r = 0.30 threshold; these weaken their scale") +
    theme_adv() +
    theme(legend.position = "bottom")
  
  sv("B5_item_difficulty_discrimination", b5, 12, 8.5)
}

# ============================================================================
# B6  DEMOGRAPHIC CROSSTAB HEATMAP
# ============================================================================

if (!is.na(sector_col) && !is.na(gender_col)) {
  ct <- map_dfr(IDX, function(v) {
    dat %>% filter(!is.na(.data[[sector_col]]), !is.na(.data[[gender_col]]),
                   !is.na(.data[[v]])) %>%
      group_by(Sector = .data[[sector_col]], Gender = .data[[gender_col]]) %>%
      summarise(m = mean(.data[[v]]), n = n(), .groups = "drop") %>%
      filter(n >= 15) %>%
      mutate(Index = LAB[[v]], Cell = paste0(Sector, "\n", Gender))
  })
  
  if (nrow(ct)) {
    ct <- ct %>% group_by(Index) %>%
      mutate(z = (m - mean(m)) / ifelse(sd(m) == 0, 1, sd(m))) %>%
      ungroup() %>%
      mutate(Index = factor(Index, levels = rev(unname(LAB[IDX]))))
    
    b6 <- ggplot(ct, aes(Cell, Index, fill = z)) +
      geom_tile(colour = "white", linewidth = 1) +
      geom_text(aes(label = sprintf("%.1f", m), colour = abs(z) > 1.1),
                size = 3.1, fontface = "bold") +
      scale_colour_manual(values = c("grey20", "white"), guide = "none") +
      scale_fill_gradient2(low = NEGC, mid = "#F7F8FA", high = POSC,
                           midpoint = 0, name = "Within-index\nz-score") +
      labs(title = "Index Means by Sector and Gender",
           subtitle = "Cell labels are raw means; colour is standardised within each index so rows are comparable",
           x = NULL, y = NULL,
           caption = "Cells with fewer than 15 respondents are omitted") +
      theme_adv() +
      theme(axis.text.x = element_text(face = "bold", size = 9, lineheight = 1.1),
            axis.text.y = element_text(face = "bold", size = 9),
            panel.grid = element_blank())
    
    sv("B6_crosstab_heatmap", b6, 10, 0.52 * length(IDX) + 3.4)
  }
}

# ============================================================================
# B7  DENSITY SHIFT COMPARISON
# ============================================================================

if (!is.na(sector_col)) {
  ds <- map_dfr(IDX, function(v) {
    dat %>% filter(!is.na(.data[[sector_col]]), !is.na(.data[[v]])) %>%
      transmute(Index = LAB[[v]], Group = .data[[sector_col]], Score = .data[[v]])
  })
  
  if (nrow(ds)) {
    ds_mu <- ds %>% group_by(Index, Group) %>%
      summarise(m = mean(Score), .groups = "drop")
    ds$Index <- factor(ds$Index, levels = unname(LAB[IDX]))
    ds_mu$Index <- factor(ds_mu$Index, levels = unname(LAB[IDX]))
    
    b7 <- ggplot(ds, aes(Score, fill = Group, colour = Group)) +
      geom_density(alpha = 0.32, linewidth = 0.7, adjust = 1.15) +
      geom_vline(data = ds_mu, aes(xintercept = m, colour = Group),
                 linetype = "22", linewidth = 0.55) +
      facet_wrap(~Index, scales = "free_y", ncol = 3) +
      scale_fill_manual(values = c(Public = "#2C5F8A", Private = "#D9772E")) +
      scale_colour_manual(values = c(Public = "#2C5F8A", Private = "#D9772E")) +
      scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
      labs(title = "Distribution Shift by Sector",
           subtitle = "Overlaid kernel densities with group means marked by dashed rules",
           x = "Score (0-100)", y = "Density", fill = NULL, colour = NULL) +
      theme_adv(11) +
      theme(strip.text = element_text(face = "bold", size = 9.5, colour = INK),
            axis.text.y = element_blank(),
            legend.position = "bottom",
            panel.spacing = unit(0.9, "lines"))
    
    sv("B7_density_shift_sector", b7, 12.5, 2.5 * ceiling(length(IDX) / 3) + 2.6)
    
    b8 <- ggplot(ds, aes(Score, colour = Group)) +
      stat_ecdf(linewidth = 0.85, geom = "step") +
      facet_wrap(~Index, ncol = 3) +
      scale_colour_manual(values = c(Public = "#2C5F8A", Private = "#D9772E")) +
      scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(title = "Cumulative Distribution by Sector",
           subtitle = "Vertical distance between curves shows the share of students separating the two groups at any score",
           x = "Score (0-100)", y = "Cumulative share", colour = NULL) +
      theme_adv(11) +
      theme(strip.text = element_text(face = "bold", size = 9.5, colour = INK),
            legend.position = "bottom",
            panel.spacing = unit(0.9, "lines"))
    
    sv("B8_ecdf_by_sector", b8, 12.5, 2.5 * ceiling(length(IDX) / 3) + 2.6)
  }
}

# ============================================================================
# B9  BOOTSTRAP PRECISION PLOT
# ============================================================================

bp <- TB6 %>% mutate(Index = fct_reorder(Index, Mean))

b9 <- ggplot(bp, aes(Mean, Index)) +
  geom_linerange(aes(xmin = Boot_CI_low, xmax = Boot_CI_high),
                 colour = "grey62", linewidth = 3.4, alpha = 0.55) +
  geom_point(aes(fill = Mean), shape = 21, size = 5.4,
             colour = "white", stroke = 1.1) +
  geom_text(aes(label = sprintf("%.1f", Mean)), colour = "white",
            size = 2.5, fontface = "bold") +
  geom_text(aes(x = Boot_CI_high + 1.2,
                label = sprintf("+/- %.1f", CI_width / 2)),
            hjust = 0, size = 2.9, colour = MUTED) +
  scale_fill_gradient2(low = LOW, mid = MID, high = HIGH, midpoint = 55,
                       limits = c(0, 100), guide = "none") +
  scale_x_continuous(limits = c(0, 104), breaks = seq(0, 100, 20),
                     expand = c(0, 0)) +
  labs(title = "Bootstrap Estimates of Index Means",
       subtitle = "2,000 resamples per index. Bars are percentile 95% confidence intervals",
       x = "Mean score (0-100)", y = NULL,
       caption = "Right-hand labels give the half-width of each interval, a direct read on estimate precision") +
  theme_adv() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(face = "bold"))

sv("B9_bootstrap_precision", b9, 12, 0.58 * nrow(bp) + 3.4)

# ============================================================================
# B10  VARIANCE DECOMPOSITION
# ============================================================================

vc <- map_dfr(IDX, function(v) {
  x <- dat[[v]]
  parts <- list()
  if (!is.na(sector_col)) parts$Sector <- dat[[sector_col]]
  if (!is.na(gender_col)) parts$Gender <- dat[[gender_col]]
  if (!all(is.na(dat$grp_factor))) parts[[str_to_title(FCT$col)]] <- dat$grp_factor
  if (!length(parts)) return(NULL)
  map_dfr(names(parts), function(nm) {
    g <- parts[[nm]]
    ok <- !is.na(x) & !is.na(g)
    if (sum(ok) < 60 || n_distinct(g[ok]) < 2) return(NULL)
    f <- aov(x[ok] ~ factor(g[ok]))
    s <- summary(f)[[1]]
    tibble(Index = LAB[[v]], Source = nm,
           Eta_sq = s[["Sum Sq"]][1] / sum(s[["Sum Sq"]]))
  })
})

if (nrow(vc)) {
  write.csv(vc %>% mutate(Eta_sq = round(Eta_sq, 4)),
            "TB8_variance_explained.csv", row.names = FALSE)
  
  vc <- vc %>% mutate(Index = factor(Index, levels = rev(unname(LAB[IDX]))))
  
  b10 <- ggplot(vc, aes(Eta_sq * 100, Index, fill = Source)) +
    geom_col(position = position_dodge(width = 0.78), width = 0.7,
             colour = "white", linewidth = 0.3) +
    scale_fill_manual(values = c("#2C5F8A", "#D9772E", "#12715A", "#8C1720")) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.06)),
                       labels = function(x) paste0(x, "%")) +
    labs(title = "Variance Explained by Demographic Factors",
         subtitle = "Eta-squared from one-way ANOVA: the share of each index's variance attributable to each factor",
         x = "Variance explained", y = NULL, fill = NULL,
         caption = "Low values indicate that within-group differences dominate between-group differences") +
    theme_adv() +
    theme(panel.grid.major.y = element_blank(),
          axis.text.y = element_text(face = "bold"),
          legend.position = "bottom")
  
  sv("B10_variance_explained", b10, 12, 0.62 * length(IDX) + 3.4)
}

TB6