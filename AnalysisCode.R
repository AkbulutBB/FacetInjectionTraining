###############################################################
# Libraries
###############################################################
library(ggplot2)
library(viridis)
library(gridExtra)
library(scales)
library(grid)
library(nparLD)
library(stats)
library(reshape2)

###############################################################
# STEP 1: Read & prepare data
###############################################################
df <- read.csv("C:/Users/bbaha/OneDrive/Documents/facet_data.csv",
               header = TRUE,
               stringsAsFactors = FALSE)

# Coerce factors we actually need
df$case_number <- factor(df$case_number, levels = sort(unique(df$case_number)))
df$surgeon_id  <- factor(df$surgeon_id)

# Defensive: ensure outcomes are finite
clean_outcome <- function(x) { x[!is.finite(x)] <- NA; x }
df$converted_surgery_time <- clean_outcome(df$converted_surgery_time)
df$FloroTotal             <- clean_outcome(df$FloroTotal)
df$totalTryNum            <- clean_outcome(df$totalTryNum)
df$totalComplication      <- clean_outcome(df$totalComplication)

# Helper: drop NA rows FOR A GIVEN OUTCOME (nparLD can handle some missing,
# but we'll give each fit the cleanest slice)
drop_na_for <- function(dat, y) dat[is.finite(dat[[y]]) & !is.na(dat[[y]]), , drop = FALSE]

time_ord <- levels(df$case_number)

###############################################################
# STEP 2: LD-F1 models (within-subject time only)
###############################################################
fit_ldf1 <- function(dat, yvar) {
  dd <- drop_na_for(dat, yvar)
  ld.f1(
    y          = dd[[yvar]],
    time       = dd$case_number,
    subject    = dd$surgeon_id,
    description= TRUE,
    time.order = time_ord
  )
}

fit_surg_time <- fit_ldf1(df, "converted_surgery_time")
fit_floro     <- fit_ldf1(df, "FloroTotal")
fit_try       <- fit_ldf1(df, "totalTryNum")
fit_comp      <- fit_ldf1(df, "totalComplication")

###############################################################
# HELPERS: model-safe extractors & plot saving
###############################################################

# Extract ANOVA/Wald tests regardless of ld.f1 vs f1.ld.f1
extract_tests <- function(fit) {
  # Try time-specific slots first (f1.ld.f1), then generic (ld.f1)
  anova_obj <- if (!is.null(fit$ANOVA.test.time)) fit$ANOVA.test.time else fit$ANOVA.test
  wald_obj  <- if (!is.null(fit$Wald.test.time))  fit$Wald.test.time  else fit$Wald.test
  list(anova = anova_obj, wald = wald_obj)
}

# Extract RTE robustly
extract_rte <- function(fit) {
  r <- as.data.frame(fit$RTE)
  # Prefer a column literally named RTE, else first numeric column
  if ("RTE" %in% names(r)) return(r)
  num_cols <- vapply(r, is.numeric, logical(1))
  if (any(num_cols)) {
    r$RTE <- r[[which(num_cols)[1]]]
    return(r)
  }
  stop("Could not find RTE numeric column.")
}

# Format a single model’s results into text
format_model_results <- function(fit, title) {
  tests <- extract_tests(fit)
  rte   <- extract_rte(fit)
  
  paste0(
    "\n", title, "\n",
    "========================================\n",
    "ANOVA-Type Test:\n",
    paste(capture.output(print(tests$anova)), collapse = "\n"),
    "\nWald-Type Test:\n",
    paste(capture.output(print(tests$wald)), collapse = "\n"),
    "\nRelative Treatment Effects (RTE):\n",
    paste(capture.output(print(rte)), collapse = "\n"),
    "\n----------------------------------------\n"
  )
}

# Save each plot as both PDF and PNG, and also show in Plots pane
save_both <- function(plot, base, width = 10, height = 7, dpi = 300) {
  # Show in Plots pane
  print(plot)
  # Save files
  ggsave(paste0(base, ".pdf"), plot, width = width, height = height, limitsize = FALSE)
  ggsave(paste0(base, ".png"), plot, width = width, height = height, dpi = dpi, limitsize = FALSE)
}

###############################################################
# STEP 3: Format results to a text file (prettified)
###############################################################

# Pretty p-value formatter
pretty_p <- function(p) {
  ifelse(is.na(p), NA,
         ifelse(p < 0.001, "<0.001",
                sprintf("%.3f", p)))
}

# Extract ANOVA/Wald tests safely
extract_tests <- function(fit) {
  anova_obj <- if (!is.null(fit$ANOVA.test.time)) fit$ANOVA.test.time else fit$ANOVA.test
  wald_obj  <- if (!is.null(fit$Wald.test.time))  fit$Wald.test.time  else fit$Wald.test
  list(anova = anova_obj, wald = wald_obj)
}

# Extract RTE robustly
extract_rte <- function(fit) {
  r <- as.data.frame(fit$RTE)
  if ("RTE" %in% names(r)) return(r)
  num_cols <- vapply(r, is.numeric, logical(1))
  if (any(num_cols)) {
    r$RTE <- r[[which(num_cols)[1]]]
    return(r)
  }
  stop("Could not find RTE numeric column.")
}

# Format results neatly
format_model_results <- function(fit, title) {
  tests <- extract_tests(fit)
  rte   <- extract_rte(fit)
  
  # Convert to data.frames
  anova_tbl <- as.data.frame(tests$anova)
  wald_tbl  <- as.data.frame(tests$wald)
  
  # Round nicely
  for (nm in c("Statistic","ATS","WTS","df")) {
    if (nm %in% names(anova_tbl)) anova_tbl[[nm]] <- round(anova_tbl[[nm]], 3)
    if (nm %in% names(wald_tbl))  wald_tbl[[nm]]  <- round(wald_tbl[[nm]], 3)
  }
  
  # Pretty p-values
  if ("p-value" %in% names(anova_tbl)) {
    anova_tbl[["p-value"]] <- sapply(anova_tbl[["p-value"]], pretty_p)
  }
  if ("p-value" %in% names(wald_tbl)) {
    wald_tbl[["p-value"]] <- sapply(wald_tbl[["p-value"]], pretty_p)
  }
  
  paste0(
    "\n", title, "\n",
    "========================================\n",
    "ANOVA-Type Test:\n",
    paste(capture.output(print(anova_tbl, row.names = FALSE)), collapse = "\n"),
    "\nWald-Type Test:\n",
    paste(capture.output(print(wald_tbl, row.names = FALSE)), collapse = "\n"),
    "\nRelative Treatment Effects (RTE):\n",
    paste(capture.output(print(rte)), collapse = "\n"),
    "\n----------------------------------------\n"
  )
}

# Master results text
results_text <- paste0(
  "Facet Injection Model - LD-F1 Statistical Results (time only)\n",
  "Date of Analysis: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
  "========================================\n",
  format_model_results(fit_surg_time, "Converted Surgery Time"),
  format_model_results(fit_floro,     "Fluoroscopy Shots"),
  format_model_results(fit_try,       "Total Try Attempts"),
  format_model_results(fit_comp,      "Total Complications")
)

###############################################################
# STEP 3.5: Descriptive summaries per session + append to report
###############################################################

# Helpers
q25 <- function(x) stats::quantile(x, 0.25, na.rm = TRUE)
q75 <- function(x) stats::quantile(x, 0.75, na.rm = TRUE)

summarize_by_session <- function(dat, yvar) {
  stopifnot(all(c("case_number", yvar) %in% names(dat)))
  dd <- dat[is.finite(dat[[yvar]]) & !is.na(dat[[yvar]]),
            c("case_number", yvar)]
  names(dd)[2] <- "y"
  if (nrow(dd) == 0L) {
    return(data.frame(
      case_number = factor(character(), levels = levels(dat$case_number)),
      N=integer(), Mean=numeric(), SD=numeric(), Median=numeric(),
      Q1=numeric(), Q3=numeric(), Min=numeric(), Max=numeric()
    ))
  }
  
  q25 <- function(x) stats::quantile(x, 0.25, na.rm = TRUE)
  q75 <- function(x) stats::quantile(x, 0.75, na.rm = TRUE)
  
  agg <- aggregate(y ~ case_number, data = dd, FUN = function(v) c(
    N      = sum(is.finite(v)),
    Mean   = mean(v, na.rm = TRUE),
    SD     = stats::sd(v, na.rm = TRUE),
    Median = stats::median(v, na.rm = TRUE),
    Q1     = q25(v),
    Q3     = q75(v),
    Min    = min(v, na.rm = TRUE),
    Max    = max(v, na.rm = TRUE)
  ))
  
  # expand the matrix-column safely
  stats_df <- as.data.frame(agg$y, stringsAsFactors = FALSE)
  out <- cbind(case_number = agg$case_number, stats_df, row.names = NULL)
  
  # round for display
  num_cols <- setdiff(names(out), "case_number")
  out[num_cols] <- lapply(out[num_cols], function(z) if (is.numeric(z)) round(z, 3) else z)
  out
}


# Build per-session tables
sum_time  <- summarize_by_session(df, "converted_surgery_time")
sum_floro <- summarize_by_session(df, "FloroTotal")
sum_try   <- summarize_by_session(df, "totalTryNum")
sum_comp  <- summarize_by_session(df, "totalComplication")

# Save CSVs (machine-readable)
utils::write.csv(sum_time,  "facet_summary_converted_surgery_time.csv", row.names = FALSE)
utils::write.csv(sum_floro, "facet_summary_fluoroscopy_shots.csv",      row.names = FALSE)
utils::write.csv(sum_try,   "facet_summary_total_attempts.csv",         row.names = FALSE)
utils::write.csv(sum_comp,  "facet_summary_total_complications.csv",    row.names = FALSE)

# Human-readable block for the text report
fmt_row <- function(r1) {
  paste0(
    "Session ", as.character(r1$case_number), ": ",
    "N=", r1$N, "; ",
    "Mean±SD=", r1$Mean, "±", r1$SD, "; ",
    "Median [Q1–Q3]=", r1$Median, " [", r1$Q1, "–", r1$Q3, "]; ",
    "Range=", r1$Min, "–", r1$Max
  )
}

summ_block <- function(title, tab) {
  if (nrow(tab) == 0L) return(paste0("\n", title, "\n(no data)\n"))
  lines <- vapply(seq_len(nrow(tab)), function(i) fmt_row(tab[i, , drop = FALSE]),
                  FUN.VALUE = character(1))
  paste0(
    "\n", title, "\n",
    "----------------------------------------\n",
    paste(lines, collapse = "\n"),
    "\n"
  )
}

descriptive_text <- paste0(
  "\n\nDescriptive Summaries by Session\n",
  "========================================\n",
  summ_block("Converted Surgery Time (minutes)", sum_time),
  summ_block("Fluoroscopy Shots (count)",        sum_floro),
  summ_block("Total Attempts (count)",           sum_try),
  summ_block("Total Complications (count)",      sum_comp)
)

# Append to your existing results_text and write out
full_report <- paste0(results_text, descriptive_text, "\n")
writeLines(full_report, "facet_statistical_results_LD-F1.txt")
cat(full_report)


###############################################################
# STEP 4: Visualization (no PGY; time-only) – unchanged logic ok
###############################################################
theme_publication <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey70", fill = NA),
      axis.title = element_text(size = rel(1.1)),
      axis.text = element_text(size = rel(0.9)),
      plot.title = element_text(size = rel(1.2), face = "bold"),
      legend.title = element_text(size = rel(0.9)),
      legend.text = element_text(size = rel(0.8)),
      legend.position = "bottom"
    )
}

set.seed(2024)

pbx <- function(y, ylab, ttl) {
  ggplot(df, aes(x = case_number, y = .data[[y]])) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_jitter(aes(color = surgeon_id), width = 0.2, height = 0, alpha = 0.6) +
    scale_color_viridis_d() +
    labs(title = ttl, x = "Session", y = ylab, color = "Surgeon") +
    theme_publication()
}

p1 <- pbx("converted_surgery_time", "Surgery Time (minutes)",
          "Converted Surgery Time Across Sessions")
p2 <- pbx("FloroTotal", "Number of Shots",
          "Fluoroscopy Shots Across Sessions")
p3 <- pbx("totalTryNum", "Number of Attempts",
          "Total Attempts Across Sessions")
p4 <- pbx("totalComplication", "Complications",
          "Total Complications Across Sessions")

pline <- function(y, ylab, ttl) {
  ggplot(df, aes(x = case_number, y = .data[[y]], group = surgeon_id)) +
    geom_line(aes(color = surgeon_id), alpha = 0.5) +
    geom_point(aes(color = surgeon_id), size = 2) +
    scale_color_viridis_d() +
    labs(title = ttl, x = "Session", y = ylab, color = "Surgeon") +
    theme_publication()
}
p5 <- pline("converted_surgery_time", "Surgery Time (minutes)",
            "Individual Learning Trajectories")

# RTE plotting for ld.f1 (time-only)
make_rte_df_ldf1 <- function(fit, outcome, time_ord = levels(df$case_number)) {
  r <- extract_rte(fit)
  # Try to align rows to time levels if they look like "Time1/Time2/Time3"
  if (!is.null(rownames(r))) {
    # strip non-digits, coerce to order if possible
    idx <- suppressWarnings(as.integer(gsub("\\D", "", rownames(r))))
    ord <- order(ifelse(is.na(idx), seq_len(nrow(r)), idx))
    r <- r[ord, , drop = FALSE]
  }
  data.frame(
    case_number = factor(time_ord, levels = time_ord),
    RTE = as.numeric(r$RTE),
    outcome = outcome
  )
}

plot_rte_time <- function(data, title) {
  ggplot(data, aes(x = case_number, y = RTE, group = 1)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    labs(title = paste("Relative Treatment Effect by Session -", title),
         x = "Session", y = "RTE") +
    theme_publication()
}

rte_surg <- make_rte_df_ldf1(fit_surg_time, "Surgery Time")
rte_floro <- make_rte_df_ldf1(fit_floro, "Fluoroscopy Shots")
rte_try   <- make_rte_df_ldf1(fit_try, "Attempts")
rte_comp  <- make_rte_df_ldf1(fit_comp, "Complications")

p6 <- plot_rte_time(rte_surg, "Surgery Time")
p7 <- plot_rte_time(rte_floro, "Fluoroscopy Shots")
p8 <- plot_rte_time(rte_try,   "Attempts")
p9 <- plot_rte_time(rte_comp,  "Complications")

###############################################################
# STEP 5: Save plots (PDF + PNG) AND show in Plots pane
###############################################################
save_both(p1, "facet_surgery_time_LDF1")
save_both(p2, "facet_floro_shots_LDF1")
save_both(p3, "facet_attempts_LDF1")
save_both(p4, "facet_complications_LDF1")
save_both(p5, "facet_learning_trajectories_LDF1")

save_both(p6, "facet_rte_surgery_time_LDF1")
save_both(p7, "facet_rte_floro_shots_LDF1")
save_both(p8, "facet_rte_attempts_LDF1")
save_both(p9, "facet_rte_complications_LDF1")

# Done!
