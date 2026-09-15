# 17_panel_regressions.R
#
# Runs the Section-5 fixed-effects panel regressions (14 specs x 3
# subsamples), with clustered and Driscoll-Kraay standard errors, plus a
# Wald test for a pre/post-2009Q2 structural break and a combined
# pre/shift/post table via a hand-built delta-method computation. Produces
# Table 5 and Table C.1.

source(here::here("R", "00_config.R"))

suppressMessages({
  library(tidyverse)
  library(plm)
  library(tseries)
  library(gplots)
  library(lmtest)
  library(purrr)
  library(tibble)
  library(car)
})

out_dir <- path$output$tables

spec_formulas <- list(
  # Table 5: Dependent var: stock market return
  "return ~ time_idx + VIX_t + delta_VIX + credit_l:VIX_t + credit_l_lag:VIX_lag + GDPg_lag + NEER_lag",
  "return ~ time_idx + VIX_t + delta_VIX + credit_nb_l:VIX_t + credit_nb_l_lag:VIX_lag + GDPg_lag + NEER_lag",
  "return ~ time_idx + VIX_t + delta_VIX + credit_a:VIX_t + credit_a_lag:VIX_lag + GDPg_lag + NEER_lag",
  "return ~ time_idx + VIX_t + delta_VIX + debt_l:VIX_t + debt_l_lag:VIX_lag + GDPg_lag + NEER_lag",
  "return ~ time_idx + VIX_t + delta_VIX + debt_a:VIX_t + debt_a_lag:VIX_lag + GDPg_lag + NEER_lag",
  "return ~ time_idx + VIX_t + delta_VIX + equity_l:VIX_t + equity_l_lag:VIX_lag + GDPg_lag + NEER_lag",
  "return ~ time_idx + VIX_t + delta_VIX + equity_a:VIX_t + equity_a_lag:VIX_lag + GDPg_lag + NEER_lag",
  # Table 6: Dependent var: banking sector leverage growth
  "lev_growth ~ time_idx + VIX_t + delta_VIX + credit_l:VIX_t + credit_l_lag:VIX_lag + GDPg_lag + NEER_lag",
  "lev_growth ~ time_idx + VIX_t + delta_VIX + credit_nb_l:VIX_t + credit_nb_l_lag:VIX_lag + GDPg_lag + NEER_lag",
  "lev_growth ~ time_idx + VIX_t + delta_VIX + credit_a:VIX_t + credit_a_lag:VIX_lag + GDPg_lag + NEER_lag",
  "lev_growth ~ time_idx + VIX_t + delta_VIX + debt_l:VIX_t + debt_l_lag:VIX_lag + GDPg_lag + NEER_lag",
  "lev_growth ~ time_idx + VIX_t + delta_VIX + debt_a:VIX_t + debt_a_lag:VIX_lag + GDPg_lag + NEER_lag",
  "lev_growth ~ time_idx + VIX_t + delta_VIX + equity_l:VIX_t + equity_l_lag:VIX_lag + GDPg_lag + NEER_lag",
  "lev_growth ~ time_idx + VIX_t + delta_VIX + equity_a:VIX_t + equity_a_lag:VIX_lag + GDPg_lag + NEER_lag"
)

subsamples <- list(
  full    = function(df) df,
  preGFC  = function(df) subset(df, quarter <= "2009 Q2"),
  postGFC = function(df) subset(df, quarter >= "2009 Q3")
)

run_spec <- function(formula_str, data) {
  f <- as.formula(formula_str)
  pdata <- pdata.frame(data, index = c("country", "quarter"))
  fe_mod <- plm(f, data = pdata, model = "within", effect = "individual")

  N_obs <- length(residuals(fe_mod))
  adj_r2 <- unname(summary(fe_mod)$r.squared[2])

  cl_vcov <- vcovHC(fe_mod, method = "arellano", type = "HC0", cluster = "group")
  dk_vcov <- vcovSCC(fe_mod, type = "HC0")

  clust_coeft <- coeftest(fe_mod, vcov. = cl_vcov)
  dk_coeft <- coeftest(fe_mod, vcov. = dk_vcov)

  bp <- tryCatch(bptest(fe_mod, studentize = FALSE), error = function(e) NULL)
  cd <- tryCatch(pcdtest(fe_mod, test = "cd"), error = function(e) NULL)
  bg <- tryCatch(pbgtest(fe_mod), error = function(e) NULL)

  list(
    model = fe_mod, N = N_obs, adj_R2 = adj_r2,
    cluster_SE = clust_coeft, driscoll_kraay = dk_coeft,
    bp = bp, cd = cd, bg = bg, countries = unique(index(fe_mod)$country)
  )
}

Panel <- read_csv(file.path(path$build$panel, "panel_regression_dataset.csv"), show_col_types = FALSE) %>%
  group_by(country) %>%
  arrange(quarter) %>%
  mutate(time_idx = seq(1, 140, 1)) %>%
  ungroup() %>%
  relocate(time_idx) %>%
  mutate(post = if_else(quarter >= "2009 Q3", 1L, 0L))

results <- list()
for (sub_name in names(subsamples)) {
  df_sub <- subsamples[[sub_name]](Panel)
  results[[sub_name]] <- list()
  for (i in seq_along(spec_formulas)) {
    spec_name <- paste0("spec", i)
    cat("Running", sub_name, spec_name, "...\n")
    results[[sub_name]][[spec_name]] <- run_spec(spec_formulas[[i]], data = df_sub)
  }
}

extract_coefs <- function(ct) {
  m <- as.matrix(ct)
  tibble(term = rownames(m), estimate = m[, 1], std.error = m[, 2], t.value = m[, 3], p.value = m[, 4])
}

all_coefs <- imap_dfr(results, ~ {
  subs <- .y
  map_dfr(.x, ~ {
    res <- .x
    dk <- extract_coefs(res$driscoll_kraay) %>% mutate(se_type = "DK")
    cl <- extract_coefs(res$cluster_SE) %>% mutate(se_type = "CL")
    bind_rows(dk, cl) %>%
      mutate(
        subsample = subs, N = res$N, adj_R2 = res$adj_R2,
        bp.p = ifelse(is.null(res$bp), NA_real_, res$bp$p.value),
        cd.p = ifelse(is.null(res$cd), NA_real_, res$cd$p.value),
        bg.p = ifelse(is.null(res$bg), NA_real_, res$bg$p.value)
      )
  }, .id = "spec")
}, .id = "subsample")

all_coefs <- all_coefs %>%
  mutate(spec_num = as.integer(sub("spec", "", spec)), block = if_else(spec_num <= 7, "1-7", "8-14"))

format_table <- function(df) {
  df %>%
    filter(!(term %in% c("time_idx", "GDPg_lag", "NEER_lag"))) %>%
    mutate(
      across(c(estimate, std.error, t.value), ~ formatC(., format = "f", digits = 3)),
      adj_R2 = format(adj_R2, format = "f", digits = 2),
      stars = case_when(p.value < 0.01 ~ 3, p.value < 0.05 ~ 2, p.value < 0.10 ~ 1, TRUE ~ 0)
    ) %>%
    relocate(stars, .before = se_type)
}

# Table C.1: restricted panel regression, one CL/DK pair per subsample
# (Panel A = full sample, Panel B = pre-GFC, Panel C = post-GFC)
panel_letter <- c(full = "A", preGFC = "B", postGFC = "C")
panel_label <- c(full = "full_sample", preGFC = "preGFC", postGFC = "postGFC")

for (sub_name in names(subsamples)) {
  table_cl <- all_coefs %>%
    filter(block == "1-7", se_type == "CL", subsample == sub_name) %>%
    format_table()
  table_dk <- all_coefs %>%
    filter(block == "1-7", se_type == "DK", subsample == sub_name) %>%
    format_table()
  prefix <- sprintf("tableC1_panel%s_%s", panel_letter[[sub_name]], panel_label[[sub_name]])
  write.csv(table_cl, file.path(out_dir, paste0(prefix, "_cluster_se.csv")), row.names = FALSE)
  write.csv(table_dk, file.path(out_dir, paste0(prefix, "_driscoll_kraay_se.csv")), row.names = FALSE)
}

# ------------------------------------------------------------------------------
# Wald break test (pooled FE vs interacted FE allowing slope shifts post-2009Q2)
# ------------------------------------------------------------------------------

build_interaction_formula <- function(formula_str) {
  f <- as.formula(formula_str)
  rhs_terms <- attr(terms(f), "term.labels")
  rhs <- paste(rhs_terms, collapse = " + ")
  inter <- paste(sprintf("post:(%s)", rhs_terms), collapse = " + ")
  as.formula(paste(all.vars(f)[1], "~", paste(c(rhs, inter), collapse = " + ")))
}

df_full <- subsamples$full(Panel)
pdata_full <- pdata.frame(df_full, index = c("country", "quarter"))

wald_break <- map_dfr(seq_along(spec_formulas), function(i) {
  spec_str <- spec_formulas[[i]]
  base_mod <- plm(as.formula(spec_str), data = pdata_full, model = "within", effect = "individual")
  int_form <- build_interaction_formula(spec_str)
  int_mod <- plm(int_form, data = pdata_full, model = "within", effect = "individual")

  wt_cl <- lmtest::waldtest(base_mod, int_mod, test = "F", vcov = function(x) vcovHC(x, method = "arellano", type = "HC0", cluster = "group"))
  wt_dk <- lmtest::waldtest(base_mod, int_mod, test = "F", vcov = function(x) vcovSCC(x, type = "HC0"))

  tibble(
    spec = paste0("spec", i),
    wald_F_CL = as.numeric(wt_cl[2, "F"]), wald_df1_CL = as.integer(wt_cl[2, "Df"]), wald_df2_CL = as.integer(wt_cl[2, "Res.Df"]), wald_p_CL = as.numeric(wt_cl[2, "Pr(>F)"]),
    wald_F_DK = as.numeric(wt_dk[2, "F"]), wald_df1_DK = as.integer(wt_dk[2, "Df"]), wald_df2_DK = as.integer(wt_dk[2, "Res.Df"]), wald_p_DK = as.numeric(wt_dk[2, "Pr(>F)"])
  )
})

write.csv(wald_break, file.path(out_dir, "table05_wald_structural_break_test.csv"), row.names = FALSE)

# ------------------------------------------------------------------------------
# Unrestricted interacted model: pre, shift, implied post (SE, t only)
# ------------------------------------------------------------------------------

canon_name <- function(x) paste(sort(strsplit(x, ":", fixed = TRUE)[[1]]), collapse = ":")

stars_fun <- function(p) {
  dplyr::case_when(is.na(p) ~ "", p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
}

fmt3 <- function(x) ifelse(is.na(x), "", formatC(x, format = "f", digits = 3))

extract_pre_post <- function(mod, vcov_fun, drop_terms = c("time_idx", "GDPg_lag", "NEER_lag")) {
  coefs <- coef(mod)
  coef_names <- names(coefs)
  vcovM <- vcov_fun(mod)
  dfres <- df.residual(mod)

  canon_map <- setNames(coef_names, vapply(coef_names, canon_name, FUN.VALUE = character(1)))
  base_terms <- coef_names[!grepl("(^|:)post(:|$)", coef_names)]
  base_terms <- base_terms[!(base_terms %in% drop_terms)]

  rows <- lapply(base_terms, function(base_name) {
    beta <- unname(coefs[base_name])
    se_b <- sqrt(vcovM[base_name, base_name])
    t_b <- beta / se_b
    p_b <- 2 * pt(abs(t_b), df = dfres, lower.tail = FALSE)

    shift_canon <- canon_name(paste("post", base_name, sep = ":"))
    shift_name <- unname(canon_map[shift_canon])

    if (!is.na(shift_name) && nzchar(shift_name)) {
      gamma <- unname(coefs[shift_name])
      se_g <- sqrt(vcovM[shift_name, shift_name])
      t_g <- gamma / se_g
      p_g <- 2 * pt(abs(t_g), df = dfres, lower.tail = FALSE)

      L <- rep(0, length(coefs))
      names(L) <- coef_names
      L[base_name] <- 1
      L[shift_name] <- 1
      post <- sum(L * coefs)
      se_po <- as.numeric(sqrt(t(L) %*% vcovM %*% L))
      t_po <- post / se_po
      p_po <- 2 * pt(abs(t_po), df = dfres, lower.tail = FALSE)
    } else {
      gamma <- t_g <- p_g <- NA_real_
      post <- t_po <- p_po <- NA_real_
    }

    tibble::tibble(
      term = base_name,
      pre_est = fmt3(beta), pre_t = fmt3(t_b), pre_star = stars_fun(p_b),
      shift_est = fmt3(gamma), shift_t = fmt3(t_g), shift_star = stars_fun(p_g),
      post_est = fmt3(post), post_t = fmt3(t_po), post_star = stars_fun(p_po)
    )
  })

  dplyr::bind_rows(rows)
}

pre_post_tables <- purrr::imap_dfr(spec_formulas, function(spec_str, i) {
  spec_id <- paste0("spec", i)
  int_form <- build_interaction_formula(spec_str)
  int_mod <- plm(int_form, data = pdata_full, model = "within", effect = "individual")

  tab <- extract_pre_post(int_mod, vcov_fun = function(x) vcovHC(x, method = "arellano", type = "HC0", cluster = "group"), drop_terms = c("time_idx", "GDPg_lag", "NEER_lag"))

  header <- tibble::tibble(spec = spec_id, N = nobs(int_mod), adj_R2 = as.numeric(summary(int_mod)$r.squared["adjrsq"]))

  tab %>%
    dplyr::mutate(spec = spec_id) %>%
    dplyr::left_join(header, by = "spec") %>%
    dplyr::left_join(wald_break %>% dplyr::select(spec, wald_p_CL, wald_p_DK), by = "spec") %>%
    dplyr::select(spec, N, adj_R2, wald_p_CL, wald_p_DK, term, pre_est, pre_t, pre_star, shift_est, shift_t, shift_star, post_est, post_t, post_star)
})

write.csv(pre_post_tables, file.path(out_dir, "table05_unrestricted_panel_regression.csv"), row.names = FALSE)
