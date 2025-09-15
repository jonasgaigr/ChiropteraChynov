# Read processed data ----
chynov <- readr::read_csv2(
  "Data/Processed/chynov.csv"
)

chynov_selected <- readr::read_csv2(
  "Data/Processed/chynov_selected.csv"
)

## Data preparaion----
chynov_sum <-
  chynov %>%
  dplyr::group_by(
    druh, 
    sezona
  ) %>%
  dplyr::reframe(
    sezona = as.character(sezona),
    pocet = max(pocet, na.rm = TRUE)
  ) %>%
  dplyr::distinct()

# Analysis----
## ---- Including month data ----
# assume `chynov` has columns mesic, rok, druh and pocet

# distribuce log-transformovaných početností
fitdistrplus::descdist(
  log(chynov$pocet[chynov$pocet > 0]),
  discrete = FALSE
)


# fit uniform distribution
fit_uniform <- fitdistrplus::fitdist(chynov$pocet, "unif")

# normality tests
shap_raw <- shapiro.test(chynov$pocet)
shap_log <- shapiro.test(log(chynov$pocet))

# dip-test for multimodality
dip_raw <- diptest::dip.test(log(chynov$pocet))

# Kruskal–Wallis test across months
kruskal_month <- kruskal.test(pocet ~ mesic, data = chynov)

list(
  fit_uniform    = fit_uniform,
  shapiro_raw    = shap_raw,
  shapiro_log    = shap_log,
  dip_log        = dip_raw,
  kruskal_month  = kruskal_month
)


## BATS TRENDS BY SPECIES ----
bat_models <- chynov %>%
  dplyr::group_by(druh) %>%
  dplyr::reframe(
    tidied = list({
      fit <- try(stats::lm(pocet ~ rok, data = dplyr::pick(dplyr::everything())), silent = TRUE)
      if (inherits(fit, "try-error")) return(broom::tidy(stats::lm(0 ~ 1))) 
      broom::tidy(fit)
    }),
    glanced = list({
      fit <- try(stats::lm(pocet ~ rok, data = dplyr::pick(dplyr::everything())), silent = TRUE)
      if (inherits(fit, "try-error")) return(tibble::tibble(adj.r.squared = NA_real_))
      broom::glance(fit)
    }),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    coef    = purrr::map_dbl(tidied, ~ { tmp <- dplyr::filter(.x, term == "rok") %>% dplyr::pull(estimate); if(length(tmp) == 0) NA_real_ else tmp }),
    p.value = purrr::map_dbl(tidied, ~ { tmp <- dplyr::filter(.x, term == "rok") %>% dplyr::pull(p.value); if(length(tmp) == 0) NA_real_ else tmp }),
    adj.R2  = purrr::map_dbl(glanced, "adj.r.squared")
  ) %>%
  dplyr::select(-tidied, -glanced) %>%
  dplyr::mutate(
    trend = dplyr::case_when(
      is.na(p.value)        ~ "no data",
      p.value > 0.05        ~ "no trend",
      coef > 0              ~ "positive",
      TRUE                  ~ "negative"
    )
  )

# Optional: format only for display
bat_models_formatted <- bat_models %>%
  dplyr::mutate(
    dplyr::across(c(coef, p.value, adj.R2), ~ base::formatC(.x, digits = 3, format = "f"))
  )

# View or export
print(bat_models)

## Selected species ----
bat_models_selected <- chynov_selected %>%
  dplyr::group_by(druh) %>%
  dplyr::reframe(
    tidied = list({
      fit <- try(stats::lm(pocet ~ rok, data = dplyr::pick(dplyr::everything())), silent = TRUE)
      if (inherits(fit, "try-error")) return(broom::tidy(stats::lm(0 ~ 1))) 
      broom::tidy(fit)
    }),
    glanced = list({
      fit <- try(stats::lm(pocet ~ rok, data = dplyr::pick(dplyr::everything())), silent = TRUE)
      if (inherits(fit, "try-error")) return(tibble::tibble(adj.r.squared = NA_real_))
      broom::glance(fit)
    }),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    coef    = purrr::map_dbl(tidied, ~ { tmp <- dplyr::filter(.x, term == "rok") %>% dplyr::pull(estimate); if(length(tmp) == 0) NA_real_ else tmp }),
    p.value = purrr::map_dbl(tidied, ~ { tmp <- dplyr::filter(.x, term == "rok") %>% dplyr::pull(p.value); if(length(tmp) == 0) NA_real_ else tmp }),
    adj.R2  = purrr::map_dbl(glanced, "adj.r.squared")
  ) %>%
  dplyr::select(-tidied, -glanced) %>%
  dplyr::mutate(
    trend = dplyr::case_when(
      is.na(p.value)        ~ "no data",
      p.value > 0.05        ~ "no trend",
      coef > 0              ~ "positive",
      TRUE                  ~ "negative"
    )
  )

# Optional: format only for display
bat_models_formatted_selected <- bat_models_selected %>%
  dplyr::mutate(
    dplyr::across(c(coef, p.value, adj.R2), ~ base::formatC(.x, digits = 3, format = "f"))
  )

# View or export
print(bat_models_selected)


# Write results ----
readr::write_csv2(
  bat_models, 
  "Outputs/bat_trends_by_species.csv"
  )
readr::write_csv2(
  bat_models_selected, 
  "Outputs/bat_trends_by_species_selected.csv"
)
