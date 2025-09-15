#----------------------------------------------------------#
# Load packages -----
#----------------------------------------------------------#
packages <- c(
  "tidyverse",
  "janitor",
  "sf", 
  "sp", 
  "proj4", 
  "openxlsx",
  "fuzzyjoin", 
  "remotes"
)

# Standard package
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

#----------------------------------------------------------#
# Load data -----
#----------------------------------------------------------#
# no header
raw <- openxlsx::read.xlsx("Data/Input/netopyri_chynov.xlsx", sheet = 1, colNames = FALSE)

# vytažení prvních dvou řádků jako "víceúrovňový header"
header1 <- raw[1, ] %>% unlist() %>% as.character()
header2 <- raw[2, ] %>% unlist() %>% as.character()

# sloučení do jedné hlavičky
col_names <- ifelse(is.na(header2), header1, paste(header1, header2, sep = "_"))

# přejmenování tabulky s novou hlavičkou
df <- raw[-c(1,2), ] %>%
  setNames(col_names)

# první sloupec je sezóna
df <- df %>%
  rename(sezona = 1)

# převod do long formátu
chynov <- df %>%
  tidyr::pivot_longer(
    cols = -sezona,
    names_to = c("druh", "mesic"),
    names_sep = "_",
    values_to = "pocet"
  ) %>%
  dplyr::mutate(
    pocet = as.numeric(pocet),
    mesic = dplyr::recode(mesic,
                          "prosinec" = "12",
                          "únor"    = "02"),
    # Extract start and end years from original sezona
    sezona_start = sub("/.*", "", sezona),
    sezona_end   = sub(".*/", "", sezona),
    # Convert 2-digit years to 4-digit years
    sezona_start = dplyr::case_when(
      nchar(sezona_start) == 2 & as.numeric(sezona_start) <= 30 ~ paste0("20", sezona_start),
      nchar(sezona_start) == 2 & as.numeric(sezona_start) > 30  ~ paste0("19", sezona_start),
      TRUE ~ sezona_start
    ),
    sezona_end = dplyr::case_when(
      nchar(sezona_end) == 2 & as.numeric(sezona_end) <= 30 ~ paste0("20", sezona_end),
      nchar(sezona_end) == 2 & as.numeric(sezona_end) > 30  ~ paste0("19", sezona_end),
      TRUE ~ sezona_end
    ),
    # Rebuild adjusted sezona as "YYYY/YYYY"
    sezona = paste0(sezona_start, "/", sezona_end),
    # Numeric year for plotting
    rok = ifelse(mesic == "12", as.numeric(sezona_start), as.numeric(sezona_end))
  ) %>%
  dplyr::select(rok, mesic, sezona, druh, pocet) %>%
  # replace all NAs with 0
  dplyr::mutate(
    dplyr::across(everything(), ~ ifelse(is.na(.x), 0, .x))
  )

glimpse(chynov)

# Select species and month for detailed analysis ----
chynov_selected <-
  chynov %>%
  dplyr::filter(
    druh %in% c(
      "Myotis nattereri", 
      "Myotis daubentonii", 
      "Myotis myotis", 
      "Plecotus auritus", 
      "Barbastella barbastellus",
      "celkem")
    ) %>%
  dplyr::filter(
    mesic == "02"
  )

# Write processed data ----
write_csv2(
  chynov,
  "Data/Processed/chynov.csv"
)

write_csv2(
  chynov_selected,
  "Data/Processed/chynov_selected.csv"
)
