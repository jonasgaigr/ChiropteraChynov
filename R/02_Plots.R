# Ensure output directories exist ----
if(!dir.exists("Outputs/Plots/monthly")) dir.create("Outputs/Plots/monthly", recursive = TRUE)
if(!dir.exists("Outputs/Plots/season"))  dir.create("Outputs/Plots/season", recursive = TRUE)
if(!dir.exists("Outputs/Plots/selected"))  dir.create("Outputs/Plots/selected", recursive = TRUE)

# Use species from chynov as the master key
species_list <- unique(chynov$druh)
species_list_selected <- unique(chynov_selected$druh)

# Plot function with optional monthly coloring ----
plot_species <- function(sp, data, monthly = FALSE, output_dir = "Outputs/Plots") {
  df_sp <- dplyr::filter(data, druh == sp)
  
  # Převod měsíců jen pokud monthly = TRUE
  if (monthly) {
    df_sp <- df_sp %>%
      dplyr::mutate(
        mesic = dplyr::case_when(
          mesic %in% c("2", "02", 2)   ~ "únor",
          mesic %in% c("12", "12", 12) ~ "prosinec",
          TRUE ~ as.character(mesic)
        ),
        mesic = factor(mesic, levels = c("prosinec", "únor"))
      )
    aes_mapping <- aes(x = as.numeric(rok), y = pocet, colour = mesic)
  } else {
    aes_mapping <- aes(x = as.numeric(rok), y = pocet)
  }
  
  p <- ggplot(df_sp, aes_mapping) + 
    geom_point() +
    geom_smooth(method = "glm", se = TRUE) +
    ggtitle(paste0("Vývoj počtu jedinců – ", sp, "\n")) +
    xlab("\nrok") +
    ylab("počet\n") +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 12),
      plot.title = element_text(hjust = 0.5),
      panel.grid = element_blank(),
      legend.title = element_text(),
      legend.position = if(monthly) c(1.125, 0.5) else "none",
      legend.text = element_text(size = 9),
      plot.margin = grid::unit(c(1,3,1,1), "cm")
    ) +
    theme(legend.title.align = 0.5)
  
  # Save the plot
  filename <- paste0(output_dir, "/", gsub(" ", "_", sp), ".png")
  ggsave(filename = filename, plot = p, width = 8, height = 5)
  
  return(p)
}

# Monthly plots ----
plots_monthly <- purrr::map(species_list, ~ plot_species(.x, chynov, monthly = TRUE, output_dir = "Outputs/Plots/monthly"))

# Season-only plots ----
# Ensure rok numeric for plotting
chynov_sum <- chynov_sum %>%
  dplyr::mutate(rok = as.numeric(str_sub(sezona, 1, 4)))

plots_season <- purrr::map(species_list, ~ plot_species(.x, chynov_sum, monthly = FALSE, output_dir = "Outputs/Plots/season"))

# Selected species plots ----
plots_selected <- purrr::map(species_list_selected, ~ plot_species(.x, chynov_selected, monthly = FALSE, output_dir = "Outputs/Plots/selected"))
