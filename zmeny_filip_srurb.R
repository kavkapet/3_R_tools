library(readr)
library(dplyr)
library(tidyr)

setwd("d:/2_granty_projekty/2_Bezici/2023_SrUrb/01_reseni_projektu/08_blokova_maxima/zmeny_filip_2026/")

# --------------------------------------------------
# 1) nastavení
# --------------------------------------------------
input_file <- "subhr_cl.csv"
output_dir <- "gis_exports"
value_col  <- "diff"

# --------------------------------------------------
# 2) načtení dat
# --------------------------------------------------
df <- read.csv(
  file = input_file,
  header = TRUE,
  stringsAsFactors = FALSE,
  dec = ".",
  sep = ","
)

# --------------------------------------------------
# 3) kontrola sloupců
# --------------------------------------------------
need_cols <- c("cl", "yr", "dur", "rp", value_col, "lon", "lat")
miss <- setdiff(need_cols, names(df))

if (length(miss) > 0) {
  stop("Chybí sloupce: ", paste(miss, collapse = ", "))
}

# --------------------------------------------------
# 4) vytvoření výstupních složek
# --------------------------------------------------
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "wide_yr_dur"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "wide_dur_rp"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "by_rp"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "by_cl"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "by_cl_rp"), showWarnings = FALSE, recursive = TRUE)

# --------------------------------------------------
# 5) kontrola duplicit
# --------------------------------------------------
dup_yr_dur <- df %>%
  count(cl, lon, lat, rp, yr, dur) %>%
  filter(n > 1)

if (nrow(dup_yr_dur) > 0) {
  warning("Existují duplicity pro kombinaci cl-lon-lat-rp-yr-dur. Pivot může vytvořit více hodnot v jedné buňce.")
}

dup_dur_rp <- df %>%
  count(cl, lon, lat, yr, dur, rp) %>%
  filter(n > 1)

if (nrow(dup_dur_rp) > 0) {
  warning("Existují duplicity pro kombinaci cl-lon-lat-yr-dur-rp. Pivot může vytvořit více hodnot v jedné buňce.")
}

# --------------------------------------------------
# 6) wide varianta 1: sloupce = yr + dur
#    řádek = bod + rp + cl
# --------------------------------------------------
wide_yr_dur <- df %>%
  mutate(yr_dur = paste0("yr", yr, "_dur", dur)) %>%
  pivot_wider(
    id_cols = c(cl, lon, lat, rp),
    names_from = yr_dur,
    values_from = all_of(value_col)
  ) %>%
  arrange(cl, rp, lon, lat)

write_csv(
  wide_yr_dur,
  file.path(output_dir, "wide_yr_dur", paste0("wide_yr_dur_", value_col, ".csv"))
)

# --------------------------------------------------
# 7) export wide_yr_dur podle rp
# --------------------------------------------------
rp_values <- sort(unique(wide_yr_dur$rp))

for (r in rp_values) {
  
  df_out <- wide_yr_dur %>%
    filter(rp == r)
  
  write_csv(
    df_out,
    file.path(output_dir, "by_rp", paste0("wide_yr_dur_", value_col, "_rp", r, ".csv"))
  )
}

# --------------------------------------------------
# 8) export wide_yr_dur podle cl
# --------------------------------------------------
cl_values <- sort(unique(wide_yr_dur$cl))

for (cval in cl_values) {
  
  df_out <- wide_yr_dur %>%
    filter(cl == cval)
  
  write_csv(
    df_out,
    file.path(output_dir, "by_cl", paste0("wide_yr_dur_", value_col, "_cl_", cval, ".csv"))
  )
}

# --------------------------------------------------
# 9) export wide_yr_dur podle cl + rp
# --------------------------------------------------
for (cval in cl_values) {
  for (r in rp_values) {
    
    df_out <- wide_yr_dur %>%
      filter(cl == cval, rp == r)
    
    write_csv(
      df_out,
      file.path(output_dir, "by_cl_rp", paste0("wide_yr_dur_", value_col, "_cl_", cval, "_rp", r, ".csv"))
    )
  }
}

# --------------------------------------------------
# 10) wide varianta 2: sloupce = dur + rp
#     řádek = bod + yr + cl
# --------------------------------------------------
wide_dur_rp <- df %>%
  mutate(dur_rp = paste0("dur", dur, "_rp", rp)) %>%
  pivot_wider(
    id_cols = c(cl, lon, lat, yr),
    names_from = dur_rp,
    values_from = all_of(value_col)
  ) %>%
  arrange(cl, yr, lon, lat)

write_csv(
  wide_dur_rp,
  file.path(output_dir, "wide_dur_rp", paste0("wide_dur_rp_", value_col, ".csv"))
)

# --------------------------------------------------
# 11) export wide_dur_rp podle cl
# --------------------------------------------------
cl_values2 <- sort(unique(wide_dur_rp$cl))

for (cval in cl_values2) {
  
  df_out <- wide_dur_rp %>%
    filter(cl == cval)
  
  write_csv(
    df_out,
    file.path(output_dir, "wide_dur_rp", paste0("wide_dur_rp_", value_col, "_cl_", cval, ".csv"))
  )
}

# --------------------------------------------------
# 12) export wide_dur_rp podle yr
# --------------------------------------------------
yr_values <- sort(unique(wide_dur_rp$yr))

for (y in yr_values) {
  
  df_out <- wide_dur_rp %>%
    filter(yr == y)
  
  write_csv(
    df_out,
    file.path(output_dir, "wide_dur_rp", paste0("wide_dur_rp_", value_col, "_yr", y, ".csv"))
  )
}

# --------------------------------------------------
# 13) přehled do konzole
# --------------------------------------------------
cat("\nHotovo.\n")
cat("Vstupní soubor:", input_file, "\n")
cat("Výstupní složka:", output_dir, "\n")
cat("Použitý sloupec hodnot:", value_col, "\n")
cat("Počet záznamů:", nrow(df), "\n")
cat("Počet unikátních cl:", length(unique(df$cl)), "\n")
cat("Počet unikátních rp:", length(unique(df$rp)), "\n")
cat("Počet unikátních yr:", length(unique(df$yr)), "\n")
cat("Počet unikátních dur:", length(unique(df$dur)), "\n")



yr_target <- 2035
cl_target <- 3

df_out <- wide_dur_rp %>%
  filter(yr == yr_target, cl == cl_target)

write_csv(
  df_out,
  file.path(
    output_dir,
    "wide_dur_rp",
    paste0("wide_dur_rp_", value_col, "_yr", yr_target, "_cl", cl_target, ".csv")
  )
)
