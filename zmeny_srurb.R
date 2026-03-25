# install.packages(c("readr","dplyr","stringr"))
library(readr)
library(dplyr)
library(tidyr)
setwd("d:/2_granty_projekty/2_Bezici/2023_SrUrb/01_reseni_projektu/08_blokova_maxima/posuny_filip/")
# --- Nastavení cest ---
in_file <- read.csv2("cc.csv")          # <- uprav)
in_file = read.csv2(file = "cc.csv", header = TRUE, stringsAsFactors=FALSE, dec = ".", sep = ",")
df = in_file
df$h_model = df$q_model*df$dur*3600/1000*1000 # q_model jsou v jednotkách kg/s/ha a h_model chci výšky v mm
df$h_historical = df$q_historical*df$dur*3600/1000*1000 # 

# Kontrola, že jsou očekávané sloupce
need_cols <- c("id","dur","rp","model","yr","q_model","q_historical","diff","lon","lat")
miss <- setdiff(need_cols, names(df))
if (length(miss) > 0) stop("Chybí sloupce: ", paste(miss, collapse = ", "))

# --- 2) Průměry pro jednotlivé body (odděleně pro yr) ---
# "bod" + zachování rp a dur:
group_vars <- c("id","dur","rp","yr","lon","lat")

means <- df %>%
  group_by(across(all_of(group_vars))) %>%
  summarise(
    h_model_mean      = mean(h_model, na.rm = TRUE),
    h_historical_mean = mean(h_historical, na.rm = TRUE),
    h_model_std      = sd(h_model, na.rm = TRUE),
    h_historical_std = sd(h_historical, na.rm = TRUE),
    diff_mean         = mean(diff, na.rm = TRUE),
    n_models          = sum(!is.na(q_model)),
    .groups = "drop"
  )

# --- 3) Vyber model/scénář nejbližší průměru (podle q_model) ---
closest <- df %>%
  inner_join(means, by = group_vars) %>%
  mutate(dist_to_mean = abs(h_model - h_model_mean)) %>%
  group_by(across(all_of(group_vars))) %>%
  slice_min(order_by = dist_to_mean, with_ties = FALSE) %>%
  ungroup()

closest_out <- closest %>%
  transmute(
    across(all_of(group_vars)),
    closest_model = model,          # ← název scénáře
    h_model,
    h_model_mean,
    dist_to_mean,
    h_historical,
    h_historical_mean,
    diff,
    diff_mean
  )
model_counts_total <- closest_out %>%
  count(closest_model, sort = TRUE)

print(model_counts_total)
# --- Uložení "vše dohromady" ---
write_csv(means, "means_by_point_2035_2070.csv")
write_csv(closest_out, "closest_to_mean_by_point_2035_2070.csv")

# --- 4) Rozdělení na soubory pro 2035 a 2070 ---
means_2035   <- filter(means, yr == 2035)
means_2070   <- filter(means, yr == 2070)
closest_2035 <- filter(closest_out, yr == 2035)
closest_2070 <- filter(closest_out, yr == 2070)

write_csv(means_2035,"means_by_point_2035.csv")
write_csv(means_2070,"means_by_point_2070.csv")
write_csv(closest_2035,"closest_to_mean_by_point_2035.csv")
write_csv(closest_2070,"closest_to_mean_by_point_2070.csv")

# --- filtr pouze rp = 20 ---
means_rp20   <- means %>% filter(rp == 20)
closest_rp20 <- closest_out %>% filter(rp == 20)

# požadované délky trvání
durations <- c(1, 6, 24)

# --- cyklus přes dur a rok ---
for (d in durations) {
  
  # ===== 2035 =====
  write_csv(
    means_rp20 %>% filter(yr == 2035, dur == d),
    paste0("means_rp20_dur", d, "_2035.csv")
  )
  
  write_csv(
    closest_rp20 %>% filter(yr == 2035, dur == d),
    paste0("closest_rp20_dur", d, "_2035.csv")
  )
  
  # ===== 2070 =====
  write_csv(
    means_rp20 %>% filter(yr == 2070, dur == d),
    paste0("means_rp20_dur", d, "_2070.csv")
  )
  
  write_csv(
    closest_rp20 %>% filter(yr == 2070, dur == d),
    paste0("closest_rp20_dur", d, "_2070.csv")
  )
}


means_wide <- means %>%
  mutate(yr_dur = paste0("yr", yr, "_dur", dur)) %>%
  pivot_wider(
    id_cols = c(id, lon, lat, rp),
    names_from = yr_dur,
    values_from = h_model_mean
  ) %>%
  arrange(id, rp)

write.csv(means_wide, "means_wide_yr_dur.csv", row.names = FALSE)

rp_values <- sort(unique(means_wide$rp))

# vytvořit soubor pro každé rp
for (r in rp_values) {
  
  df_out <- means_wide %>%
    filter(rp == r)
  
  write_csv(
    df_out,
    paste0("means_wide_rp", r, ".csv")
  )
}


model_name <- "pr_EUR-11_MPI-M-MPI-ESM-LR_rcp85_r2i1p1_CLMcom-ETH-COSMO-crCLIM-v1-1_v1_1hr"

str_sce <- df %>%
  filter(model == model_name)

df_str_sce <- str_sce %>%
  mutate(dur_rp = paste0("dur", dur, "_rp", rp)) %>%
  pivot_wider(
    id_cols = c(id, lon, lat, yr),
    names_from = dur_rp,
    values_from = c(h_model, h_historical, diff)) %>%
  arrange(id, yr)
df_str_sce_2035 <- df_str_sce %>%
  filter(yr == 2035)
df_str_sce_2070 <- df_str_sce %>%
  filter(yr == 2070)

write.csv(df_str_sce, "means_h_scenario_wide.csv", row.names = FALSE)
write.csv(df_str_sce_2035, "means_h_scenario_wide_2035.csv", row.names = FALSE)
write.csv(df_str_sce_2070, "means_h_scenario_wide_2070.csv", row.names = FALSE)
