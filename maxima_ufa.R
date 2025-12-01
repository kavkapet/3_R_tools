library(readxl)
library(dplyr)
library(ggplot2)
library(fst)
library(tidyr)

# Define the file path
setwd("d:/2_granty_projekty/2_Bezici/2023_SrUrb/01_reseni_projektu/08_blokova_maxima/")
#d:/2_granty_projekty/2_Bezici/2023_SrUrb/01_reseni_projektu/08_blokova_maxima/
file_path <- "Gridovana_Maxima_CR/Blokova_Maxima_1hod.xlsx"
folder_path <- "Gridovana_Maxima_CR"

data_spat = read.csv("gridy_ufa_id_jed_upv.csv", header = TRUE, sep = ",", dec = ".", encoding = "UTF-8")
# Read the first sheet "Pixely" and keep columns A-G
#pixely_data <- read_excel(file_path, sheet = "Pixely", range = "A1:G82738")

# Initialize a dataframe with the "Pixely" data
#final_data <- pixely_data

# Define the folder path containing the files

# List all files with "Blokova_Maxima" in their name and .xlsx extension
file_list <- list.files(path = folder_path, pattern = "Blokova_Maxima.*\\.xlsx$", full.names = TRUE)

# Initialize an empty dataframe for the final data
final_data <- NULL

# Loop through each file
for (file_path in file_list) {
  # Extract the file-specific suffix (everything after the last underscore in the file name)
  file_suffix <- tools::file_path_sans_ext(basename(file_path))
  file_suffix <- sub(".*_(.*)$", "\\1", file_suffix)
  
  # Read the first sheet "Pixely" and keep columns A-G
  pixely_data <- read_excel(file_path, sheet = "Pixely", range = "A1:G82738")
  
  # If final_data is NULL, initialize it with pixely_data
  if (is.null(final_data)) {
    final_data <- pixely_data
  }
  
  # Get the names of all sheets in the current file
  sheet_names <- excel_sheets(file_path)
  
  # Loop through all sheets except the first one
  for (sheet in sheet_names[-1]) {
    # Read the data from the current sheet, keeping columns A-U
    sheet_data <- read_excel(file_path, sheet = sheet, range = "A1:U82738")
    
    # Modify column names to include both the sheet name and file-specific suffix
    colnames(sheet_data)[-1] <- paste0(colnames(sheet_data)[-1], "_", sheet, "_", file_suffix)
    
    # Merge the data based on the "ID" column
    final_data <- merge(final_data, sheet_data, by = "ID", all.x = TRUE)
  }
}

# View the merged dataset
#head(final_data)

all_data <- merge(data_spat, final_data, by.x = "TARGET_FID",by.y = "ID", all = FALSE)
all_data_kat = all_data
all_data_upov = all_data
# Optional: Save the final dataset to a new Excel file
# write.xlsx(final_data, "Merged_Data_All_Files.xlsx")

# Function to extract and match suffix patterns from column names
extract_suffix <- function(colnames, delimiter) {
  sapply(strsplit(colnames, delimiter), function(x) paste(tail(x, 2), collapse = "_"))
}

# Extract suffix patterns from column names
suffix_patterns <- extract_suffix(colnames(all_data), "_")

# Initialize a new dataframe to store the calculated means

#usuffix_patterns = unique(suffix_patterns[20:307])
#usuffix_patterns = colnames(all_data[20:307])
usuffix_patterns = unique(suffix_patterns[20:length(suffix_patterns)])


#katastry
all_data_kat <- all_data_kat %>%
  filter(KOD_KU != "")
averages_kat <- data.frame("KOD_KU" = unique(all_data$KOD_KU))
#UPOVS
all_data_upov <- all_data_upov %>%
  filter(UPOV_ID != "")
averages_upov <- data.frame("UPOV_ID" = unique(all_data$UPOV_ID))
#kraje
all_data <- all_data %>%
  filter(NAZ_CZNUTS3 != "")
averages <- data.frame("NAZ_CZNUTS3" = unique(all_data$NAZ_CZNUTS3))
# Loop through unique suffix patterns and calculate means
for (suffix in usuffix_patterns) {
  # Identify columns with matching suffix
  split_text <- strsplit(suffix, "_")[[1]]
  suffix <- paste(tail(split_text, 2), collapse = "_")
  matching_cols <- grep(suffix, names(all_data), value = TRUE)
  #print(matching_cols[1])
  print(suffix)
  #split_text <- strsplit(matching_cols, "_")[[1]]
  #finame <- paste(tail(split_text, 2), collapse = "_")
  
  # Calculate mean for matching columns
  #colname_to_extract <- suffix
  finame <- suffix #sub(".*_(.*_.*)_.*$", "\\1", colname_to_extract)
  avg_finame = paste0("avg_", finame)
  max5_finame = paste0("max5_", finame)
  max_finame = paste0("max_", finame)
  
  avg_data <- all_data %>%
    select("NAZ_CZNUTS3", all_of(matching_cols)) %>%
    group_by(NAZ_CZNUTS3) %>%
    ungroup()
  avg_data[[avg_finame]] = rowMeans(avg_data[, 2:ncol(avg_data)], na.rm = TRUE)
  avg_data <- avg_data %>%
    group_by(NAZ_CZNUTS3) %>%
    summarise(across(-1, ~ mean(.x, na.rm = TRUE)))
  
  max5_data <- all_data %>%
    select("NAZ_CZNUTS3", all_of(matching_cols)[1:5]) %>%
    group_by(NAZ_CZNUTS3) %>%
    ungroup()
  max5_data[[max5_finame]] = rowMeans(max5_data[, 2:6], na.rm = TRUE)
  max5_data <- max5_data %>%
    group_by(NAZ_CZNUTS3) %>%
    summarise(across(-1, ~ max(.x, na.rm = TRUE)))
  
  max_data <- max5_data[,1:2]
  #max_data[[max_finame]] = avg_data[, 2]
  colnames(max_data)[2] = max_finame 
  
  
  # Rename columns based on suffix pattern

  # Merge with the averages dataframe
  averages <- merge(averages, avg_data[,c(1,ncol(avg_data))], by.x = "NAZ_CZNUTS3", by.y = "NAZ_CZNUTS3")
  averages <- merge(averages, max5_data[,c(1,ncol(max5_data))], by.x = "NAZ_CZNUTS3", by.y = "NAZ_CZNUTS3")
  averages <- merge(averages, max_data[,c(1,ncol(max_data))], by.x = "NAZ_CZNUTS3", by.y = "NAZ_CZNUTS3")
}
###UPOVs
for (suffix in usuffix_patterns) {
  # Identify columns with matching suffix
  split_text <- strsplit(suffix, "_")[[1]]
  suffix <- paste(tail(split_text, 2), collapse = "_")
  matching_cols <- grep(suffix, names(all_data), value = TRUE)
  #print(matching_cols[1])
  print(suffix)
  #split_text <- strsplit(matching_cols, "_")[[1]]
  #finame <- paste(tail(split_text, 2), collapse = "_")
  
  # Calculate mean for matching columns
  #colname_to_extract <- suffix
  finame <- suffix #sub(".*_(.*_.*)_.*$", "\\1", colname_to_extract)
  avg_finame = paste0("avg_", finame)
  max5_finame = paste0("max5_", finame)
  max_finame = paste0("max_", finame)
  
  avg_data <- all_data %>%
    select("UPOV_ID", all_of(matching_cols)) %>%
    group_by(UPOV_ID) %>%
    ungroup()
  avg_data[[avg_finame]] = rowMeans(avg_data[, 2:ncol(avg_data)], na.rm = TRUE)
  avg_data <- avg_data %>%
    group_by(UPOV_ID) %>%
    summarise(across(-1, ~ mean(.x, na.rm = TRUE)))
  
  max5_data <- all_data %>%
    select("UPOV_ID", all_of(matching_cols)[1:5]) %>%
    group_by(UPOV_ID) %>%
    ungroup()
  max5_data[[max5_finame]] = rowMeans(max5_data[, 2:6], na.rm = TRUE)
  max5_data <- max5_data %>%
    group_by(UPOV_ID) %>%
    summarise(across(-1, ~ max(.x, na.rm = TRUE)))
  
  max_data <- max5_data[,1:2]
  #max_data[[max_finame]] = avg_data[, 2]
  colnames(max_data)[2] = max_finame 
  
  
  # Rename columns based on suffix pattern
  
  # Merge with the averages dataframe
  averages_upov <- merge(averages_upov, avg_data[,c(1,ncol(avg_data))], by.x = "UPOV_ID", by.y = "UPOV_ID")
  averages_upov <- merge(averages_upov, max5_data[,c(1,ncol(max5_data))], by.x = "UPOV_ID", by.y = "UPOV_ID")
  averages_upov <- merge(averages_upov, max_data[,c(1,ncol(max_data))], by.x = "UPOV_ID", by.y = "UPOV_ID")
}

#katastry
for (suffix in usuffix_patterns) {
  # Identify columns with matching suffix
  split_text <- strsplit(suffix, "_")[[1]]
  suffix <- paste(tail(split_text, 2), collapse = "_")
  matching_cols <- grep(suffix, names(all_data), value = TRUE)
  #print(matching_cols[1])
  print(suffix)
  #split_text <- strsplit(matching_cols, "_")[[1]]
  #finame <- paste(tail(split_text, 2), collapse = "_")
  
  # Calculate mean for matching columns
  #colname_to_extract <- suffix
  finame <- suffix #sub(".*_(.*_.*)_.*$", "\\1", colname_to_extract)
  avg_finame = paste0("avg_", finame)
  max5_finame = paste0("max5_", finame)
  max_finame = paste0("max_", finame)
  
  avg_data <- all_data %>%
    select("KOD_KU", all_of(matching_cols)) %>%
    group_by(KOD_KU) %>%
    ungroup()
  avg_data[[avg_finame]] = rowMeans(avg_data[, 2:ncol(avg_data)], na.rm = TRUE)
  avg_data <- avg_data %>%
    group_by(KOD_KU) %>%
    summarise(across(-1, ~ mean(.x, na.rm = TRUE)))
  
  max5_data <- all_data %>%
    select("KOD_KU", all_of(matching_cols)[1:5]) %>%
    group_by(KOD_KU) %>%
    ungroup()
  max5_data[[max5_finame]] = rowMeans(max5_data[, 2:6], na.rm = TRUE)
  max5_data <- max5_data %>%
    group_by(KOD_KU) %>%
    summarise(across(-1, ~ max(.x, na.rm = TRUE)))
  
  max_data <- max5_data[,1:2]
  #max_data[[max_finame]] = avg_data[, 2]
  colnames(max_data)[2] = max_finame 
  
  
  # Rename columns based on suffix pattern
  
  # Merge with the averages dataframe
  averages_kat <- merge(averages_kat, avg_data[,c(1,ncol(avg_data))], by.x = "KOD_KU", by.y = "KOD_KU")
  averages_kat <- merge(averages_kat, max5_data[,c(1,ncol(max5_data))], by.x = "KOD_KU", by.y = "KOD_KU")
  averages_kat <- merge(averages_kat, max_data[,c(1,ncol(max_data))], by.x = "KOD_KU", by.y = "KOD_KU")
}

write.csv(averages, "averages_kraje.csv")
write.csv(averages_upov, "averages_upov.csv")
write.csv(averages_kat, "averages_kat.csv")


