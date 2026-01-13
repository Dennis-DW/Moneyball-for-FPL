# Ensure data directory exists
if (!dir.exists("data")) dir.create("data")

# --- HELPER: GET FILE PATH ---
get_gw_filename <- function(gameweek) {
  return(file.path("data", paste0("prediction_gw_", gameweek, ".csv")))
}

# --- SAVE A NEW PREDICTION ---
save_prediction <- function(team_df, gameweek) {
  file_path <- get_gw_filename(gameweek)
  
  print(paste("📂 [Manager] Checking file path:", file_path))
  
  # Safety Check: Do not overwrite existing predictions for the same GW
  if (file.exists(file_path)) {
    warning(paste("Prediction for GW", gameweek, "already exists. Skipping save."))
    print("❌ [Manager] Save Aborted: File already exists.") 
    return(FALSE)
  }
  
  # Add metadata
  team_df$gameweek <- gameweek
  team_df$predicted_at <- Sys.time()
  
  # Write to file
  write.csv(team_df, file_path, row.names = FALSE)
  print(paste("✅ [Manager] SUCCESS! CSV file created at:", file_path))
  return(TRUE)
}

# --- CHECK IF PREDICTION EXISTS ---
has_prediction_for_gw <- function(gameweek) {
  file_path <- get_gw_filename(gameweek)
  return(file.exists(file_path))
}

# --- LOAD SPECIFIC PREDICTION ---
get_prediction <- function(gameweek) {
  file_path <- get_gw_filename(gameweek)
  
  if (!file.exists(file_path)) {
    return(NULL)
  }
  
  print(paste(" [Manager] Loading saved team from:", file_path))
  read.csv(file_path, stringsAsFactors = FALSE)
}