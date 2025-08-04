#connect to dropbox to download

Sys.setenv(DROPBOX_REFRESH_TOKEN = "mWzxICjMxBUAAAAAAAAAAdKPMzxFnLqIVnJA5W77OGLc1mO6_6pnmXxfRQIy1S4b")
Sys.setenv(DROPBOX_APP_KEY = "7irn2mcwov0dslk")
Sys.setenv(DROPBOX_APP_SECRET = "hj13ag9wj44pah6")

library(reticulate)

# Load Dropbox SDK
dropbox <- import("dropbox")

# Authenticate using refresh token (headless)
dbx <- dropbox$Dropbox(
  oauth2_access_token = NULL,
  oauth2_refresh_token = Sys.getenv("DROPBOX_REFRESH_TOKEN"),
  app_key = Sys.getenv("DROPBOX_APP_KEY"),
  app_secret = Sys.getenv("DROPBOX_APP_SECRET")
)

res <- dbx$files_list_folder("/Enrollment Data")


# Step 2: Filter only file entries (ignore folders)
files_only <- Filter(function(x) py_has_attr(x, "server_modified"), res$entries)

# Check if the folder is empty
if (length(files_only) == 0) {
  stop("No files found in /Enrollment Data.")
}

# Step 3: Sort by modification time, descending
files_sorted <- files_only[order(sapply(files_only, function(x) x$server_modified), decreasing = TRUE)]

# Step 4: Get the latest file
latest_file <- files_sorted[[1]]

# --- 2. build local target path in the 'data' folder ---
dir.create("data", showWarnings = FALSE)        # make sure folder exists
local_path <- file.path("tracker/data/", latest_file$name)

# --- 3. download ---
dbx$files_download_to_file(local_path, latest_file$path_display)

cat("Downloaded", latest_file$name, "to", local_path, "\n")
