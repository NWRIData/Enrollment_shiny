import os
import dropbox
from datetime import datetime

# Fetch credentials from the runner's environment variables
DROPBOX_REFRESH_TOKEN = os.environ.get("DROPBOX_REFRESH_TOKEN")
DROPBOX_APP_KEY = os.environ.get("DROPBOX_APP_KEY")
DROPBOX_APP_SECRET = os.environ.get("DROPBOX_APP_SECRET")

# Quick safety check to make sure they loaded correctly
if not all([DROPBOX_REFRESH_TOKEN, DROPBOX_APP_KEY, DROPBOX_APP_SECRET]):
    raise ValueError("Missing Dropbox credentials! Check your GitHub Secrets.")

# Authenticate
dbx = dropbox.Dropbox(
    oauth2_access_token=None,
    oauth2_refresh_token=DROPBOX_REFRESH_TOKEN,
    app_key=DROPBOX_APP_KEY,
    app_secret=DROPBOX_APP_SECRET
)

# List files in folder
res = dbx.files_list_folder("/Enrollment Data")
files = [entry for entry in res.entries if hasattr(entry, "server_modified")]

if not files:
    raise Exception("No files found in /Enrollment Data.")

# Sort by server_modified time
files.sort(key=lambda x: x.server_modified, reverse=True)
latest = files[0]

# Create output dir
os.makedirs("tracker/data", exist_ok=True)
local_path = os.path.join("tracker/data", latest.name)

# --- SMART POLLING LOGIC ---
if os.path.exists(local_path):
    print(f"Latest file '{latest.name}' already exists locally. No new data.")
    
    # Send a "false" signal to GitHub Actions
    if "GITHUB_OUTPUT" in os.environ:
        with open(os.environ["GITHUB_OUTPUT"], "a") as f:
            f.write("new_data=false\n")
else:
    print(f"New file detected: '{latest.name}'. Downloading...")
    dbx.files_download_to_file(local_path, latest.path_display)
    print(f"Successfully downloaded to {local_path}")
    
    # Send a "true" signal to GitHub Actions
    if "GITHUB_OUTPUT" in os.environ:
        with open(os.environ["GITHUB_OUTPUT"], "a") as f:
            f.write("new_data=true\n")
