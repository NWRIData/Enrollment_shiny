import os
import dropbox
from datetime import datetime

# Set your credentials (ideally use GitHub secrets)
DROPBOX_REFRESH_TOKEN = "mWzxICjMxBUAAAAAAAAAAdKPMzxFnLqIVnJA5W77OGLc1mO6_6pnmXxfRQIy1S4b"
DROPBOX_APP_KEY = "7irn2mcwov0dslk"
DROPBOX_APP_SECRET = "hj13ag9wj44pah6"

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

# Modify filename to add "Leo" before extension
filename, ext = os.path.splitext(latest.name)
new_filename = f"{filename}_Leo{ext}"

# Create output dir and download
os.makedirs("tracker/data", exist_ok=True)
local_path = os.path.join("tracker/data", new_filename)

dbx.files_download_to_file(local_path, latest.path_display)

print(f"Downloaded {latest.name} as {new_filename} to {local_path}")
