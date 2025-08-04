import pandas as pd
import mysql.connector
from datetime import date

import os



# Connect to the database
conn = mysql.connector.connect(
    host=os.environ['DB_HOST'],
    database=os.environ['DB_NAME'],
    user=os.environ['DB_USER'],
    password=os.environ['DB_PASSWORD'],
    port=3306,
    ssl_disabled=False
)

# Create a cursor to execute SQL commands
cursor = conn.cursor()
cursor.execute("SHOW TABLES LIKE 'nwri_eligible_enrolled'")
nwri_eligible_enrolled = cursor.fetchall()

# Load data
query = """
SELECT 
    NWRIEnrollmentID, 
    DistrictName, 
    EnrollmentDate,
    AdmissionDate 
FROM 
    nwri_eligible_enrolled
"""

df = pd.read_sql(query, conn)

# Get today's date in YYYY-MM-DD format
today = date.today().isoformat()  # e.g., '2025-07-07'

# Build filename with date
filename = f"docs/enrollment/data/nwri_eligible_enrolled_2425_{today}.csv"
# Save the file
df.to_csv(filename, index=False)

# Close the cursor and connection
cursor.close()
conn.close()
