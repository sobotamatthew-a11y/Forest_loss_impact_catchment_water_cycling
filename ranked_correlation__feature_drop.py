import pandas as pd
import numpy as np
import os
import glob

# Configuration
THRESHOLD = 0.85
BASE_DIR = "/home/mas22047/Forest_loss_RB_EI/"

# Target variables to protect
TARGETS = ['rb_diff1', 'rb_diff2', 'ei_diff1', 'ei_diff2']
# Metadata to ignore for correlation but keep in file
METADATA = ['gauge_name', 'gauge_id', 'loss_year', 'country']

# Find all files
csv_files = glob.glob(os.path.join(BASE_DIR, "*/data.csv"))
csv_files = [f for f in csv_files if "temp_backup" not in f]

print(f"Processing {len(csv_files)} files individually...")

for f in csv_files:
    df = pd.read_csv(f)

    # Isolate predictors
    cols_to_exclude = TARGETS + METADATA
    predictors_df = df.drop(columns=[c for c in cols_to_exclude if c in df.columns])

    # Only numeric columns
    predictors_df = predictors_df.select_dtypes(include=[np.number])

    # Correlation matrix
    corr_matrix = predictors_df.corr(method='spearman').abs()
    upper = corr_matrix.where(np.triu(np.ones(corr_matrix.shape), k=1).astype(bool))

    # Find redundant columns
    to_drop = [column for column in upper.columns if any(upper[column] > THRESHOLD)]

    print(f"{os.path.relpath(f, BASE_DIR)}: Dropping {len(to_drop)} redundant features...")

    # Drop redundant features
    df_cleaned = df.drop(columns=to_drop)
    df_cleaned.to_csv(f, index=False)

    print(f"   Final column count: {len(df_cleaned.columns)}")

print("\nDone! Each file has been processed individually.")
