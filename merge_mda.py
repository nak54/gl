import io
import requests
import pandas as pd

from google.colab import drive
drive.mount('/content/drive')

BASE_RAW_25 = (
    "https://raw.githubusercontent.com/"
    "john-friedman/Every-10-K-MDA-01-01-1993-12-21-2025./main/"
    "batches_mda_compressed/2025"
)
BASE_RAW_24 = (
    "https://raw.githubusercontent.com/"
    "john-friedman/Every-10-K-MDA-01-01-1993-12-21-2025./main/"
    "batches_mda_compressed/2024"
)

files = [f"batch_{i}.parquet.zst" for i in range(7)]

dfs1 = []
dfs2 = []
for fname in files:
    url1 = f"{BASE_RAW_24}/{fname}"
	url2 = f"{BASE_RAW_25}/{fname}"
	
    print(f"Reading {url1}")
    r1 = requests.get(url1, timeout=120)
    r1.raise_for_status()

    # Read parquet bytes directly from memory
    df1 = pd.read_parquet(io.BytesIO(r1.content), engine="pyarrow")
    df1["source_file"] = fname
    dfs1.append(df1)
	
	print(f"Reading {url2}")
    r2 = requests.get(url2, timeout=120)
    r2.raise_for_status()

    # Read parquet bytes directly from memory
    df2 = pd.read_parquet(io.BytesIO(r2.content), engine="pyarrow")
    df2["source_file"] = fname
    dfs2.append(df2)

merged = pd.concat([dfs1,dfs2], ignore_index=True)

print("Rows:", len(merged))
print("Columns:", list(merged.columns))

output_path = "/content/drive/MyDrive/mda/mda_24_25_merged.parquet"
merged.to_parquet(output_path, engine="pyarrow", index=False)

print(f"Wrote {output_path}")

