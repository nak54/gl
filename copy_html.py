"""
copy_html.py
Copies all .html files found under the source directory into a
single flat destination folder (html_ch), preserving filenames.
If two files share the same name, the later one is suffixed with
its parent folder name to avoid collisions.
"""

import shutil
from pathlib import Path

# ── Config ────────────────────────────────────────────────────────────────────
SOURCE_DIR = Path(__file__).parent          # folder containing this script
DEST_DIR   = SOURCE_DIR / "html_ch"         # target flat directory
# ─────────────────────────────────────────────────────────────────────────────


def copy_html_files(source: Path, dest: Path) -> None:
    dest.mkdir(parents=True, exist_ok=True)

    html_files = sorted(source.rglob("*.html"))
    if not html_files:
        print("No .html files found.")
        return

    seen: dict[str, Path] = {}   # filename stem → first source path
    copied = 0

    for src in html_files:
        # Skip files already inside the destination folder
        if dest in src.parents:
            continue

        stem = src.name
        if stem in seen:
            # Disambiguate: append parent folder name
            stem = f"{src.parent.name}_{src.name}"

        target = dest / stem
        shutil.copy2(src, target)
        seen[src.name] = src
        print(f"  copied  {src.relative_to(source)}  →  html_ch/{stem}")
        copied += 1

    print(f"\nDone — {copied} file(s) copied to '{dest}'.")


if __name__ == "__main__":
    copy_html_files(SOURCE_DIR, DEST_DIR)
