"""render_table_2_1_sample_by_league.py.

Reads  tables/table_2_1_sample_by_league.csv
Writes figures/table_2_1_sample_by_league.png

Style: Latin Modern Roman 12pt, zebra-striped rows, italic muted caption,
double-rule header (no filled header bar).
"""

from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd

from _style import (
    COLOUR_MUTED,
    COLOUR_ZEBRA,
    apply_global_style,
)

ROOT      = Path(__file__).resolve().parents[2]
TBL_PATH  = ROOT / "tables"  / "table_2_1_sample_by_league.csv"
OUT_PATH  = ROOT / "figures" / "table_2_1_sample_by_league.png"


def main() -> None:
    apply_global_style()

    df = pd.read_csv(TBL_PATH)
    df.columns = ["Div", "Country", "n matches",
                  "H%", "D%", "A%", "n train", "n test"]

    # Format percentages consistently
    for c in ("H%", "D%", "A%"):
        df[c] = df[c].apply(lambda v: f"{float(v):.1f}")

    # Append totals row
    totals = {
        "Div": "Total", "Country": "",
        "n matches": int(df["n matches"].sum()),
        "H%": "", "D%": "", "A%": "",
        "n train": int(df["n train"].sum()),
        "n test":  int(df["n test"].sum()),
    }
    df = pd.concat([df, pd.DataFrame([totals])], ignore_index=True)

    n_rows, n_cols = df.shape
    col_widths = [0.08, 0.32, 0.11, 0.07, 0.07, 0.07, 0.11, 0.11]

    fig_w, fig_h = 9.0, 0.40 * n_rows + 1.6
    fig, ax = plt.subplots(figsize=(fig_w, fig_h))
    ax.axis("off")

    tbl = ax.table(
        cellText   = df.values.tolist(),
        colLabels  = df.columns.tolist(),
        colWidths  = col_widths,
        cellLoc    = "left",
        loc        = "upper center",
    )
    tbl.auto_set_font_size(False)
    tbl.set_fontsize(12)
    tbl.scale(1, 1.35)

    # Double-rule header style: no fill, thicker top+bottom edges
    for c in range(n_cols):
        cell = tbl[(0, c)]
        cell.set_text_props(weight="bold")
        cell.set_facecolor("white")
        cell.visible_edges = "TB"
        cell.set_linewidth(1.2)

    # Body rows: zebra striping, totals row bolded and separated
    for r in range(1, n_rows + 1):
        row_colour = COLOUR_ZEBRA if (r - 1) % 2 else "white"
        is_total = df.iloc[r - 1]["Div"] == "Total"
        for c in range(n_cols):
            cell = tbl[(r, c)]
            cell.set_facecolor(row_colour)
            cell.visible_edges = ""
            if is_total:
                cell.set_text_props(weight="bold")
                cell.visible_edges = "T"
                cell.set_linewidth(1.0)

    # Bottom rule under the last body row
    for c in range(n_cols):
        cell = tbl[(n_rows, c)]
        cell.visible_edges = cell.visible_edges + "B" if cell.visible_edges else "B"
        cell.set_linewidth(1.2)

    # Italic muted caption
    caption = ("Table 2.1. Per-league sample breakdown after filtering to complete six-book "
               "1X2 odds over 2013/14-2023/24. n = 42,536 matches across 11 leagues.")
    fig.text(0.5, 0.02, caption, ha="center", va="bottom",
             style="italic", color=COLOUR_MUTED, fontsize=10, wrap=True)

    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(OUT_PATH, dpi=300, bbox_inches="tight", facecolor="white")
    plt.close(fig)
    print(f"Saved {OUT_PATH.relative_to(ROOT)}")


if __name__ == "__main__":
    main()
