"""render_table_4_7_per_regime.py.

Reads  tables/table_4_7_per_regime.csv
Writes figures/table_4_7_per_regime.png

Style: Latin Modern Roman 12pt, the model with the lowest Brier within each
regime block is highlighted in pale green; positive lifts coloured muted green,
negative lifts muted red.
"""

from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd

from _style import (
    COLOUR_MUTED,
    COLOUR_NEG,
    COLOUR_POS,
    COLOUR_WINNER,
    COLOUR_ZEBRA,
    apply_global_style,
)

ROOT     = Path(__file__).resolve().parents[2]
TBL_PATH = ROOT / "tables"  / "table_4_7_per_regime.csv"
OUT_PATH = ROOT / "figures" / "table_4_7_per_regime.png"


def main() -> None:
    apply_global_style()

    df = pd.read_csv(TBL_PATH)
    df = df.sort_values(["Regime", "Brier"], kind="stable").reset_index(drop=True)

    # Display frame
    disp = pd.DataFrame({
        "Regime":      df["Regime"].astype(str),
        "Model":       df["Model"],
        "N":           df["n"].astype(int).map(lambda v: f"{v:,}"),
        "Brier":       df["Brier"].map(lambda v: f"{v:.4f}"),
        "LogLoss":     df["LogLoss"].map(lambda v: f"{v:.4f}"),
        "Macro-AUC":   df["MacroAUC"].map(lambda v: f"{v:.4f}"),
        "Brier lift":  df["BrierLift"].map(lambda v: f"{v:+.4f}"),
        "LogLoss lift":df["LogLossLift"].map(lambda v: f"{v:+.4f}"),
    })
    numeric_brier_lift   = df["BrierLift"].tolist()
    numeric_logloss_lift = df["LogLossLift"].tolist()

    # Winner row per Regime = the lowest Brier within that regime (sorted first)
    winner_rows = set()
    for _, grp in df.groupby("Regime"):
        winner_rows.add(grp["Brier"].idxmin())

    n_rows, n_cols = disp.shape
    col_widths = [0.08, 0.17, 0.10, 0.10, 0.11, 0.12, 0.12, 0.13]
    fig_w, fig_h = 10.5, 0.40 * n_rows + 1.8
    fig, ax = plt.subplots(figsize=(fig_w, fig_h))
    ax.axis("off")

    tbl = ax.table(
        cellText   = disp.values.tolist(),
        colLabels  = disp.columns.tolist(),
        colWidths  = col_widths,
        cellLoc    = "center",
        loc        = "upper center",
    )
    tbl.auto_set_font_size(False)
    tbl.set_fontsize(12)
    tbl.scale(1, 1.35)

    # Header: double-rule style, no fill
    for c in range(n_cols):
        cell = tbl[(0, c)]
        cell.set_text_props(weight="bold")
        cell.set_facecolor("white")
        cell.visible_edges = "TB"
        cell.set_linewidth(1.2)

    for r in range(1, n_rows + 1):
        is_winner = (r - 1) in winner_rows
        row_colour = COLOUR_WINNER if is_winner else (
            COLOUR_ZEBRA if (r - 1) % 2 else "white"
        )
        for c in range(n_cols):
            cell = tbl[(r, c)]
            cell.set_facecolor(row_colour)
            cell.visible_edges = ""

        # Regime block separator: thin top rule at first row of each regime
        if r == 1 or df.iloc[r - 1]["Regime"] != df.iloc[r - 2]["Regime"]:
            for c in range(n_cols):
                cell = tbl[(r, c)]
                cell.visible_edges = "T"
                cell.set_linewidth(0.6)

        # Colour the Brier-lift and LogLoss-lift cells
        for col_idx, vec in ((6, numeric_brier_lift), (7, numeric_logloss_lift)):
            v = vec[r - 1]
            if v is None or pd.isna(v):
                continue
            ink = COLOUR_POS if v > 0 else (COLOUR_NEG if v < 0 else "black")
            tbl[(r, col_idx)].get_text().set_color(ink)

        # Winner row: bold the model name and Brier
        if is_winner:
            tbl[(r, 1)].get_text().set_weight("bold")
            tbl[(r, 3)].get_text().set_weight("bold")

    # Bottom rule
    for c in range(n_cols):
        cell = tbl[(n_rows, c)]
        cell.visible_edges = "B"
        cell.set_linewidth(1.2)

    caption = ("Table 4.7. Per-regime test-set metrics across 2 baselines and 3 ML models. "
               "Green row = model with lowest Brier within the regime. Lift columns are "
               "relative improvements over the Naive-baseline reference.")
    fig.text(0.5, 0.02, caption, ha="center", va="bottom",
             style="italic", color=COLOUR_MUTED, fontsize=10, wrap=True)

    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(OUT_PATH, dpi=300, bbox_inches="tight", facecolor="white")
    plt.close(fig)
    print(f"Saved {OUT_PATH.relative_to(ROOT)}")


if __name__ == "__main__":
    main()
