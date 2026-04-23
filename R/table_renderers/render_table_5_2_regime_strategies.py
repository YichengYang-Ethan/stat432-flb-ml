"""render_table_5_2_regime_strategies.py.

Reads  tables/table_5_2_regime_strategies.csv
Writes figures/table_5_2_regime_strategies.png

Style: Latin Modern Roman 12pt. The best-ROI model within each Strategy block is
highlighted in pale green; the 'C: Regime 3+4 (ctl)' control block is tinted in
pale rose. Negative ROI / Sharpe values are muted red; positive are muted green.
"""

from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd

from _style import (
    COLOUR_CONTROL,
    COLOUR_MUTED,
    COLOUR_NEG,
    COLOUR_POS,
    COLOUR_WINNER,
    COLOUR_ZEBRA,
    apply_global_style,
)

ROOT     = Path(__file__).resolve().parents[2]
TBL_PATH = ROOT / "tables"  / "table_5_2_regime_strategies.csv"
OUT_PATH = ROOT / "figures" / "table_5_2_regime_strategies.png"


def main() -> None:
    apply_global_style()

    df = pd.read_csv(TBL_PATH)

    strategy_order = ["Unconditional", "A: Regime 1", "B: Regime 1+2",
                      "C: Regime 3+4 (ctl)"]
    df["Strategy"] = pd.Categorical(df["Strategy"], categories=strategy_order,
                                    ordered=True)
    df = df.sort_values(["Strategy", "ROI"], ascending=[True, False],
                        kind="stable").reset_index(drop=True)

    disp = pd.DataFrame({
        "Strategy":  df["Strategy"].astype(str),
        "Model":     df["Model"],
        "N":         df["N"].astype(int).map(lambda v: f"{v:,}"),
        "Avg odds":  df["Avg_odds"].map(lambda v: f"{v:.2f}"),
        "Win rate":  df["Win_rate"].map(lambda v: f"{v * 100:.1f}%"),
        "Profit":    df["Total_profit"].map(lambda v: f"{v:+.1f}"),
        "ROI":       df["ROI"].map(lambda v: f"{v:+.4f}"),
        "Sharpe":    df["Sharpe"].map(lambda v: f"{v:+.2f}"),
        "MaxDD abs": df["MaxDD_abs"].map(lambda v: f"{v:.1f}"),
    })
    numeric_roi    = df["ROI"].tolist()
    numeric_sharpe = df["Sharpe"].tolist()

    # Winner row per Strategy = highest ROI in that strategy block
    winner_rows: set[int] = set()
    for _, grp in df.groupby("Strategy", observed=True):
        winner_rows.add(grp["ROI"].idxmax())

    control_rows = set(df.index[df["Strategy"].astype(str) == "C: Regime 3+4 (ctl)"].tolist())

    n_rows, n_cols = disp.shape
    col_widths = [0.18, 0.14, 0.08, 0.09, 0.09, 0.10, 0.10, 0.09, 0.11]
    fig_w, fig_h = 11.5, 0.42 * n_rows + 1.9
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

    # Header
    for c in range(n_cols):
        cell = tbl[(0, c)]
        cell.set_text_props(weight="bold")
        cell.set_facecolor("white")
        cell.visible_edges = "TB"
        cell.set_linewidth(1.2)

    for r in range(1, n_rows + 1):
        is_winner  = (r - 1) in winner_rows
        is_control = (r - 1) in control_rows

        if is_winner:
            row_colour = COLOUR_WINNER
        elif is_control:
            row_colour = COLOUR_CONTROL
        else:
            row_colour = COLOUR_ZEBRA if (r - 1) % 2 else "white"

        for c in range(n_cols):
            cell = tbl[(r, c)]
            cell.set_facecolor(row_colour)
            cell.visible_edges = ""

        # Strategy-block separator
        if r == 1 or df.iloc[r - 1]["Strategy"] != df.iloc[r - 2]["Strategy"]:
            for c in range(n_cols):
                cell = tbl[(r, c)]
                cell.visible_edges = "T"
                cell.set_linewidth(0.6)

        # Colour ROI and Sharpe
        for col_idx, vec in ((6, numeric_roi), (7, numeric_sharpe)):
            v = vec[r - 1]
            if v is None or pd.isna(v):
                continue
            ink = COLOUR_POS if v > 0 else (COLOUR_NEG if v < 0 else "black")
            tbl[(r, col_idx)].get_text().set_color(ink)

        if is_winner:
            tbl[(r, 1)].get_text().set_weight("bold")
            tbl[(r, 6)].get_text().set_weight("bold")

    for c in range(n_cols):
        cell = tbl[(n_rows, c)]
        cell.visible_edges = "B"
        cell.set_linewidth(1.2)

    caption = ("Table 5.2. Regime-conditional betting strategies at Pinnacle closing. "
               "Green row = highest ROI within the strategy block; pale-rose block = "
               "control (regimes 3+4). All 1-unit flat stakes, EV > 0 filter.")
    fig.text(0.5, 0.02, caption, ha="center", va="bottom",
             style="italic", color=COLOUR_MUTED, fontsize=10, wrap=True)

    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(OUT_PATH, dpi=300, bbox_inches="tight", facecolor="white")
    plt.close(fig)
    print(f"Saved {OUT_PATH.relative_to(ROOT)}")


if __name__ == "__main__":
    main()
