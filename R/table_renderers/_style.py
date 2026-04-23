"""Shared style tokens for the three FLB table renderers.

Fonts and colour palette match the report PDF:
    font        : Latin Modern Roman 12pt (falls back to serif if missing)
    winner row  : #edf4ed  (pale green)
    control row : #f3eeee  (pale rose)
    positive    : #2b6b3f  (muted green)
    negative    : #a63a2a  (muted red)
    muted text  : #6b6b6b
"""

from __future__ import annotations

import matplotlib as mpl

FONT_FAMILY    = ["Latin Modern Roman", "CMU Serif", "STIX Two Text", "serif"]
FONT_SIZE      = 12
COLOUR_WINNER  = "#edf4ed"
COLOUR_CONTROL = "#f3eeee"
COLOUR_ZEBRA   = "#fafafa"
COLOUR_POS     = "#2b6b3f"
COLOUR_NEG     = "#a63a2a"
COLOUR_MUTED   = "#6b6b6b"


def apply_global_style() -> None:
    """Set matplotlib rcParams once per script."""
    mpl.rcParams["font.family"] = FONT_FAMILY
    mpl.rcParams["font.size"]   = FONT_SIZE
    mpl.rcParams["axes.unicode_minus"] = False


def fmt_signed_num(x: float, fmt: str = "{:+.4f}") -> str:
    """Format a signed number (e.g. ROI / delta) for display."""
    if x is None:
        return ""
    return fmt.format(x)


def colour_signed(x: float) -> str:
    """Return the muted-red / muted-green ink colour for a signed value."""
    if x is None:
        return "black"
    return COLOUR_POS if x > 0 else (COLOUR_NEG if x < 0 else "black")
