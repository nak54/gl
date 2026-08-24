"""
SQLite utility functions for the Educational Stock Trading Simulator.

The trade ledger is the durable source of truth. The Streamlit app rebuilds
cash, holdings, and the trade-history dashboard by replaying the rows stored
in this database.
"""

from __future__ import annotations

import sqlite3
from pathlib import Path
from typing import Any, Iterable


TRADE_COLUMNS = (
    "id",
    "executed_at",
    "side",
    "ticker",
    "qty",
    "price",
    "value",
    "shares_before",
    "shares_after",
    "cash_before",
    "cash_after",
    "reflection",
)


def _connect(db_path: str | Path) -> sqlite3.Connection:
    """Open a short-lived SQLite connection with dict-like rows."""
    path = Path(db_path)
    path.parent.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(path)
    conn.row_factory = sqlite3.Row
    return conn


def init_db(db_path: str | Path) -> None:
    """Create the trade ledger table if it does not already exist."""
    with _connect(db_path) as conn:
        conn.execute(
            """
            CREATE TABLE IF NOT EXISTS trades (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                executed_at TEXT NOT NULL,
                side TEXT NOT NULL CHECK (side IN ('Buy', 'Sell')),
                ticker TEXT NOT NULL,
                qty INTEGER NOT NULL CHECK (qty > 0),
                price REAL NOT NULL CHECK (price >= 0),
                value REAL NOT NULL CHECK (value >= 0),
                shares_before INTEGER NOT NULL CHECK (shares_before >= 0),
                shares_after INTEGER NOT NULL CHECK (shares_after >= 0),
                cash_before REAL NOT NULL,
                cash_after REAL NOT NULL,
                reflection TEXT NOT NULL
            )
            """
        )
        conn.execute(
            """
            CREATE INDEX IF NOT EXISTS idx_trades_executed_at
            ON trades (executed_at, id)
            """
        )


def _row_to_trade(row: sqlite3.Row) -> dict[str, Any]:
    """Convert one SQLite row into the transaction shape used by app.py."""
    return {
        "id": int(row["id"]),
        "time": row["executed_at"],
        "side": row["side"],
        "ticker": row["ticker"],
        "qty": int(row["qty"]),
        "price": float(row["price"]),
        "value": float(row["value"]),
        "shares_before": int(row["shares_before"]),
        "shares_after": int(row["shares_after"]),
        "cash_before": float(row["cash_before"]),
        "cash_after": float(row["cash_after"]),
        "reflection": row["reflection"],
    }


def load_trades(db_path: str | Path) -> list[dict[str, Any]]:
    """Load all persisted trades in execution order."""
    init_db(db_path)
    with _connect(db_path) as conn:
        rows = conn.execute(
            """
            SELECT id, executed_at, side, ticker, qty, price, value,
                   shares_before, shares_after, cash_before, cash_after, reflection
            FROM trades
            ORDER BY id
            """
        ).fetchall()
    return [_row_to_trade(row) for row in rows]


def record_trade(db_path: str | Path, trade: dict[str, Any]) -> int:
    """Persist one committed buy/sell transaction and return its row id."""
    init_db(db_path)
    with _connect(db_path) as conn:
        cursor = conn.execute(
            """
            INSERT INTO trades (
                executed_at, side, ticker, qty, price, value,
                shares_before, shares_after, cash_before, cash_after, reflection
            )
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            (
                trade["time"],
                trade["side"],
                trade["ticker"],
                int(trade["qty"]),
                float(trade["price"]),
                float(trade["value"]),
                int(trade["shares_before"]),
                int(trade["shares_after"]),
                float(trade["cash_before"]),
                float(trade["cash_after"]),
                trade.get("reflection", ""),
            ),
        )
        return int(cursor.lastrowid)


def clear_trades(db_path: str | Path) -> None:
    """Delete all persisted trade transactions."""
    init_db(db_path)
    with _connect(db_path) as conn:
        conn.execute("DELETE FROM trades")


def rebuild_portfolio(
    trades: Iterable[dict[str, Any]],
    starting_cash: float,
) -> tuple[float, dict[str, int]]:
    """Replay the persisted trade ledger into cash and open holdings."""
    cash = float(starting_cash)
    holdings: dict[str, int] = {}

    for trade in trades:
        ticker = trade["ticker"]
        qty = int(trade["qty"])
        value = float(trade["value"])

        if trade["side"] == "Buy":
            cash -= value
            holdings[ticker] = holdings.get(ticker, 0) + qty
        elif trade["side"] == "Sell":
            cash += value
            holdings[ticker] = holdings.get(ticker, 0) - qty

        if holdings.get(ticker, 0) <= 0:
            holdings.pop(ticker, None)

    return cash, holdings
