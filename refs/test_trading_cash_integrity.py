"""
Pytest tests for the Educational Stock Trading Simulator SQLite ledger.

Copy this file into your project as tests/test_trading_cash_integrity.py, or run it
against a specific module path:

    TRADE_STORE_PATH=./trade_store.py pytest -q tests/test_trading_cash_integrity.py

In this sandbox, the uploaded file has a generated name, so the tests also know how
to load trade_store(4).py when TRADE_STORE_PATH is not set.
"""

from __future__ import annotations

import importlib.util
import os
import sqlite3
from pathlib import Path
from types import ModuleType
from typing import Any

import pytest

STARTING_CASH = 100_000.0


def _load_trade_store() -> ModuleType:
    """Load trade_store.py from normal project layout or from an explicit path."""
    env_path = os.environ.get("TRADE_STORE_PATH")
    candidates = []
    if env_path:
        candidates.append(Path(env_path))

    here = Path(__file__).resolve().parent
    candidates.extend(
        [
            here.parent / "trade_store.py",
            here / "trade_store.py",
            here / "trade_store(4).py",
            Path("/mnt/data/trade_store(4).py"),
        ]
    )

    for candidate in candidates:
        if candidate.exists():
            spec = importlib.util.spec_from_file_location("trade_store_under_test", candidate)
            assert spec and spec.loader, f"Could not load module spec for {candidate}"
            module = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(module)
            return module

    raise FileNotFoundError(
        "Could not find trade_store.py. Set TRADE_STORE_PATH=/path/to/trade_store.py."
    )


trade_store = _load_trade_store()


@pytest.fixture()
def db_path(tmp_path: Path) -> Path:
    return tmp_path / "nested" / "trades.sqlite3"


def _trade(
    *,
    side: str,
    ticker: str,
    qty: int,
    price: float,
    cash_before: float,
    shares_before: int,
    reflection: str = "unit test reflection",
) -> dict[str, Any]:
    """Build a trade row in the same shape used by app._execute_trade."""
    value = qty * price
    if side == "Buy":
        cash_after = cash_before - value
        shares_after = shares_before + qty
    elif side == "Sell":
        cash_after = cash_before + value
        shares_after = shares_before - qty
    else:
        cash_after = cash_before
        shares_after = shares_before

    return {
        "time": "2026-08-24 09:00:00",
        "side": side,
        "ticker": ticker,
        "qty": qty,
        "price": price,
        "value": value,
        "shares_before": shares_before,
        "shares_after": shares_after,
        "cash_before": cash_before,
        "cash_after": cash_after,
        "reflection": reflection,
    }


def _assert_ledger_is_internally_consistent(
    trades: list[dict[str, Any]], starting_cash: float = STARTING_CASH
) -> tuple[float, dict[str, int]]:
    """
    Strong test-side validator.

    trade_store.rebuild_portfolio intentionally replays trade rows. This helper
    also verifies that each persisted row agrees with the replayed cash and share
    counts, so the test suite catches corrupt rows or future implementation drift.
    """
    cash = float(starting_cash)
    holdings: dict[str, int] = {}

    for row in trades:
        side = row["side"]
        ticker = row["ticker"]
        qty = int(row["qty"])
        price = float(row["price"])
        value = float(row["value"])
        shares_before = holdings.get(ticker, 0)

        assert side in {"Buy", "Sell"}
        assert qty > 0
        assert price >= 0
        assert value >= 0
        assert value == pytest.approx(qty * price)
        assert float(row["cash_before"]) == pytest.approx(cash)
        assert int(row["shares_before"]) == shares_before

        if side == "Buy":
            assert value <= cash, "buy would create negative cash/no-margin violation"
            cash -= value
            shares_after = shares_before + qty
        else:
            assert qty <= shares_before, "sell would create short/oversell violation"
            cash += value
            shares_after = shares_before - qty

        assert float(row["cash_after"]) == pytest.approx(cash)
        assert int(row["shares_after"]) == shares_after

        if shares_after:
            holdings[ticker] = shares_after
        else:
            holdings.pop(ticker, None)

    rebuilt_cash, rebuilt_holdings = trade_store.rebuild_portfolio(trades, starting_cash)
    assert rebuilt_cash == pytest.approx(cash)
    assert rebuilt_holdings == holdings
    return cash, holdings


def test_new_database_loads_empty_and_rebuilds_starting_cash(db_path: Path) -> None:
    trade_store.init_db(db_path)

    assert db_path.exists()
    assert trade_store.load_trades(db_path) == []
    cash, holdings = trade_store.rebuild_portfolio([], STARTING_CASH)
    assert cash == pytest.approx(STARTING_CASH)
    assert holdings == {}


def test_init_db_creates_missing_parent_directory(tmp_path: Path) -> None:
    db_path = tmp_path / "does" / "not" / "exist" / "trades.sqlite3"

    trade_store.init_db(db_path)

    assert db_path.exists()


def test_buy_sell_sequence_persists_reloads_and_rebuilds_cash_and_holdings(db_path: Path) -> None:
    cash = STARTING_CASH
    shares = {"AAA": 0, "BBB": 0}

    t1 = _trade(side="Buy", ticker="AAA", qty=10, price=50.00, cash_before=cash, shares_before=shares["AAA"])
    trade_store.record_trade(db_path, t1)
    cash = t1["cash_after"]
    shares["AAA"] = t1["shares_after"]

    t2 = _trade(side="Buy", ticker="BBB", qty=5, price=200.00, cash_before=cash, shares_before=shares["BBB"])
    trade_store.record_trade(db_path, t2)
    cash = t2["cash_after"]
    shares["BBB"] = t2["shares_after"]

    t3 = _trade(side="Sell", ticker="AAA", qty=4, price=55.00, cash_before=cash, shares_before=shares["AAA"])
    trade_store.record_trade(db_path, t3)

    trades = trade_store.load_trades(db_path)
    final_cash, final_holdings = _assert_ledger_is_internally_consistent(trades)

    assert [row["side"] for row in trades] == ["Buy", "Buy", "Sell"]
    assert final_cash == pytest.approx(98_720.00)
    assert final_holdings == {"AAA": 6, "BBB": 5}
    assert trades[-1]["cash_after"] == pytest.approx(final_cash)
    assert trades[-1]["shares_after"] == 6


def test_full_liquidation_removes_zero_share_position(db_path: Path) -> None:
    buy = _trade(side="Buy", ticker="AAA", qty=3, price=100.00, cash_before=STARTING_CASH, shares_before=0)
    trade_store.record_trade(db_path, buy)
    sell = _trade(side="Sell", ticker="AAA", qty=3, price=100.00, cash_before=buy["cash_after"], shares_before=3)
    trade_store.record_trade(db_path, sell)

    trades = trade_store.load_trades(db_path)
    final_cash, final_holdings = _assert_ledger_is_internally_consistent(trades)

    assert final_cash == pytest.approx(STARTING_CASH)
    assert final_holdings == {}


def test_clear_trades_resets_ledger_to_starting_cash(db_path: Path) -> None:
    trade_store.record_trade(
        db_path,
        _trade(side="Buy", ticker="AAA", qty=1, price=100.00, cash_before=STARTING_CASH, shares_before=0),
    )
    assert len(trade_store.load_trades(db_path)) == 1

    trade_store.clear_trades(db_path)

    trades = trade_store.load_trades(db_path)
    cash, holdings = trade_store.rebuild_portfolio(trades, STARTING_CASH)
    assert trades == []
    assert cash == pytest.approx(STARTING_CASH)
    assert holdings == {}


def test_rebuild_portfolio_is_deterministic_across_multiple_loads(db_path: Path) -> None:
    trade_store.record_trade(
        db_path,
        _trade(side="Buy", ticker="AAA", qty=2, price=125.00, cash_before=STARTING_CASH, shares_before=0),
    )

    first_load = trade_store.load_trades(db_path)
    second_load = trade_store.load_trades(db_path)

    assert first_load == second_load
    assert trade_store.rebuild_portfolio(first_load, STARTING_CASH) == trade_store.rebuild_portfolio(second_load, STARTING_CASH)


def test_autoincrement_ids_are_in_execution_order(db_path: Path) -> None:
    id1 = trade_store.record_trade(
        db_path,
        _trade(side="Buy", ticker="AAA", qty=1, price=10.00, cash_before=STARTING_CASH, shares_before=0),
    )
    id2 = trade_store.record_trade(
        db_path,
        _trade(side="Buy", ticker="BBB", qty=1, price=20.00, cash_before=99_990.00, shares_before=0),
    )

    trades = trade_store.load_trades(db_path)

    assert id1 < id2
    assert [row["id"] for row in trades] == [id1, id2]


@pytest.mark.parametrize(
    "bad_update",
    [
        {"side": "Hold"},
        {"qty": 0},
        {"qty": -1},
        {"price": -0.01},
        {"value": -0.01},
        {"shares_before": -1},
        {"shares_after": -1},
    ],
)
def test_sqlite_constraints_reject_invalid_trade_rows(db_path: Path, bad_update: dict[str, Any]) -> None:
    trade = _trade(side="Buy", ticker="AAA", qty=1, price=100.00, cash_before=STARTING_CASH, shares_before=0)
    trade.update(bad_update)

    with pytest.raises(sqlite3.IntegrityError):
        trade_store.record_trade(db_path, trade)


def test_inconsistent_cash_or_trade_math_is_detected_by_integrity_helper(db_path: Path) -> None:
    bad_trade = _trade(side="Buy", ticker="AAA", qty=2, price=100.00, cash_before=STARTING_CASH, shares_before=0)
    bad_trade["value"] = 999.00
    bad_trade["cash_after"] = STARTING_CASH - 999.00
    trade_store.record_trade(db_path, bad_trade)

    with pytest.raises(AssertionError):
        _assert_ledger_is_internally_consistent(trade_store.load_trades(db_path))


def test_direct_oversell_row_is_detected_by_integrity_helper(db_path: Path) -> None:
    buy = _trade(side="Buy", ticker="AAA", qty=1, price=100.00, cash_before=STARTING_CASH, shares_before=0)
    trade_store.record_trade(db_path, buy)

    # This row mimics a corrupt/direct DB insert that bypasses app._execute_trade.
    # The database accepts shares_after=0, but the test-side ledger validator catches
    # that selling 2 shares from a 1-share position is impossible.
    oversell = _trade(side="Sell", ticker="AAA", qty=2, price=100.00, cash_before=buy["cash_after"], shares_before=1)
    oversell["shares_after"] = 0
    trade_store.record_trade(db_path, oversell)

    with pytest.raises(AssertionError):
        _assert_ledger_is_internally_consistent(trade_store.load_trades(db_path))


def test_no_margin_rule_is_detected_by_integrity_helper_when_app_is_bypassed(db_path: Path) -> None:
    # The Streamlit commit path blocks this, but direct persistence would not.
    no_margin_violation = _trade(
        side="Buy",
        ticker="AAA",
        qty=2,
        price=60_000.00,
        cash_before=STARTING_CASH,
        shares_before=0,
    )
    trade_store.record_trade(db_path, no_margin_violation)

    with pytest.raises(AssertionError):
        _assert_ledger_is_internally_consistent(trade_store.load_trades(db_path))
