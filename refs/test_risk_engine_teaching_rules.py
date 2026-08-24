"""
Pytest tests for teaching/risk rules used before a buy trade.

Run with:

    RISK_ENGINE_PATH=./risk_engine.py pytest -q tests/test_risk_engine_teaching_rules.py
"""

from __future__ import annotations

import importlib.util
import os
from pathlib import Path
from types import ModuleType


def _load_risk_engine() -> ModuleType:
    env_path = os.environ.get("RISK_ENGINE_PATH")
    candidates = []
    if env_path:
        candidates.append(Path(env_path))

    here = Path(__file__).resolve().parent
    candidates.extend(
        [
            here.parent / "risk_engine.py",
            here / "risk_engine.py",
            here / "risk_engine(1).py",
            Path("/mnt/data/risk_engine(1).py"),
        ]
    )

    for candidate in candidates:
        if candidate.exists():
            spec = importlib.util.spec_from_file_location("risk_engine_under_test", candidate)
            assert spec and spec.loader, f"Could not load module spec for {candidate}"
            module = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(module)
            return module

    raise FileNotFoundError(
        "Could not find risk_engine.py. Set RISK_ENGINE_PATH=/path/to/risk_engine.py."
    )


re_ = _load_risk_engine()


THRESHOLDS = {
    "pe_ratio": {
        "label": "P/E Ratio",
        "category": "Valuation",
        "unit": "ratio",
        "direction": "higher_is_riskier",
        "watch": 30.0,
        "risky": 45.0,
        "definition": "price divided by earnings",
        "relevance": "valuation risk",
        "classroom_rule": "higher values are riskier",
    },
    "current_ratio": {
        "label": "Current Ratio",
        "category": "Liquidity",
        "unit": "ratio",
        "direction": "lower_is_riskier",
        "watch": 1.5,
        "risky": 1.0,
        "definition": "current assets divided by current liabilities",
        "relevance": "short-term liquidity",
        "classroom_rule": "lower values are riskier",
    },
    "avg_dollar_volume": {
        "label": "Average Dollar Volume",
        "category": "Liquidity",
        "unit": "currency",
        "direction": "lower_is_riskier",
        "watch": 1_000_000.0,
        "risky": 250_000.0,
        "definition": "daily shares times price",
        "relevance": "exit liquidity",
        "classroom_rule": "thin trading can be risky",
    },
}


def _stock(**kpis):
    return {"ticker": "TST", "price": 100.0, "kpis": kpis}


def test_missing_kpi_is_not_classified_as_safe() -> None:
    assert re_.classify_kpi(None, THRESHOLDS["pe_ratio"]) == re_.STATE_MISSING


def test_higher_is_riskier_classification_edges() -> None:
    row = THRESHOLDS["pe_ratio"]
    assert re_.classify_kpi(30.0, row) == re_.STATE_SAFE
    assert re_.classify_kpi(30.01, row) == re_.STATE_WATCH
    assert re_.classify_kpi(45.0, row) == re_.STATE_WATCH
    assert re_.classify_kpi(45.01, row) == re_.STATE_RISKY


def test_lower_is_riskier_classification_edges() -> None:
    row = THRESHOLDS["current_ratio"]
    assert re_.classify_kpi(1.5, row) == re_.STATE_SAFE
    assert re_.classify_kpi(1.49, row) == re_.STATE_WATCH
    assert re_.classify_kpi(1.0, row) == re_.STATE_WATCH
    assert re_.classify_kpi(0.99, row) == re_.STATE_RISKY


def test_position_cap_flag_uses_resulting_total_position() -> None:
    stock = _stock(pe_ratio=20.0, current_ratio=2.0, avg_dollar_volume=2_000_000.0)
    kpi_results = re_.evaluate_stock_kpis(stock, THRESHOLDS)

    flags = re_.buy_trade_flags(
        stock=stock,
        kpi_results=kpi_results,
        thresholds=THRESHOLDS,
        key_kpis=["pe_ratio"],
        existing_shares=100,
        order_qty=60,
        portfolio_value=100_000.0,
        position_cap_pct=15.0,
    )

    assert any(flag.key == "position_cap" and flag.severity == re_.STATE_RISKY for flag in flags)


def test_key_kpi_risky_and_missing_generate_buy_flags() -> None:
    stock = _stock(pe_ratio=50.0, current_ratio=None, avg_dollar_volume=2_000_000.0)
    kpi_results = re_.evaluate_stock_kpis(stock, THRESHOLDS)

    flags = re_.buy_trade_flags(
        stock=stock,
        kpi_results=kpi_results,
        thresholds=THRESHOLDS,
        key_kpis=["pe_ratio", "current_ratio"],
        existing_shares=0,
        order_qty=1,
        portfolio_value=100_000.0,
        position_cap_pct=15.0,
    )

    by_key = {flag.key: flag for flag in flags}
    assert by_key["pe_ratio"].severity == re_.STATE_RISKY
    assert by_key["current_ratio"].severity == re_.STATE_MISSING


def test_average_dollar_volume_below_watch_generates_trade_flag_without_duplicate_generic_flag() -> None:
    stock = _stock(pe_ratio=20.0, current_ratio=2.0, avg_dollar_volume=900_000.0)
    kpi_results = re_.evaluate_stock_kpis(stock, THRESHOLDS)

    flags = re_.buy_trade_flags(
        stock=stock,
        kpi_results=kpi_results,
        thresholds=THRESHOLDS,
        key_kpis=["pe_ratio"],
        existing_shares=0,
        order_qty=1,
        portfolio_value=100_000.0,
        position_cap_pct=15.0,
    )

    matching = [flag for flag in flags if flag.key == "avg_dollar_volume_watch"]
    duplicate_generic = [flag for flag in flags if flag.key == "avg_dollar_volume"]
    assert len(matching) == 1
    assert matching[0].severity == re_.STATE_WATCH
    assert duplicate_generic == []


def test_order_flags_sorted_places_risky_then_missing_then_watch() -> None:
    flags = [
        re_.Flag(re_.STATE_WATCH, "kpi", "watch", "watch", "detail"),
        re_.Flag(re_.STATE_MISSING, "kpi", "missing", "missing", "detail"),
        re_.Flag(re_.STATE_RISKY, "trade", "risky", "risky", "detail"),
    ]

    sorted_flags = re_.order_flags_sorted(flags)

    assert [flag.severity for flag in sorted_flags] == [
        re_.STATE_RISKY,
        re_.STATE_MISSING,
        re_.STATE_WATCH,
    ]
