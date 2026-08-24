"""
Educational Stock Trading Simulator (Risk-First Learning Tool) — v2
Streamlit implementation.

IMPORTANT ABOUT PERSISTENCE
----------------------------
Buy/sell transactions are written to a local SQLite database named
trades.sqlite3. On startup, the app loads the persisted trade ledger and
rebuilds cash, holdings, and dashboard trade history from that database.
Use the "Reset Simulation" button in the sidebar to clear persisted trades
and return to the $100,000 starting state.
"""

import copy
from datetime import datetime
from pathlib import Path

import streamlit as st

from data import DEFAULT_THRESHOLDS, DEFAULT_KEY_KPIS, SAMPLE_STOCKS
import risk_engine as re_
from trade_store import clear_trades, init_db, load_trades, rebuild_portfolio, record_trade

APP_DIR = Path(__file__).resolve().parent
DB_PATH = APP_DIR / "trades.sqlite3"
STARTING_CASH = 100_000.0
POSITION_CAP_PCT = 15.0

st.set_page_config(page_title="Stock Trading Simulator", page_icon="📈", layout="wide")

# ---------------------------------------------------------------------------
# Session state initialization
# ---------------------------------------------------------------------------

def refresh_portfolio_from_db() -> None:
    """Load the durable trade ledger and rebuild Streamlit's visible state."""
    ss = st.session_state
    trades = load_trades(ss.db_path)
    ss.trade_history = trades
    ss.cash, ss.holdings = rebuild_portfolio(trades, STARTING_CASH)
    ss.last_transaction = trades[-1] if trades else None


def init_state():
    ss = st.session_state
    if "initialized" in ss:
        return

    ss.initialized = True
    ss.db_path = DB_PATH
    init_db(ss.db_path)

    # Trade-dependent state comes from SQLite, not from a fresh in-memory list.
    refresh_portfolio_from_db()

    ss.thresholds = copy.deepcopy(DEFAULT_THRESHOLDS)
    ss.key_kpis = list(DEFAULT_KEY_KPIS)
    ss.position_cap_pct = POSITION_CAP_PCT
    ss.selected_ticker = None
    ss.trade_stage = "ticket"      # "ticket" | "risk_gate"
    ss.trade_side = "Buy"
    ss.trade_qty = 1
    ss.reflection_text = ""
    ss.trade_form_id = 0
    ss.nav = "Dashboard"
    ss.nav_radio = "Dashboard"
    ss.pending_nav = None


def reset_simulation():
    # Clear the durable ledger first; then clear Streamlit state. The following
    # st.rerun() starts a clean run where init_state() reloads an empty ledger.
    clear_trades(st.session_state.get("db_path", DB_PATH))
    for key in list(st.session_state.keys()):
        del st.session_state[key]


init_state()

STOCKS_BY_TICKER = {s["ticker"]: s for s in SAMPLE_STOCKS}


def queue_navigation(page: str):
    """Request a page change after the next rerun without mutating a rendered widget key."""
    st.session_state.nav = page
    st.session_state.pending_nav = page


def _sync_nav_from_sidebar():
    """Keep the app page in sync when the student uses the sidebar radio."""
    st.session_state.nav = st.session_state.nav_radio
    st.session_state.pending_nav = None


def portfolio_value():
    ss = st.session_state
    total = ss.cash
    for ticker, shares in ss.holdings.items():
        stock = STOCKS_BY_TICKER.get(ticker)
        if stock and shares:
            total += shares * stock["price"]
    return total


# ---------------------------------------------------------------------------
# Shared UI helpers
# ---------------------------------------------------------------------------

def risk_badge(state: str) -> str:
    meta = re_.STATE_META[state]
    return (
        f'<span style="background:{meta["bg"]};color:{meta["color"]};'
        f'border:1px solid {meta["color"]}33;border-radius:6px;padding:2px 8px;'
        f'font-weight:600;font-size:0.85rem;white-space:nowrap;">'
        f'{meta["icon"]} {meta["label"]}</span>'
    )


def legend():
    st.markdown(
        " &nbsp; ".join(risk_badge(s) for s in [re_.STATE_SAFE, re_.STATE_WATCH, re_.STATE_RISKY, re_.STATE_MISSING]),
        unsafe_allow_html=True,
    )
    st.caption("Color is never the only signal — every state also shows an icon and text label.")


def disclaimer_banner():
    st.info(
        "📘 **Paper-trading simulator for classroom learning.** Not a brokerage, not "
        "financial advice, and not a buy/sell recommendation. Thresholds are one "
        "uniform set of classroom heuristics applied across all sectors — they don't "
        "auto-adjust for industry norms. A metric flagged Watch or Risky may be normal "
        "for that company's sector. No single metric should be read as a verdict on its "
        "own — use the KPI Summary Table as a whole."
    )


# ---------------------------------------------------------------------------
# Screen: Student Dashboard
# ---------------------------------------------------------------------------

def screen_dashboard():
    st.header("Student Dashboard")
    disclaimer_banner()

    pv = portfolio_value()
    last_tx = st.session_state.get("last_transaction")
    if last_tx:
        st.success(
            f"Last transaction recorded: {last_tx['side']} {last_tx['qty']} share(s) "
            f"of {last_tx['ticker']} at {re_.format_value(last_tx['price'], 'currency')}. "
            "The cash balance, open positions, and trade history below now reflect this trade."
        )

    c1, c2, c3 = st.columns(3)
    c1.metric("Cash Balance", re_.format_value(st.session_state.cash, "currency"))
    c2.metric("Total Portfolio Value", re_.format_value(pv, "currency"))
    c3.metric("Open Positions", str(len([t for t, s in st.session_state.holdings.items() if s > 0])))

    st.subheader("Open Positions")
    rows = []
    for ticker, shares in st.session_state.holdings.items():
        if shares <= 0:
            continue
        stock = STOCKS_BY_TICKER[ticker]
        mkt_val = shares * stock["price"]
        pct = (mkt_val / pv * 100) if pv else 0
        rows.append({
            "Ticker": ticker, "Name": stock["name"], "Sector": stock["sector"],
            "Shares": shares, "Price": re_.format_value(stock["price"], "currency"),
            "Market Value": re_.format_value(mkt_val, "currency"),
            "% of Portfolio": f"{pct:.1f}%",
        })
    if rows:
        st.dataframe(rows, width='stretch', hide_index=True)
    else:
        st.caption("No open positions yet. Visit the Stock Selector to research and place a trade.")

    st.subheader("Recent Trade History")
    if st.session_state.trade_history:
        hist_rows = [
            {
                "Time": h["time"], "Side": h["side"], "Ticker": h["ticker"],
                "Qty": h["qty"], "Price": re_.format_value(h["price"], "currency"),
                "Value": re_.format_value(h["value"], "currency"),
                "Shares After": h.get("shares_after", ""),
                "Cash After": re_.format_value(h["cash_after"], "currency") if "cash_after" in h else "",
            }
            for h in reversed(st.session_state.trade_history)
        ]
        st.dataframe(hist_rows, width='stretch', hide_index=True)
    else:
        st.caption("No trades placed yet.")


# ---------------------------------------------------------------------------
# Screen: Stock Selector
# ---------------------------------------------------------------------------

def screen_selector():
    st.header("Stock Selector")
    disclaimer_banner()

    sectors = sorted({s["sector"] for s in SAMPLE_STOCKS})
    col1, col2 = st.columns([2, 1])
    with col1:
        query = st.text_input("Search by ticker or name", "")
    with col2:
        sector_filter = st.selectbox("Filter by sector", ["All"] + sectors)

    filtered = SAMPLE_STOCKS
    if query:
        q = query.strip().lower()
        filtered = [s for s in filtered if q in s["ticker"].lower() or q in s["name"].lower()]
    if sector_filter != "All":
        filtered = [s for s in filtered if s["sector"] == sector_filter]

    if not filtered:
        st.warning("No stocks match your search.")
        return

    for stock in filtered:
        kpi_results = re_.evaluate_stock_kpis(stock, st.session_state.thresholds)
        worst = _worst_state(kpi_results)
        with st.container(border=True):
            c1, c2, c3 = st.columns([3, 2, 1])
            with c1:
                st.markdown(f"**{stock['ticker']}** — {stock['name']}")
                st.caption(f"Sector: {stock['sector']}  ·  Price: {re_.format_value(stock['price'], 'currency')}")
            with c2:
                st.markdown(risk_badge(worst) + " overall snapshot*", unsafe_allow_html=True)
                st.caption("*Worst individual KPI state shown here — see stock page for the full picture.")
            with c3:
                if st.button("View / Trade", key=f"view_{stock['ticker']}"):
                    st.session_state.selected_ticker = stock["ticker"]
                    st.session_state.trade_stage = "ticket"
                    queue_navigation("Stock Detail")
                    st.rerun()


def _worst_state(kpi_results):
    order = [re_.STATE_RISKY, re_.STATE_MISSING, re_.STATE_WATCH, re_.STATE_SAFE]
    states = {r["state"] for r in kpi_results.values()}
    for s in order:
        if s in states:
            return s
    return re_.STATE_SAFE


# ---------------------------------------------------------------------------
# Screen: Stock Detail (KPI Cards + Summary Table + Trade Ticket)
# ---------------------------------------------------------------------------

def screen_stock_detail():
    ticker = st.session_state.selected_ticker
    if not ticker or ticker not in STOCKS_BY_TICKER:
        st.warning("Select a stock from the Stock Selector first.")
        return
    stock = STOCKS_BY_TICKER[ticker]
    thresholds = st.session_state.thresholds
    kpi_results = re_.evaluate_stock_kpis(stock, thresholds)

    if st.button("← Back to Stock Selector"):
        st.session_state.trade_stage = "ticket"
        queue_navigation("Stock Selector")
        st.rerun()

    st.header(f"{stock['ticker']} — {stock['name']}")
    st.caption(f"Sector: {stock['sector']}  ·  Current Price: {re_.format_value(stock['price'], 'currency')}")

    if stock.get("note"):
        st.warning(f"📝 **Teaching / context note:** {stock['note']}")

    legend()

    st.subheader("KPI Cards")
    cards = list(kpi_results.items())
    cols_per_row = 3
    for i in range(0, len(cards), cols_per_row):
        row_cards = cards[i:i + cols_per_row]
        cols = st.columns(cols_per_row)
        for col, (key, res) in zip(cols, row_cards):
            row = res["row"]
            with col:
                with st.container(border=True):
                    st.markdown(f"**{row['label']}**  \n_{row['category']}_")
                    st.markdown(risk_badge(res["state"]), unsafe_allow_html=True)
                    st.markdown(f"**Value:** {re_.format_value(res['value'], row['unit'])}")
                    with st.expander("Definition, relevance & margin"):
                        st.write(f"**What it means:** {row['definition']}")
                        st.write(f"**Why it matters:** {row['relevance']}")
                        st.write(f"**Classroom rule:** {row['classroom_rule']}")
                        st.write(f"**Margin:** {res['margin'].text}")

    st.subheader("KPI Summary Table")
    table_rows = []
    for key, res in kpi_results.items():
        row = res["row"]
        table_rows.append({
            "KPI": row["label"],
            "Category": row["category"],
            "Value": re_.format_value(res["value"], row["unit"]),
            "State": re_.STATE_META[res["state"]]["icon"] + " " + re_.STATE_META[res["state"]]["label"],
            "Watch Threshold": _fmt_threshold(row["watch"], row["unit"]),
            "Risky Threshold": _fmt_threshold(row["risky"], row["unit"]),
            "Margin to Risky Boundary": res["margin"].text,
            "Key KPI?": "Yes" if key in st.session_state.key_kpis else "",
        })
    st.dataframe(table_rows, width='stretch', hide_index=True)

    st.divider()
    screen_trade_ticket(stock, kpi_results)


def _fmt_threshold(v, unit):
    if isinstance(v, tuple):
        return f"{re_.format_value(v[0], unit)} – {re_.format_value(v[1], unit)}"
    return re_.format_value(v, unit)


# ---------------------------------------------------------------------------
# Trade Ticket + Pre-Trade Risk Summary (buy) / Lighter Sell Flow
# ---------------------------------------------------------------------------

def screen_trade_ticket(stock, kpi_results):
    st.subheader("Trade Ticket")
    ss = st.session_state
    ticker = stock["ticker"]
    existing_shares = ss.holdings.get(ticker, 0)
    pv = portfolio_value()

    if ss.trade_stage == "risk_gate":
        _render_risk_gate(stock, kpi_results, existing_shares, pv)
        return

    c1, c2 = st.columns(2)
    with c1:
        side = st.radio(
            "Order type", ["Buy", "Sell"], horizontal=True,
            key=f"trade_side_radio_{ss.trade_form_id}",
            index=0 if ss.trade_side == "Buy" else 1,
        )
        ss.trade_side = side
    with c2:
        max_qty = 100000 if side == "Buy" else existing_shares
        qty = st.number_input(
            "Quantity (whole shares)", min_value=0, max_value=max(max_qty, 0),
            value=min(ss.trade_qty, max(max_qty, 0)) if max_qty else 0,
            step=1, key=f"trade_qty_input_{ss.trade_form_id}",
        )
        ss.trade_qty = qty

    st.caption(f"You currently own **{existing_shares}** shares of {ticker}.")

    order_value = qty * stock["price"]
    resulting_shares = existing_shares + qty if side == "Buy" else existing_shares - qty
    resulting_value = max(resulting_shares, 0) * stock["price"]
    pct_of_portfolio = (resulting_value / pv * 100) if pv else 0

    p1, p2, p3 = st.columns(3)
    p1.metric("Order Value", re_.format_value(order_value, "currency"))
    p2.metric(f"Resulting Position ({side})", f"{max(resulting_shares,0)} sh")
    p3.metric("Resulting % of Portfolio", f"{pct_of_portfolio:.1f}%")

    errors = []
    if qty <= 0:
        errors.append("Enter a quantity greater than zero.")
    if side == "Buy" and order_value > ss.cash:
        errors.append(
            f"Insufficient cash: order value {re_.format_value(order_value, 'currency')} exceeds "
            f"available cash {re_.format_value(ss.cash, 'currency')}. No margin is allowed."
        )
    if side == "Sell" and qty > existing_shares:
        errors.append(f"You only own {existing_shares} shares — cannot sell {qty}.")

    for e in errors:
        st.error(e)

    st.caption(
        "This preview does not predict profit and a green preview never means the market risk "
        "goes away — continue to see the full risk check."
    )

    if st.button("Continue to Pre-Trade Risk Summary", type="primary", disabled=bool(errors)):
        ss.trade_stage = "risk_gate"
        ss.reflection_text = ""
        st.rerun()


def _render_risk_gate(stock, kpi_results, existing_shares, pv):
    ss = st.session_state
    side = ss.trade_side
    qty = ss.trade_qty
    ticker = stock["ticker"]

    st.markdown("### Pre-Trade Risk Summary")

    if side == "Buy":
        flags = re_.buy_trade_flags(
            stock, kpi_results, ss.thresholds, ss.key_kpis,
            existing_shares, qty, pv, ss.position_cap_pct,
        )
        flags = re_.order_flags_sorted(flags)

        if flags:
            st.markdown(f"**{len(flags)} flag(s) triggered.** Each is shown separately below — flags are never combined into a single score.")
            for f in flags:
                icon = re_.STATE_META[f.severity]["icon"]
                label_txt = re_.STATE_META[f.severity]["label"]
                with st.container(border=True):
                    st.markdown(risk_badge(f.severity) + f"  **{f.label}**", unsafe_allow_html=True)
                    st.write(f.detail)
        else:
            st.success("No configured risk rule triggered.")
            st.caption("This does not mean the trade is safe — it means no configured rule fired. Market risk is never fully removed.")
    else:
        st.caption("Selling reduces market exposure, so the 15% cap and KPI-risk flags do not block a sell.")
        st.write(f"You are selling **{qty}** of your **{existing_shares}** shares of {ticker}.")
        if qty > existing_shares:
            st.error(f"You only own {existing_shares} shares — cannot sell {qty}.")
        else:
            st.success("Share-count check passed.")
        st.caption("For context, here is the current KPI Summary for this stock:")
        table_rows = []
        for key, res in kpi_results.items():
            row = res["row"]
            table_rows.append({
                "KPI": row["label"],
                "Value": re_.format_value(res["value"], row["unit"]),
                "State": re_.STATE_META[res["state"]]["icon"] + " " + re_.STATE_META[res["state"]]["label"],
            })
        st.dataframe(table_rows, width='stretch', hide_index=True)

    st.markdown("---")
    st.markdown("**Required reflection** (one response, required for every trade):")
    reflection = st.text_area(
        "Why are you making this trade, and what risk(s) above are you accepting?",
        value=ss.reflection_text, key=f"reflection_input_{ss.trade_form_id}", height=100,
    )
    ss.reflection_text = reflection

    reflection_ok = len(reflection.strip()) > 0
    if not reflection_ok:
        st.caption("Enter a reflection response to enable the acknowledgment checkbox.")

    ack_label = (
        "I have reviewed all flags shown above and accept the associated risks."
        if side == "Buy" else
        "I confirm I want to place this sell order."
    )
    ack = st.checkbox(ack_label, key=f"ack_checkbox_{ss.trade_form_id}", disabled=not reflection_ok)

    can_submit = reflection_ok and ack
    if side == "Sell" and qty > existing_shares:
        can_submit = False
    if side == "Buy" and qty <= 0:
        can_submit = False

    b1, b2 = st.columns(2)
    with b1:
        if st.button("← Back to Trade Ticket"):
            ss.trade_stage = "ticket"
            st.rerun()
    with b2:
        if st.button("Submit Paper Trade", type="primary", disabled=not can_submit):
            if _execute_trade(stock, side, qty):
                st.rerun()

    st.caption(
        "This tool never predicts profit and never implies that a green status removes market risk."
    )


def _execute_trade(stock, side, qty):
    """Persist a paper trade to SQLite and route back to the Dashboard.

    Returns True when the transaction is recorded. Returns False and shows an
    error when a defensive validation fails. The UI already validates these
    cases, but the commit path re-checks them so buy and sell state cannot drift
    if a stale widget submits.
    """
    ss = st.session_state
    ticker = stock["ticker"]
    price = stock["price"]

    try:
        qty = int(qty)
    except (TypeError, ValueError):
        st.error("Quantity must be a whole number.")
        return False

    if qty <= 0:
        st.error("Enter a quantity greater than zero.")
        return False

    if side not in {"Buy", "Sell"}:
        st.error("Order type must be Buy or Sell.")
        return False

    # Re-read the durable ledger immediately before commit so validation uses
    # the latest persisted cash and holdings.
    refresh_portfolio_from_db()

    value = qty * price
    cash_before = float(ss.cash)
    current_shares = int(ss.holdings.get(ticker, 0))

    if side == "Buy" and value > cash_before:
        st.error(
            f"Insufficient cash: order value {re_.format_value(value, 'currency')} exceeds "
            f"available cash {re_.format_value(cash_before, 'currency')}. No margin is allowed."
        )
        return False

    if side == "Sell" and qty > current_shares:
        st.error(f"You only own {current_shares} shares — cannot sell {qty}.")
        return False

    if side == "Buy":
        cash_after = cash_before - value
        shares_after = current_shares + qty
    else:
        cash_after = cash_before + value
        shares_after = current_shares - qty

    transaction = {
        "time": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        "side": side,
        "ticker": ticker,
        "qty": qty,
        "price": price,
        "value": value,
        "shares_before": current_shares,
        "shares_after": shares_after,
        "cash_before": cash_before,
        "cash_after": cash_after,
        "reflection": ss.reflection_text.strip(),
    }

    try:
        transaction["id"] = record_trade(ss.db_path, transaction)
    except Exception as exc:
        st.error(f"Trade was not recorded because SQLite returned an error: {exc}")
        return False

    # Do not use ss.trade_history.append(...). SQLite is the source of truth;
    # reload from the database so the dashboard, cash, and holdings match disk.
    refresh_portfolio_from_db()
    ss.last_transaction = transaction

    # Reset the staged trade form so the next order cannot inherit a stale
    # reflection or checked acknowledgment from the just-submitted order.
    ss.trade_stage = "ticket"
    ss.trade_qty = 1
    ss.reflection_text = ""
    ss.trade_form_id += 1

    queue_navigation("Dashboard")
    return True


# ---------------------------------------------------------------------------
# Screen: Teacher Settings
# ---------------------------------------------------------------------------

def screen_teacher_settings():
    st.header("Teacher Settings")
    st.caption("Edit thresholds, directionality reference, key-KPI list, and trade-level rule values. Changes apply immediately for the rest of this session.")

    ss = st.session_state

    st.subheader("Trade-Level Rules")
    ss.position_cap_pct = st.number_input(
        "Maximum % of portfolio in one stock (checked against total resulting position)",
        min_value=1.0, max_value=100.0, value=float(ss.position_cap_pct), step=0.5,
    )

    st.subheader("Key KPIs (can block a buy trade if Risky or missing)")
    all_keys = list(ss.thresholds.keys())
    cols = st.columns(3)
    new_key_kpis = []
    for i, key in enumerate(all_keys):
        with cols[i % 3]:
            checked = st.checkbox(ss.thresholds[key]["label"], value=key in ss.key_kpis, key=f"keykpi_{key}")
            if checked:
                new_key_kpis.append(key)
    ss.key_kpis = new_key_kpis

    st.subheader("KPI Thresholds")
    st.caption("Direction is fixed per KPI definition (higher_is_riskier / lower_is_riskier); Watch and Risky boundaries are editable.")
    for key, row in ss.thresholds.items():
        with st.expander(f"{row['label']}  ({row['category']}, direction: {row['direction']})"):
            unit = row["unit"]
            step = 0.1 if unit != "currency" else 1000.0
            c1, c2 = st.columns(2)
            with c1:
                new_watch = st.number_input(
                    f"Watch threshold ({unit})", value=float(row["watch"]), step=step, key=f"watch_{key}",
                )
            with c2:
                new_risky = st.number_input(
                    f"Risky threshold ({unit})", value=float(row["risky"]), step=step, key=f"risky_{key}",
                )
            row["watch"] = new_watch
            row["risky"] = new_risky

    if st.button("Reset all thresholds & rules to shipped defaults"):
        ss.thresholds = copy.deepcopy(DEFAULT_THRESHOLDS)
        ss.key_kpis = list(DEFAULT_KEY_KPIS)
        ss.position_cap_pct = POSITION_CAP_PCT
        st.rerun()


# ---------------------------------------------------------------------------
# Navigation
# ---------------------------------------------------------------------------

CUSTOM_CSS = """
<style>
html, body, [class*="css"] { font-size: 16px; }
p, li, span, div { font-size: 1rem; }
.stButton>button:focus, .stCheckbox>label:focus-within, .stRadio>label:focus-within {
    outline: 3px solid #1a73e8 !important; outline-offset: 2px;
}
.stDataFrame { font-size: 0.95rem; }
</style>
"""


def main():
    st.markdown(CUSTOM_CSS, unsafe_allow_html=True)
    st.sidebar.title("📈 Stock Trading Simulator")
    st.sidebar.caption("Educational paper-trading tool — not real money, not financial advice.")

    pages = ["Dashboard", "Stock Selector", "Stock Detail", "Teacher Settings"]
    if st.session_state.nav not in pages:
        st.session_state.nav = "Dashboard"

    pending_nav = st.session_state.get("pending_nav")
    if pending_nav in pages:
        # This runs before the sidebar radio is created, so it is safe to sync
        # the radio widget to a page selected by a button or completed trade.
        st.session_state.nav = pending_nav
        st.session_state.nav_radio = pending_nav
        st.session_state.pending_nav = None
    elif st.session_state.get("nav_radio") not in pages:
        st.session_state.nav_radio = st.session_state.nav

    choice = st.sidebar.radio("Go to", pages, key="nav_radio", on_change=_sync_nav_from_sidebar)
    st.session_state.nav = choice

    st.sidebar.markdown("---")
    pv = portfolio_value()
    st.sidebar.metric("Portfolio Value", re_.format_value(pv, "currency"))
    st.sidebar.caption("SQLite-backed: trades persist in trades.sqlite3 until you click Reset Simulation.")
    if st.sidebar.button("🔄 Reset Simulation"):
        reset_simulation()
        st.rerun()

    if st.session_state.nav == "Dashboard":
        screen_dashboard()
    elif st.session_state.nav == "Stock Selector":
        screen_selector()
    elif st.session_state.nav == "Stock Detail":
        screen_stock_detail()
    elif st.session_state.nav == "Teacher Settings":
        screen_teacher_settings()


if __name__ == "__main__":
    main()
