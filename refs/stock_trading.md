# Educational Stock Trading Simulator (Risk-First Learning Tool)

## Objective
Build a single-file, browser-based paper-trading simulator that teaches high school students (grades 9–12) to evaluate business and market risk metrics before placing simulated stock trades.

---

## CORE REQUIREMENTS

### 1. KPI Curriculum
Teach and display these metrics with name, value, unit, plain-English definition, relevance, and classroom risk rule:
- **Valuation:** P/E ratio
- **Growth:** Revenue growth, EPS growth
- **Profitability:** Operating margin, Free cash flow
- **Leverage:** Debt-to-equity ratio
- **Liquidity:** Current ratio, Average daily dollar volume
- **Volatility:** Beta, ATR percent, Maximum drawdown

### 2. Risk Classification
Each KPI must be labeled as **Safe | Watch | Risky | Not Enough Data**.
- Missing/invalid/stale data → **Not Enough Data** (never Safe).
- Teacher-editable thresholds determine Watch and Risky boundaries.
- Store directionality: `higher_is_riskier`, `lower_is_riskier`, or `outside_band_is_riskier`.

### 3. Illustrative Classroom Thresholds
*(teacher-editable)*

| KPI | Direction | Watch | Risky |
|-----|-----------|-------|-------|
| P/E ratio | higher_is_riskier | >30 | >45 |
| Debt-to-equity | higher_is_riskier | >1.5 | >2.5 |
| Current ratio | lower_is_riskier | <1.5 | <1.0 |
| Operating margin | lower_is_riskier | <10% | <5% |
| Beta | higher_is_riskier | >1.4 | >1.8 |
| ATR percent | higher_is_riskier | >4% | >6% |
| Max drawdown | higher_is_riskier | >20% | >30% |

For growth/cash-flow metrics, require teacher to set context-specific thresholds.

### 4. Risk-Margin Calculation
*(displayed next to every KPI)*

- Show raw gap to Risky boundary in native units.
- Show percentage margin when boundary ≠ 0:
  - **higher_is_riskier:** `((value − risky_threshold) / |risky_threshold|) × 100`
  - **lower_is_riskier:** `((risky_threshold − value) / |risky_threshold|) × 100`
  - **outside_band_is_riskier:** distance beyond nearest bound / |bound| × 100
- If not yet Risky, label as "distance to Risky boundary."
- If threshold is zero or undefined, show native-unit gap and "percentage margin not available."
- Example: *"Debt-to-equity = 2.8; Risky boundary = 2.5; 0.3 above; 12% beyond boundary."*

### 5. Trade-Level Risk Rules
Flag a simulated order if:
- New position would exceed **15% of portfolio value**.
- Any selected KPI is **Risky**.
- Key KPI data is **missing**.
- Average daily dollar volume is very low.

Show **all flags separately**—do not aggregate into a single score.

### 6. Trade Risk Gate
Before submission:
- Run all KPI and trade-level checks.
- Display **Risky** flags first, then **Watch**.
- For each flag: show KPI name, value, threshold/range, margin, and which rule triggered.
- Require acknowledgment before allowing paper trade.
- If no rule triggers: say *"No configured risk rule triggered"* (never "This trade is safe").
- Never predict profit or imply green status removes market risk.

### 7. Virtual Portfolio Rules
- Start: **$100,000** virtual cash.
- Whole shares only; no margin, options, shorts, or real money.
- Max **15% of portfolio** in one stock.

---

## UI COMPONENTS

### Required Screens
1. **Student Dashboard** – portfolio summary, open positions, cash balance.
2. **Stock Selector** – search/browse fictional companies.
3. **KPI Cards** – each shows Safe/Watch/Risky/Not Enough Data, definition, tooltip, risk margin.
4. **KPI Summary Table** – all metrics in one view with threshold-margin column.
5. **Trade Ticket** – quantity input, preview order value vs. portfolio %, risk pre-check.
6. **Pre-Trade Risk Summary** – all flags with explanations and acknowledgment checkbox.
7. **Post-Trade Reflection** – optional prompt asking student to explain their decision.
8. **Teacher Settings** – edit all KPI thresholds, directionality, and trade-level rules.

### UX Principles
- Plain English, short definitions, visual risk bands, tooltips.
- Responsive layout, keyboard-accessible controls, readable text (≥14px body).
- Clear visual legend: Safe (green), Watch (yellow), Risky (red), Not Enough Data (gray).
- One reflection question before each trade.

---

## DATA MODE

Use **built-in fictional/sample companies** with simulated static data embedded in the HTML file.
- Include ~5–10 sample stocks covering a range of Safe, Watch, Risky, and Not Enough Data scenarios.
- No network calls, APIs, or external libraries.

---

## SAFETY & COMPLIANCE

### Educational Disclaimers
- This is a **paper-trading simulator**, not a brokerage.
- No real-money execution, no personalized financial advice, no buy/sell recommendations.
- All thresholds are **classroom heuristics**, not universal investment rules.
- Explain that industries have different normal KPI ranges; one metric alone does not determine trade quality.

### Data Integrity
- Never convert missing/invalid data to Safe.
- Visibly show "Not Enough Data" and explain why.

### Accessibility
- Keyboard navigation, ARIA labels, color contrast ≥ WCAG AA.

---

## ACCEPTANCE CRITERIA

The app must:
- ✅ Work end-to-end as a paper-trading learning experience.
- ✅ Correctly apply KPI directionality and teacher-editable thresholds.
- ✅ Calculate and display margins to Risky boundaries with plain-English explanations.
- ✅ Flag trades violating KPI or trade-level rules, showing exact reason and evidence.
- ✅ Never silently treat missing data as Safe; never declare a trade "safe."
- ✅ Provide a teacher settings panel for threshold editing.
- ✅ Use responsive, readable UI with clear Safe/Watch/Risky/Not Enough Data states.
- ✅ Include embedded sample dataset for offline testing.
- ✅ Include test scenarios: low-risk, Watch, Risky, missing-data, and boundary cases.

---

## IMPLEMENTATION CONSTRAINT

**Single self-contained HTML file.**
- No backend, server, API, external libraries, CDNs, or network calls after page load.
- All data, logic, CSS, and JavaScript embedded in one `.html` file.
- Works offline in any modern browser (Chrome, Firefox, Safari, Edge).

---

## DELIVERABLES

1. **Complete HTML file** implementing all requirements above.
2. **Concise documentation** (~200 words) explaining:
   - Risk-rule data structure (how thresholds and directionality are stored).
   - Margin-to-Risky-boundary formulas for each directionality type.
   - Sample data structure for fictional stocks.
