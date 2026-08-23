#!/usr/bin/env python
"""Compute per-record state income tax with PolicyEngine US.

Usage:
    python src/tests/state/cross_model/pe_state_tax.py <in.csv> <out.csv> <year>

Input CSV (one row per record x state; written by cross_model_pe_leg() in
src/tests/state/test_state_cross_model.R):
    rec_id, state, joint, filing_status (1/2/3/4), page, sage, n_dep,
    dep_ages ("5;8" or ""),
    pwages, swages, psemp, ssemp,
    taxable_interest, tax_exempt_interest,
    qualified_dividends, ordinary_dividends, stcg, ltcg,
    pension_income, social_security, unemployment, rental, pass_through,
    estate, misc_income, real_estate_taxes, mortgage_interest,
    charitable_cash, charitable_noncash, childcare_expenses

Output CSV:
    rec_id, state, year, pe_state_income_tax, pe_wa_ltcg_excise, pe_wa_wftc,
    pe_version

Records are batched BATCH_SIZE households per Simulation (vectorized inside
PolicyEngine). Variables are placed on their correct entity by querying the
loaded system, so entity moves across package versions do not silently
misplace inputs.
"""

import csv
import sys

from policyengine_us import Simulation
from policyengine_us.system import system

try:
    from importlib.metadata import version as _pkg_version
    PE_VERSION = _pkg_version("policyengine-us")
except Exception:
    PE_VERSION = "unknown"

BATCH_SIZE = 500

# Our filing-status codes -> PolicyEngine's FilingStatus enum. Every tax unit
# gets one; see the note in build_situation() for why partial assignment is
# unsafe.
FILING_STATUS = {
    1: "SINGLE",
    2: "JOINT",
    3: "SEPARATE",
    4: "HEAD_OF_HOUSEHOLD",
}

# input column -> candidate PolicyEngine variable names (first match wins).
# Person-money columns are placed on the relevant person; group-entity
# variables on that record's instance of the entity.
PERSON_SPLIT_VARS = {
    # (primary column, spouse column) -> variable
    ("pwages", "swages"): ["employment_income"],
    ("psemp", "ssemp"): ["self_employment_income"],
}
PRIMARY_VARS = {
    "taxable_interest": ["taxable_interest_income"],
    "tax_exempt_interest": ["tax_exempt_interest_income"],
    "qualified_dividends": ["qualified_dividend_income"],
    "ordinary_dividends": ["non_qualified_dividend_income"],
    "stcg": ["short_term_capital_gains"],
    "ltcg": ["long_term_capital_gains"],
    "pension_income": ["taxable_pension_income", "pension_income"],
    "social_security": ["social_security"],
    "unemployment": ["unemployment_compensation"],
    "rental": ["rental_income"],
    "pass_through": ["partnership_s_corp_income"],
    "estate": ["estate_income"],
    "misc_income": ["miscellaneous_income"],
    "real_estate_taxes": ["real_estate_taxes"],
    "mortgage_interest": ["deductible_mortgage_interest", "mortgage_interest"],
    "charitable_cash": ["charitable_cash_donations"],
    "charitable_noncash": ["charitable_non_cash_donations"],
    "childcare_expenses": ["tax_unit_childcare_expenses", "childcare_expenses"],
}
# PE's generic state_income_tax INCLUDES local piggyback taxes for some
# states. For MD, with no county input, PE defaults the county to
# first-in-state (Allegany, ~3%), silently adding a county tax to every
# record - so the MD comparison must use PE's state-only variable
# (verified empirically 2026-07-24, see research/raw/md_research_core.md §11).
STATE_ONLY_LIAB_VARS = {
    "MD": "md_income_tax",
}

OUTPUT_VARS = {
    "pe_state_income_tax": ["state_income_tax"],
    "pe_wa_ltcg_excise": ["wa_capital_gains_tax"],
    "pe_wa_wftc": ["wa_working_families_tax_credit"],
    # Diagnostic for the ID exclude KD row: PE prorates the grocery credit
    # by qualified months (zeroing imputed-SNAP months), so the netted
    # amount is partial and household-specific.
    "pe_id_grocery_credit": ["id_grocery_credit"],
    # One-time state rebates PE nets into eligibility-year state_income_tax
    # (KD exclude predicates key on these columns being positive).
    "pe_ny_inflation_refund": ["ny_inflation_refund_credit"],
    "pe_va_rebate": ["va_rebate"],
    "pe_ga_surplus_rebate": ["ga_surplus_tax_rebate"],
    "pe_az_families_rebate": ["az_families_tax_rebate"],
    # WI nets the homestead credit (rent/property-tax based, one-sided for
    # us -- no rent data) into wi_income_tax; exported for the KD predicate.
    "pe_wi_homestead": ["wi_homestead_credit"],
    "pe_ct_rebate": ["ct_child_tax_rebate"],
    "pe_ri_child_rebate": ["ri_child_tax_rebate"],
    # New Mexico ran THREE one-time 2021 rebates and PE nets all three into
    # state_income_tax for TY2021 only. The variables still compute nonzero
    # values in 2022-2024 but are NOT in nm_refundable_credits those years
    # (verified 2026-08-13: PE's nm_refundable_credits equals LICTR alone from
    # 2022), so the KD predicate that uses these columns is scoped to 2021.
    "pe_nm_2021_rebate": ["nm_2021_income_rebate"],
    "pe_nm_2021_addl_rebate": ["nm_additional_2021_income_rebate"],
    "pe_nm_2021_suppl_rebate": ["nm_supplemental_2021_income_rebate"],
    # Five more one-time rebates PE nets into TY2021, found by the 2026-08-22
    # PE-2021 sweep. Each behaves exactly like the NM trio: the variable
    # computes a nonzero value in EVERY year, but is only inside that state's
    # credit total for 2021 (verified 2026-08-22 at 80k single -- HI 300, ME
    # 850, MA 516.35, MT 1,250, SC 800 in 2021 against a zero credit total in
    # 2022-2024), so the KD predicates using these columns are scoped to 2021.
    # MA's is proportional (62F, 14.0312% of TY2021 liability), the rest flat
    # or capped.
    "pe_hi_act115_rebate": ["hi_act_115_rebate"],
    "pe_me_relief_rebate": ["me_relief_rebate"],
    "pe_ma_62f_rebate": ["ma_taxpayer_refund_rebate"],
    "pe_mt_income_rebate": ["mt_income_tax_rebate"],
    "pe_mt_property_rebate": ["mt_property_tax_rebate"],
    "pe_sc_2022_rebate": ["sc_2022_rebate"],
    # PE's own federal results, used by the harness's clean-subset metrics
    "pe_fed_agi": ["adjusted_gross_income"],
    "pe_fed_taxable": ["taxable_income"],
    "pe_fed_eitc": ["eitc", "earned_income_tax_credit"],
}

GROUP_PLURALS = {
    "tax_unit": "tax_units",
    "household": "households",
    "spm_unit": "spm_units",
    "family": "families",
    "marital_unit": "marital_units",
}


def resolve(candidates, required=True):
    """Return (variable_name, entity_key) for the first extant candidate."""
    for name in candidates:
        if name in system.variables:
            return name, system.variables[name].entity.key
    if required:
        sys.exit(f"ERROR: none of {candidates} exist in policyengine-us "
                 f"{PE_VERSION}; update the mapping in src/tests/state/cross_model/pe_state_tax.py")
    return None, None


def build_situation(rows, year):
    """One situation holding len(rows) independent households."""
    yr = str(year)
    situation = {
        "people": {},
        "tax_units": {},
        "households": {},
        "spm_units": {},
        "families": {},
        "marital_units": {},
    }

    for row in rows:
        rid = row["rec_id"]
        joint = int(float(row["joint"])) == 1
        primary = f"p{rid}"
        members = [primary]
        situation["people"][primary] = {"age": {yr: max(0, int(float(row["page"])))}}

        if joint:
            spouse = f"s{rid}"
            sage = int(float(row["sage"]))
            situation["people"][spouse] = {"age": {yr: max(18, sage)}}
            members.append(spouse)

        dep_ages = [a for a in str(row["dep_ages"]).split(";") if a != ""]
        n_dep = int(float(row["n_dep"]))
        ages = [int(float(a)) for a in dep_ages]
        while len(ages) < n_dep:               # >3 deps: repeat last known age
            ages.append(ages[-1] if ages else 10)
        for k, age in enumerate(ages[:n_dep]):
            child = f"c{rid}_{k}"
            situation["people"][child] = {"age": {yr: max(0, age)}}
            members.append(child)

        # Person-level money inputs
        for (pcol, scol), candidates in PERSON_SPLIT_VARS.items():
            var, _ = resolve(candidates)
            situation["people"][primary].setdefault(var, {})[yr] = float(row[pcol])
            if joint:
                situation["people"][spouse].setdefault(var, {})[yr] = float(row[scol])

        group_values = {}
        for col, candidates in PRIMARY_VARS.items():
            var, entity = resolve(candidates)
            val = float(row[col])
            if entity == "person":
                situation["people"][primary].setdefault(var, {})[yr] = val
            else:
                group_values.setdefault(entity, {})[var] = {yr: val}

        # Group entities: one instance of each per record.
        #
        # filing_status is set EXPLICITLY ON EVERY TAX UNIT, and it must stay
        # that way. PolicyEngine normally derives it from the marital unit and
        # dependents, which recovers JOINT and HEAD_OF_HOUSEHOLD but cannot
        # recover married-filing-separately: an MFS record is a one-person tax
        # unit indistinguishable from a single filer, so the formula defaults
        # it to SINGLE. That is only harmless where a state taxes MFS as
        # single, and Wisconsin does not -- its MFS standard deduction has a
        # lower maximum, a much lower phase-out start and a steeper rate.
        #
        # Setting it for the MFS rows ALONE is not a valid fix and was tried:
        # once any tax unit in a Simulation supplies filing_status as an
        # input, PolicyEngine treats the whole vector as input and every unit
        # that did NOT supply one falls back to the default (SINGLE) instead
        # of running the formula. A batch containing one MFS row therefore
        # silently converted its joint and head-of-household rows to single
        # filers -- verified directly: a joint couple in the same batch went
        # from filing_status JOINT, federal taxable 92,300, WI tax 5,036 to
        # SINGLE, 106,150 and 5,883. Supplying every unit's status is what
        # makes the vector well-defined.
        tax_unit_values = dict(group_values.get("tax_unit", {}))
        # int(float(...)) so "3" and "3.0" both resolve; an unmapped code is
        # a crosswalk error and must fail loudly rather than default
        tax_unit_values["filing_status"] = {
            yr: FILING_STATUS[int(float(row["filing_status"]))]}
        situation["tax_units"][f"t{rid}"] = {
            "members": members, **tax_unit_values}
        situation["households"][f"h{rid}"] = {
            "members": members,
            "state_name": {yr: row["state"].upper()},
            **group_values.get("household", {})}
        situation["spm_units"][f"sp{rid}"] = {
            "members": members, **group_values.get("spm_unit", {})}
        situation["families"][f"f{rid}"] = {
            "members": members, **group_values.get("family", {})}

        # Marital units: couple together, each child alone
        situation["marital_units"][f"m{rid}"] = {
            "members": members[:2] if joint else [primary]}
        for k in range(len(ages[:n_dep])):
            situation["marital_units"][f"m{rid}_c{k}"] = {
                "members": [f"c{rid}_{k}"]}

    return situation


def main():
    if len(sys.argv) != 4:
        sys.exit(__doc__)
    in_csv, out_csv, year = sys.argv[1], sys.argv[2], int(sys.argv[3])

    # Fail fast on any unmapped variable before doing work
    for candidates in list(PRIMARY_VARS.values()) + \
            list(PERSON_SPLIT_VARS.values()) + [OUTPUT_VARS["pe_state_income_tax"]]:
        resolve(candidates)
    extra_vars = {
        k: resolve(v, required=False)[0] for k, v in OUTPUT_VARS.items()
        if k != "pe_state_income_tax"
    }
    liab_var = resolve(OUTPUT_VARS["pe_state_income_tax"])[0]

    with open(in_csv, newline="") as f:
        rows = list(csv.DictReader(f))
    print(f"pe_state_tax: {len(rows)} records, year {year}, "
          f"policyengine-us {PE_VERSION}", flush=True)

    results = []
    for start in range(0, len(rows), BATCH_SIZE):
        batch = rows[start:start + BATCH_SIZE]
        situation = build_situation(batch, year)
        sim = Simulation(situation=situation)

        liab = sim.calculate(liab_var, year)
        state_only = {}
        for st_code, var in STATE_ONLY_LIAB_VARS.items():
            if any(r["state"] == st_code for r in batch):
                state_only[st_code] = sim.calculate(var, year)
        extras = {}
        for out_col, var in extra_vars.items():
            extras[out_col] = sim.calculate(var, year) if var else [0.0] * len(batch)

        # tax_units insertion order == batch row order
        for i, row in enumerate(batch):
            liab_i = (state_only[row["state"]][i]
                      if row["state"] in state_only else liab[i])
            out = {
                "rec_id": row["rec_id"],
                "state": row["state"],
                "year": year,
                "pe_state_income_tax": round(float(liab_i), 2),
            }
            for out_col in extra_vars:
                out[out_col] = round(float(extras[out_col][i]), 2)
            out["pe_version"] = PE_VERSION
            results.append(out)
        print(f"  batch {start // BATCH_SIZE + 1}: "
              f"{min(start + BATCH_SIZE, len(rows))}/{len(rows)}", flush=True)

    with open(out_csv, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=list(results[0].keys()))
        writer.writeheader()
        writer.writerows(results)
    print(f"pe_state_tax: wrote {out_csv}", flush=True)


if __name__ == "__main__":
    main()
