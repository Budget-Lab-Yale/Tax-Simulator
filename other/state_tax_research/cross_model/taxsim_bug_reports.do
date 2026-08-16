*------------------------------------------------------------------------------
* taxsim_bug_reports.do
*
* Operationalizes NBER TAXSIM-35 bug reporting for the probe-verified TAXSIM
* issues documented in external_model_issues.md (T6-T15). The TAXSIM-35 page
* (taxsim.nber.org/taxsim35, "Bug Reporting") asks that a suspected error be
* reported as:
*
*   "extract that case (or an exemplar, if there are many cases) ... set the
*    case ID (variable 1) to -1 and email the output web page to me (as an
*    ascii attachment) with a statement of what you think is wrong. In Stata
*    also set variable idtl to 5. ... Just send an email with one or two
*    cases, the TAXSIM response and the reason you believe the response to be
*    in error."
*
* For EACH issue this do-file writes, under bug_reports/:
*   1. <tag>_webtool_input.csv : one exemplar case with taxsimid = -1 and
*      idtl = 5, ready for the Internet TAXSIM web form or the email bot
*      (mail to taxsim@nber.org with the attachment named txpydata.csv);
*      the returned full-text calculation page is the ascii attachment the
*      protocol asks for
*   2. <tag>_response.csv      : TAXSIM's numeric v1-v45 response, fetched
*      here via the taxsim35 ado (requires internet; see install note below)
*   3. <tag>_statement.txt     : the "what we think is wrong" statement, with
*      the form/statute citation and the expected value
*
* Usage (Yale HPC):
*   module load Stata/19
*   cd <repo root>
*   stata-mp -b do other/state_tax_research/cross_model/taxsim_bug_reports.do
*   (batch mode returns 0 even on error -- check the .log)
*
* The ado submits data to the NBER server. Nothing here is confidential:
* every observation is synthetic. The EMAIL itself stays manual -- attach
* <tag>_statement.txt, the web-tool text response, and <tag>_response.csv
* to a message to feenberg@nber.org, one issue per message.
*
* One-time ado install (needs internet, e.g. the login node):
*   net from "https://taxsim.nber.org/stata"
*   net install taxsim35
*------------------------------------------------------------------------------

clear all
set more off

local out_dir "other/state_tax_research/cross_model/bug_reports"
capture mkdir "`out_dir'"

* TAXSIM SOI state codes used below (taxsim.nber.org/statesoi.html):
* DE = 8, DC = 9, MD = 21, MI = 23, OK = 37, UT = 45, WI = 50

capture which taxsim35
if _rc {
    display as error "taxsim35 ado not installed. Run:"
    display as error `"  net from "https://taxsim.nber.org/stata""'
    display as error "  net install taxsim35"
    exit 111
}

*------------------------------------------------------------------------------
* submit_case: export the web-tool input, fetch the ado response, and start
* the statement file. The case must be in memory as a single observation with
* TAXSIM input variables; taxsimid/idtl are set here.
*------------------------------------------------------------------------------
capture program drop submit_case
program define submit_case
    syntax, tag(string) out_dir(string)

    * Web-tool / email-bot version: case ID -1 and full text output, per the
    * TAXSIM-35 bug-reporting instructions
    preserve
        gen taxsimid = -1
        gen idtl     = 5
        order taxsimid
        export delimited using "`out_dir'/`tag'_webtool_input.csv", replace
    restore

    * Ado version: numeric v1-v45 response for the statement's actual-value
    * lines (idtl omitted so the ado can merge the response)
    preserve
        gen taxsimid = 1
        taxsim35, replace full
        export delimited using "`out_dir'/`tag'_response.csv", replace
    restore
end

*------------------------------------------------------------------------------
* write_statement: append one line to the issue's statement file
*------------------------------------------------------------------------------
capture program drop open_statement
program define open_statement
    syntax, tag(string) out_dir(string)
    capture file close stmt_fh
    file open stmt_fh using "`out_dir'/`tag'_statement.txt", write replace
end

capture program drop stmt
program define stmt
    syntax, line(string asis)
    file write stmt_fh `line' _n
end

capture program drop close_statement
program define close_statement
    file close stmt_fh
end

*==============================================================================
* T6. Michigan: home-heating credit netted into liability on a collapsed
*     household-income base (zero-income exemplar returns siitax = -386)
*==============================================================================
clear
set obs 1
gen year   = 2019
gen state  = 23
gen mstat  = 1
gen page   = 40
submit_case, tag(t6_mi_home_heating) out_dir("`out_dir'")

open_statement, tag(t6_mi_home_heating) out_dir("`out_dir'")
stmt, line("TAXSIM-35, Michigan, TY2019, single, age 40, no income of any kind.")
stmt, line("")
stmt, line("Expected: MI-1040 liability 0. TAXSIM returns siitax = -385.60 --")
stmt, line("v30_state_household_income collapses to 0.01 here (1.01 on")
stmt, line("populated records in our samples) and the MI-1040CR-7")
stmt, line("home heating credit standard allowance (90% x one-exemption")
stmt, line("allowance, 386 in 2019) is netted into siitax. Two concerns:")
stmt, line("(a) the household-income base of 1.01 is wrong (it also appears on")
stmt, line("multi-million-AGI records, which then receive the credit);")
stmt, line("(b) the home heating credit is an energy-assistance transfer paid")
stmt, line("outside MI-1040 liability, so netting it into siitax mixes concepts.")
stmt, line("On our validation sample ~370-410 records/yr are affected 2017-2020")
stmt, line("(flat 349/351/386/418 by year).")
close_statement

*==============================================================================
* T7. Utah: retirement credit paid to any Social Security recipient,
*     ignoring the born-before-1953 gate and the 2.5c/$ MAGI phase-out
*==============================================================================
clear
set obs 1
gen year   = 2019
gen state  = 45
gen mstat  = 1
gen page   = 40
gen pwages = 2000000
gen gssi   = 20000
submit_case, tag(t7_ut_retirement_credit) out_dir("`out_dir'")

open_statement, tag(t7_ut_retirement_credit) out_dir("`out_dir'")
stmt, line("TAXSIM-35, Utah, TY2019, single, age 40, wages 2,000,000, gross")
stmt, line("Social Security 20,000.")
stmt, line("")
stmt, line("Expected: no Utah retirement credit. Utah Code 59-10-1019 restricts")
stmt, line("the credit to filers born before 1953 and phases it out at 2.5")
stmt, line("cents per dollar of modified AGI above the threshold, so a")
stmt, line("40-year-old at 2M of income qualifies for nothing. TAXSIM grants a")
stmt, line("flat 288 (= 6% x 4,800; 576 per couple; 271 under the 2017 vintage")
stmt, line("constant) to ANY record with positive gssi, at any age and any")
stmt, line("income -- verified across seven probe shapes. Dominant UT wedge in")
stmt, line("our 2017-2020 validation samples (~200-260 records/yr at exactly")
stmt, line("+288/+576).")
close_statement

*==============================================================================
* T8. Maryland TY2019: standard deduction minimum applied where the maximum
*     caps (15%-of-AGI rule)
*==============================================================================
clear
set obs 1
gen year   = 2019
gen state  = 21
gen mstat  = 1
gen page   = 40
gen pwages = 100000
submit_case, tag(t8_md_2019_std) out_dir("`out_dir'")

open_statement, tag(t8_md_2019_std) out_dir("`out_dir'")
stmt, line("TAXSIM-35, Maryland, TY2019, single, wages 100,000, standard")
stmt, line("deduction.")
stmt, line("")
stmt, line("Expected: MD standard deduction = 2,250. Form 502 instruction 16")
stmt, line("(TY2019): 15% of Maryland AGI bounded between 1,550 and 2,250 for")
stmt, line("single filers; 15% x 100,000 caps at the 2,250 MAXIMUM. TAXSIM")
stmt, line("returns v34_state_std_deduction_amount = 1,550 -- the minimum where")
stmt, line("the maximum belongs. TY2019 only (2018 and 2020 probe correctly,")
stmt, line("though 2020 uses the 2019 maxima, one indexing step stale). Effect:")
stmt, line("flat +33/+69/+83 overstatement of MD tax on every 2019")
stmt, line("standard-deduction return.")
close_statement

*==============================================================================
* T9. Wisconsin 2017-2018: bracket thresholds ~3% below the published DOR
*     tables; 2018 returns tax byte-identical to 2017
*==============================================================================
clear
set obs 1
gen year   = 2018
gen state  = 50
gen mstat  = 1
gen page   = 40
gen pwages = 400000
submit_case, tag(t9_wi_2018_brackets) out_dir("`out_dir'")

open_statement, tag(t9_wi_2018_brackets) out_dir("`out_dir'")
stmt, line("TAXSIM-35, Wisconsin, TY2018, single, wages 400,000.")
stmt, line("")
stmt, line("Expected: the TY2018 published DOR rate schedule.")
stmt, line("TAXSIM's WI 2017 and 2018 schedules use thresholds")
stmt, line("about 3% below the published tables (empirical top-bracket entry")
stmt, line("~320,250 MFJ vs the published 329,810 for 2017), and its 2018 tax")
stmt, line("is byte-identical to 2017 despite different published thresholds.")
stmt, line("Effect: flat overtaxation of ~12.80 for 6.27%-bracket records and")
stmt, line("~143.60 for top-bracket records in both years (~1,190 records/yr in")
stmt, line("our sample). The 2019-2020 vintages are correct.")
close_statement

*==============================================================================
* T10. Delaware: positive itemized deduction fabricated at the SALT cap for
*      filers with zero federal itemized components
*==============================================================================
clear
set obs 1
gen year   = 2019
gen state  = 8
gen mstat  = 1
gen page   = 40
gen pwages = 1500000
submit_case, tag(t10_de_fabricated_itemized) out_dir("`out_dir'")

open_statement, tag(t10_de_fabricated_itemized) out_dir("`out_dir'")
stmt, line("TAXSIM-35, Delaware, TY2019, single, wages 1,500,000, NO itemized")
stmt, line("inputs of any kind (no mortgage, otheritem, or proptax).")
stmt, line("")
stmt, line("Expected: DE deduction = the 3,250 standard (30 Del. C. 1108(a));")
stmt, line("v34 is correct throughout. TAXSIM instead reports")
stmt, line("v35_state_itemized_deduction = 10,000 -- exactly the federal SALT")
stmt, line("cap (5,000 MFS) -- and uses it, since it exceeds the DE standard")
stmt, line("deduction (v36 = state AGI - 10,000). The share of our sampled DE")
stmt, line("records with v35 > 0 jumps from ~54% in 2017-2018 to 97.0% in both")
stmt, line("2019 and 2020, suggesting a vintage change. Presumably TAXSIM's own")
stmt, line("computed DE income tax is being written into the DE itemized base;")
stmt, line("Delaware strips state income taxes from its base (PIT-RES Line 17b),")
stmt, line("so the 10,000 has no legal source.")
close_statement

*==============================================================================
* T12. Delaware: pension exclusion granted with no retirement income,
*      driving state AGI negative (age-80 zero-income exemplar)
*==============================================================================
clear
set obs 1
gen year   = 2019
gen state  = 8
gen mstat  = 1
gen page   = 80
submit_case, tag(t12_de_pension_exclusion) out_dir("`out_dir'")

open_statement, tag(t12_de_pension_exclusion) out_dir("`out_dir'")
stmt, line("TAXSIM-35, Delaware, TY2019, single, age 80, no income of any kind.")
stmt, line("")
stmt, line("Expected: DE state AGI = 0. TAXSIM returns v32_state_agi =")
stmt, line("-1,999.99: the 30 Del. C. 1106(b)(3) pension exclusion is granted")
stmt, line("with no pension, IRA, or eligible retirement income to exclude.")
stmt, line("619-642 records/yr in our 2017-2020 samples have TAXSIM state AGI")
stmt, line("below zero where ours is at or above zero. Related: the 2,000")
stmt, line("under-60 exclusion is granted against EARLY IRA distributions")
stmt, line("(sampled ages 32-59), which PIT-RES Line 6 disqualifies ('an early")
stmt, line("distribution from an IRA or pension fund ... does not qualify').")
close_statement

*==============================================================================
* T13. Oklahoma: $17,000 itemized cap applied flat, without the statutory
*      charity and medical exemptions (also an input-coverage note)
*==============================================================================
clear
set obs 1
gen year     = 2019
gen state    = 37
gen mstat    = 1
gen page     = 40
gen pwages   = 300000
gen mortgage = 30000
submit_case, tag(t13_ok_flat_cap) out_dir("`out_dir'")

open_statement, tag(t13_ok_flat_cap) out_dir("`out_dir'")
stmt, line("TAXSIM-35, Oklahoma, TY2019, single, wages 300,000, mortgage input")
stmt, line("30,000 (in our source data roughly half of this class is charitable")
stmt, line("contributions).")
stmt, line("")
stmt, line("Expected: 68 O.S. 2358(D)(1) caps OK itemized deductions at 17,000")
stmt, line("from TY2018 but EXEMPTS charitable contributions and medical")
stmt, line("expenses, so allowed = min(17000, base - charity - medical) +")
stmt, line("charity + medical. TAXSIM applies a flat 17,000: v35 = exactly")
stmt, line("17,000 on 91% of our OK itemizers in every cap year, and 'our")
stmt, line("itemized = TAXSIM 17,000 + charity + medical' holds to the dollar")
stmt, line("on 69% of them. TY2017 (no cap) is a clean control with no hits.")
stmt, line("Note the input-coverage angle: TAXSIM-35 has no separate charitable")
stmt, line("or medical inputs (both ride inside 'mortgage'/'otheritem'), so the")
stmt, line("statutory exemptions cannot be honored without new inputs.")
close_statement

*==============================================================================
* T14. District of Columbia: unemployment compensation subtracted from DC AGI
*      in years when DC taxed it (exempt only from TY2021)
*==============================================================================
clear
set obs 1
gen year   = 2019
gen state  = 9
gen mstat  = 1
gen page   = 40
gen pwages = 50000
gen pui    = 5000
submit_case, tag(t14_dc_ui_subtraction) out_dir("`out_dir'")

open_statement, tag(t14_dc_ui_subtraction) out_dir("`out_dir'")
stmt, line("TAXSIM-35, District of Columbia, TY2019, single, wages 50,000,")
stmt, line("unemployment compensation 5,000.")
stmt, line("")
stmt, line("Expected: DC state AGI = 55,000. The D-40 starts from federal AGI")
stmt, line("and no line of Schedule I Calculation B (the exhaustive subtraction")
stmt, line("list) subtracts unemployment compensation; the DC instructions say")
stmt, line("so directly -- 2017 booklet: 'All unemployment compensation received")
stmt, line("in 2017 is taxable'; 2020 booklet: 'All unemployment compensation")
stmt, line("received in 2020 is taxable.' The District first exempted UI in")
stmt, line("TY2021. TAXSIM returns v32_state_agi = 50,000 in 2017-2019 (and in")
stmt, line("2020 stacks the subtraction on top of the federal ARPA exclusion it")
stmt, line("also applies: v10 = 50,000 AND v32 = 45,000, removing the same")
stmt, line("5,000 twice). Effect: DC tax runs low by 4-8.95% of UI on every DC")
stmt, line("return with unemployment income in the window.")
close_statement

*==============================================================================
* T15. California 2017 CalEITC paid to childless filers past the
*      pre-expansion 25-64 age band
*==============================================================================
clear
set obs 1
gen year   = 2017
gen state  = 5
gen mstat  = 1
gen page   = 68
gen pwages = 3000
submit_case, tag(t15_ca_caleitc_age) out_dir("`out_dir'")

open_statement, tag(t15_ca_caleitc_age) out_dir("`out_dir'")
stmt, line("TAXSIM-35, California, TY2017, single, age 68, wages 3,000, no")
stmt, line("dependents.")
stmt, line("")
stmt, line("Expected: state EITC = 0. Through TY2017 the CalEITC followed the")
stmt, line("federal childless age band -- a filer without qualifying children")
stmt, line("had to be 25-64 at year end (FTB 3514 instructions). AB 1809")
stmt, line("expanded eligibility to ages 18-24 and 65+ only beginning TY2018.")
stmt, line("TAXSIM pays a 2017 credit to this 68-year-old (v39 > 0; on our")
stmt, line("validation sample, childless filers aged 67-73 are paid $60-$157).")
stmt, line("Amounts are small (the 2017 childless maximum was $223) but the")
stmt, line("age gate is categorical.")
close_statement

display as result "Bug-report artifacts written to `out_dir'/"
display as result "For each issue: email <tag>_statement.txt + the web-tool"
display as result "text response (from <tag>_webtool_input.csv, taxsimid = -1,"
display as result "idtl = 5) + <tag>_response.csv to feenberg@nber.org."
