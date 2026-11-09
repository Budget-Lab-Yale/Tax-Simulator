## B. Sourcing for Table 1

This section expands the one-line "Basis" column of Table 1. For each parameter we give the underlying citation, describe how the published estimate maps into the model's units, and, where the value is calibrated rather than taken from the literature, describe the calibration and date it. Precise pinned values are given here; the memo's table rounds them.

**Capital gains realization.**

**Long-run response (η).** The pinned value is 2.4825; Table 1 rounds to 2.48. η is the semi-elasticity in the realization rule — each cell's discretionary realization rate scales by exp(−η × change in the marginal tax cost of realizing) — and because the whole gain pool responds, it is also the model's aggregate long-run semi-elasticity. It is calibrated so that the full model reproduces our preferred permanent realization elasticity of −0.6, which sits inside the empirical range: Dowd, McClelland, and Muthitacharoen (2015) estimate a persistent elasticity of −0.72 (with a within-year elasticity of −1.2) on a large panel of returns, Agersnap and Zidar (2021) find ten-year revenue elasticities of −0.3 to −0.5 with revenue-maximizing rates of 38 to 47 percent, and the JCT and CBO scoring conventions sit in the same neighborhood. The calibration (July 2026): we ran a permanent increase in the top statutory rate on gains to 25 percent through the full model at several trial values of η, measured the realized long-run response at simulation year 30, and found the measured response linear in η; inverting the fitted line at the target semi-elasticity of −0.6/0.238 = −2.52 (the elasticity converted at the 23.8 percent top federal rate on long-term gains) gives 2.4825. The fitted line's coefficients are kept in the calibration records so that future re-derivations are arithmetic.

**Share of realizations that can retime ±1 year.** Pinned at 0.2542; Table 1 rounds to 0.25. This is the fraction of baseline realizations free to shift across adjacent years toward the lower-rate year, and it carries the model's short-run announcement response. It is calibrated (July 2026) to our preferred short-run elasticity of −1.2, which is the within-year estimate in Dowd, McClelland, and Muthitacharoen (2015). Because the retiming margin nets to zero under a uniform permanent rate change, η is pinned first and independently; given η, the timing share is then found by a direct root-finding search on the full model against the short-run moment (target semi-elasticity 1.2/0.238 = 5.04).

**Valuation and compliance discount under deemed realization at death.** A measurement parameter, not a behavioral response: it scales down the gains deemed realized at death for valuation games and noncompliance that revenue estimators assume but the realization model itself does not produce. At the 25 percent discount, the model's ten-year revenue from a deemed-realization-at-death regime comes in near $695 billion, against roughly $600 billion in JCT scoring of comparable proposals; a discount of 25 to 33 percent is also consistent with the valuation discount our estate-tax calibration implies for pass-through assets. We intend eventually to replace this single number with the estate side's asset-class-specific reporting factors.

**Income reporting and form.**

**Income conversion (σ).** σ = 0.16 governs how much top-bracket labor-type compensation is restructured into unrealized equity gains when the wedge between the ordinary rate and the equity path's expected present-value tax rate widens: 0.16 percent of the eligible compensation pool converts per percentage point of wedge. It is the model's one residual parameter. With every other reporting margin active (realization, entity shifting, underreporting, charity), we run a five-point top-rate increase through the full model, measure the total elasticity of top taxable ordinary income, and set σ so that this lands on 0.25 — the central reading of Saez, Slemrod, and Giertz (2012), whose survey puts the best available estimates between 0.12 and 0.40. The value was re-derived in July 2026 after revisions to the entity-shifting and underreporting modules, yielding 0.157, shipped as 0.16; a confirmation run measures the total top ETI at 0.2508. Because σ is residual, it is re-derived whenever any other margin in the stack changes; a standing check in the calibration records enforces this.

**Entity-shifting semi-elasticity.** From a Penn Wharton Budget Model working paper by Pearce and Prisinzano (2018), who estimate how the share of business income accruing to corporations responds to the corporate/pass-through rate differential. We take their preferred estimate of 0.3788 (their Table IV.B) and divide by the pass-through sector's roughly 60 percent share of business income, giving a semi-elasticity of pass-through income of 0.3788/0.6 ≈ 0.63 per percentage point of rate differential. One departure from the paper's framework: where the realization model is active, the tax value of deferral on retained corporate earnings is priced by that model's expected present-value tax rate for the owner's age and the prevailing death regime, rather than by the paper's fixed approximation.

**Underreporting elasticities.** From work by DeBacker, Heim, and Yuskavage at the Treasury's Office of Tax Analysis, presented at the National Tax Association annual meetings in November 2025, using IRS random-audit data from 2006 to 2017. They estimate the elasticity of noncompliance with respect to the net-of-tax rate as a component of the overall ETI, and we apply their values as net-of-tax-rate elasticities of reported income: 0.046 for self-employment income (Schedule C and F), 0.052 for partnership and S-corporation income, and 0.040 for rent (their weakest-identified estimate). Wages, interest, and dividends are assigned no response, reflecting third-party information reporting; only positive income is scaled, so the overstated-loss and deduction margins are not modeled.

**Charitable giving price elasticity (cash).** A tax-price elasticity of −0.5 applied to cash contributions on the intensive margin, where the price of giving is one minus the marginal subsidy rate. The value matches Randolph (1995), who separates permanent from transitory price responses in panel data and finds a permanent price elasticity of about −0.5. Later panel estimates run somewhat larger — Bakija and Heim (2011) find persistent elasticities of roughly −0.7 to −1 — so −0.5 is a conservative central value for a persistent response. The appreciated-asset giving margin, whose price depends on the capital gains rate, is a documented gap.

**Wealth and estates.**

**Reported-wealth semi-elasticities.** The values −7 for marketable and −17 for closely held wealth are assumptions carried over from our standalone wealth-tax model, reviewed and accepted in July 2026; they are not estimates from a single study. Units: reported wealth in each class scales by exp(rate × e), so at a 3 percent marginal rate they imply roughly 19 percent erosion of reported marketable wealth and 40 percent of closely held. The quasi-experimental literature spans a wide range that brackets these values: Seim (2017) finds small, mainly reporting responses in Sweden; Jakobsen, Jakobsen, Kleven, and Zucman (2020) find sizable long-run responses at the top in Denmark; Brülhart, Gruber, Krapf, and Schmidheiny (2022) find reported wealth about 43 percent higher per percentage-point rate cut in Switzerland, well above our assumption; and Londoño-Vélez and Ávila-Mahecha (2021) document that the wealthiest Colombians concealed about a third of their wealth offshore. These semi-elasticities also serve as a ceiling that absorbs migration and expatriation, which we do not model separately, and we recommend publishing sensitivity bands around them.

**Concealment share of the avoidance response.** Structural assumptions, set in July 2026, splitting the avoidance response above into concealment (the asset and its income and estate value leave the reported bases entirely) and legal valuation gaming (the assessed value is lowballed but the income remains visible). Marketable assets have observable exchange prices, so their avoidance is treated as 100 percent concealment; closely held avoidance is split 50/50, since valuation discounts on private businesses are real and legal. The concealment reading is consistent with the Colombian evidence, where top-end evasion took the form of entire assets hidden offshore rather than shaded valuations.

**Reported-estate net-of-tax elasticity.** From Kopczuk and Slemrod (2001), who regress reported estates on the estate tax rates prevailing over the decedent's life and find an elasticity with respect to the net-of-tax rate of about 0.16, with pooled estimates ranging from 0.10 to 0.22 across specifications; we carry the band into our sensitivity analysis. Adopted July 2026 in an exact net-of-tax power form — the reported estate retains the fraction ((1 − τ_scenario)/(1 − τ_baseline))^0.16, evaluated at each record's marginal estate rate — which reproduces the local elasticity at the current top rate, remains exact for large reforms, and handles newly taxable estates.

**Mechanical channels.**

**Share of new tax paid out of saving.** The financing profile — the share of a persistent above-baseline tax flow paid out of saving rather than consumption, by age and within-age wealth rank, running from about 0.10 at the bottom to 0.80 at the top — was calibrated in July 2026. The anchor is the bridge s = 1 − ε·(C/Y), with ε ≈ 0.7 the elasticity of consumption to permanent income from Straub (2019) and consumption-to-income ratios by rank from Mian, Straub, and Sufi (2020): the top 1 percent's income share near 20 percent against a consumption share of 6 to 7 percent gives s ≈ 0.8 at the top, while hand-to-mouth behavior (Kaplan and Violante 2022) pulls the bottom toward 0.10. The gradient is cross-checked against the saving-rate gradient in lifetime income in Dynan, Skinner, and Zeldes (2004), whose top 1 percent saves about half of income across specifications, and against the liquidity gradient of transitory MPCs in Fagereng, Holm, and Natvik (2021); the age tilt is attenuated to zero at top ranks because high-income elderly households do not run down wealth (De Nardi, French, and Jones 2010). Validation runs in July 2026 put the dollar-weighted effective share for top-concentrated reforms at about 0.78, at the top of the predicted band.

**Corporate incidence constants.** Provisional placeholders pending direct measurement, and we present corporate-rate results as such. The one anchored value is the normal-return share of the corporate wedge, 0.375: Treasury's distribution methodology attributes about 63 percent of the corporate tax to supernormal returns (Cronin, Lin, Power, and Cooper 2013) and the Tax Policy Center's about 60 percent (Nunns 2012), implying a normal share of 0.37 to 0.40; we sweep corners of 0 and 0.5. The remaining constants — the C-corporation share of the normal capital stock (0.40), the U.S.-taxable exposure scale, and the equity risk premium among them — carry stated priors rather than measurements, and each is exposed for sensitivity sweeps.

### References

Agersnap, Ole, and Owen Zidar. 2021. "The Tax Elasticity of Capital Gains and Revenue-Maximizing Rates." *American Economic Review: Insights* 3 (4): 399–416.

Bakija, Jon, and Bradley T. Heim. 2011. "How Does Charitable Giving Respond to Incentives and Income? New Estimates from Panel Data." *National Tax Journal* 64 (2, Part 2): 615–650.

Brülhart, Marius, Jonathan Gruber, Matthias Krapf, and Kurt Schmidheiny. 2022. "Behavioral Responses to Wealth Taxes: Evidence from Switzerland." *American Economic Journal: Economic Policy* 14 (4): 111–150.

Cronin, Julie Anne, Emily Y. Lin, Laura Power, and Michael Cooper. 2013. "Distributing the Corporate Income Tax: Revised U.S. Treasury Methodology." *National Tax Journal* 66 (1): 239–262.

DeBacker, Jason, Bradley Heim, and Alexander Yuskavage. 2025. "Marginal Tax Rates and Income Tax Noncompliance." U.S. Department of the Treasury, Office of Tax Analysis; presented at the National Tax Association annual meetings, November 2025.

De Nardi, Mariacristina, Eric French, and John B. Jones. 2010. "Why Do the Elderly Save? The Role of Medical Expenses." *Journal of Political Economy* 118 (1): 39–75.

Dowd, Tim, Robert McClelland, and Athiphat Muthitacharoen. 2015. "New Evidence on the Tax Elasticity of Capital Gains." *National Tax Journal* 68 (3): 511–544.

Dynan, Karen E., Jonathan Skinner, and Stephen P. Zeldes. 2004. "Do the Rich Save More?" *Journal of Political Economy* 112 (2): 397–444.

Fagereng, Andreas, Martin B. Holm, and Gisle J. Natvik. 2021. "MPC Heterogeneity and Household Balance Sheets." *American Economic Journal: Macroeconomics* 13 (4).

Jakobsen, Katrine, Kristian Jakobsen, Henrik Kleven, and Gabriel Zucman. 2020. "Wealth Taxation and Wealth Accumulation: Theory and Evidence from Denmark." *Quarterly Journal of Economics* 135 (1): 329–388.

Kaplan, Greg, and Giovanni L. Violante. 2022. "The Marginal Propensity to Consume in Heterogeneous Agent Models." *Annual Review of Economics* 14.

Kopczuk, Wojciech, and Joel Slemrod. 2001. "The Impact of the Estate Tax on Wealth Accumulation and Avoidance Behavior." In *Rethinking Estate and Gift Taxation*, edited by William G. Gale, James R. Hines Jr., and Joel Slemrod. Washington, DC: Brookings Institution Press.

Londoño-Vélez, Juliana, and Javier Ávila-Mahecha. 2021. "Enforcing Wealth Taxes in the Developing World: Quasi-Experimental Evidence from Colombia." *American Economic Review: Insights* 3 (2): 131–148.

Mian, Atif, Ludwig Straub, and Amir Sufi. 2020. "The Saving Glut of the Rich." NBER Working Paper 26941.

Nunns, Jim. 2012. "How TPC Distributes the Corporate Income Tax." Urban-Brookings Tax Policy Center.

Pearce, James, and Richard Prisinzano. 2018. "Tax Based Switching of Business Income." Penn Wharton Budget Model Working Paper W2018-2.

Randolph, William C. 1995. "Dynamic Income, Progressive Taxes, and the Timing of Charitable Contributions." *Journal of Political Economy* 103 (4): 709–738.

Saez, Emmanuel, Joel Slemrod, and Seth H. Giertz. 2012. "The Elasticity of Taxable Income with Respect to Marginal Tax Rates: A Critical Review." *Journal of Economic Literature* 50 (1): 3–50.

Seim, David. 2017. "Behavioral Responses to Wealth Taxes: Evidence from Sweden." *American Economic Journal: Economic Policy* 9 (4): 395–421.

Straub, Ludwig. 2019. "Consumption, Savings, and the Distribution of Permanent Income." Working paper, Harvard University.
