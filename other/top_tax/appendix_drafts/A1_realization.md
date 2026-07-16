## A.1 Realizing or deferring gains

### Cohorts and the stock of unrealized gains

The model operates on representative age cohorts (for joint returns, the older spouse's age), followed forward to age 119 using SSA life tables beyond the microdata's range. The state variable is the cohort's stock of unrealized gains. Each year the stock is drawn down by realizations and by death, where the death regime determines what happens to the decedent's gains: under step-up they are extinguished, under carryover they pass to heir cohorts (allocated by a dollar-weighted heir-age distribution from the SCF), and under deemed realization they are taxed. The model tracks the change in each cohort's stock relative to baseline, so a gain that goes unrealized when rates rise does not disappear; it remains in the stock and resurfaces as a later sale or as a gain held at death.

Because unrealized gains are heavily concentrated, every cohort-level input — the marginal tax rate on gains, mortality, estate-tax exposure, the wealth-tax carrying cost defined below — is weighted by each household's share of the cohort's gains, so the cell parameters reflect the wealthy households who actually hold the gains. Mortality in particular is the probability that the marginal dollar's holder dies, well below the cohort's average. The cohort-level response is then distributed back to tax units in proportion to their gains.

### The agent's problem

Consider one dollar of unrealized gain. Its holder has some non-tax motive for selling — liquidity, rebalancing, consumption — summarized by a reservation benefit $b \geq 0$ drawn from an exponential distribution. The dollar is sold when $b$ exceeds the tax cost of realizing now rather than deferring, so the share of the stock realized in a year, $r$, is the survival function of that distribution evaluated at the tax wedge. Equivalently, the cohort chooses $r$ subject to a realization cost $C(r)$ whose marginal is

$$
C'(r) = \frac{1}{\eta}\,\ln\!\left(\frac{r}{r_B}\right),
$$

where $r_B$ is the cohort's baseline realization rate and $\eta$ governs response strength. The cost is zero at the margin under baseline policy, $C'(r_B) = 0$, so baseline behavior is reproduced exactly.

The tax cost of realizing now rather than deferring is

$$
MC \;=\; \tau \;+\; \beta\,(1-m)\,\big(W_{\text{next}} - h\big) \;+\; \beta\, m\, F,
$$

where $\tau$ is the current marginal tax rate on long-term gains (measured through the tax calculator, so it embeds surtaxes and interactions), $\beta$ is the real discount factor built from the 10-year Treasury rate deflated by CPI inflation year by year, $m$ is mortality, $W_{\text{next}}$ is next year's continuation value of an unrealized dollar (the same problem solved one year and one age ahead), $h$ is the annual wealth-tax cost of carrying the deferred position, and $F$ is the value of the tax treatment at death. Setting the marginal non-tax benefit equal to the marginal cost gives a closed form for the scenario realization rate:

$$
r \;=\; r_B \,\exp\!\big(-\eta\,(MC - MC_B)\big), \qquad \text{capped at } 1,
$$

where $MC_B$ is the marginal cost under baseline law. The response is a constant semi-elasticity in the full deferral wedge, not in the current-year rate alone.

### What enters the deferral wedge

Expected future rates enter through $W_{\text{next}}$, which is solved by backward induction over ages and years, so a pre-announced rate change moves behavior before it takes effect. The remaining terms give precise form to the statement in the memo that wealth and estate taxes also affect the value of deferral.

The death value is

$$
F \;=\; (1 - c_\varphi)\,\tau\,(1 - e).
$$

Here $c_\varphi$ is the share of the gain's tax burden the holder bears at death: $0$ under step-up (death forgives the tax entirely), $1$ under deemed realization (death triggers the full tax, so $F = 0$ and deferral buys nothing at death), and an intermediate value under carryover, reflecting how much of the heir's eventual liability the holder internalizes. The regime can differ by asset class, and $c_\varphi$ is the gain-weighted mix; gains bequeathed to charity escape tax under any regime and are netted out. The factor $(1 - e)$ is the estate-tax offset: capital-gains tax due at death is deductible from the taxable estate, so each dollar of forgiven gains tax is worth only $(1 - e)$, where $e$ is the household's marginal estate rate. Below the estate exemption, $e = 0$ and the term is inert.

The carrying cost $h$ is the product of the household's marginal wealth-tax rate and its capital-gains rate, averaged with gain weights. Realizing today removes the tax from the wealth base, while deferring keeps the full pre-tax dollar in the base, so each year of continued deferral costs the wealth tax on the deferred liability. An annual wealth tax therefore raises realizations even with no change in the capital-gains schedule.

The 25 percent valuation and compliance discount under deemed realization applies to the revenue collected from gains taxed at death, not to the holder's incentive: the deferral decision sees the full statutory burden.

### One pool rather than two

An earlier version split realizations into a responsive share and an inert share of holders assumed never to respond. The exponential reservation-benefit distribution makes that split unnecessary: it already implies that the first dollar of the stock is easier to sell than the last, with a thin tail of holders whose non-tax motives dominate any plausible wedge. We therefore model a single pool, and $\eta$ is then itself the aggregate long-run semi-elasticity, with no deflation for an unresponsive remainder.

### The short-run timing margin

Retiming around an anticipated rate change is a separate overlay. A calibrated fraction of each year's realizations may shift by up to one year toward the adjacent year with the lowest rate, with the shifted share proportional to the rate gap. Under a permanent, uniform rate change every year looks the same and the overlay nets to zero, so the permanent margin and the timing margin can be calibrated independently.

### Calibration

We calibrate $\eta$ on the full model rather than on the cohort problem in isolation, so that the target elasticity is hit after mortality, death treatment, discounting, and the allocation to tax units are all accounted for. We run the simulator under a permanent capital-gains rate increase, measure the proportional realization response at a thirty-year horizon per unit change in the rate, and repeat across a small grid of $\eta$ values. The measured response is linear in $\eta$ through the origin, so the calibration is an inversion of a fitted line. The target is our preferred permanent realization elasticity of $-0.6$, expressed as a semi-elasticity by dividing by the baseline top rate of about $0.238$; with a fitted slope of $1.0155$ this gives $\eta = 2.4825$, which we quote as $2.48$. The timeable fraction, $0.25$, is calibrated the same way to a short-run realization elasticity of $-1.2$ around an announced rate change. The deemed-realization discount of 25 percent is set so that the model's ten-year revenue from taxing gains at death matches the JCT score of that policy.

### The exported object: the value of deferral

The realization model also produces, for each cohort and year, the expected present value of tax per dollar entering the deferred-gain state, which we write $\tau_{eq}$. It is computed by a backward recursion over the scenario's own realization path: in each future year a deferred dollar pays $\tau$ if realized, pays the death-triggered tax net of the estate offset if its holder dies under deemed realization, and pays the carrying cost $h$ if it survives unrealized under a wealth tax, all discounted at $\beta$. Under current law with step-up, $\tau_{eq}$ is well below the statutory rate; taxing gains at death pushes it toward $\tau$. This object is the price of the deferred-gain state used by the income-conversion and entity-form margins (Sections A.2 and A.3): any policy that makes deferral less valuable raises $\tau_{eq}$ and thereby discourages recharacterizing ordinary income as gains, with no separate assumption required.
