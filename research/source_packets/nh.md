# New Hampshire Source Packet

State: `NH`
Status: see `../state_tax/state_parameter_rollout.csv`
Last updated: `2026-07-13`

## Scope

- Tax years covered: 2017-forward.
- Profile: `narrow_investment_iit`.
- Encoded law: Interest and Dividends Tax through tax year 2024; repeal for
  taxable periods beginning on or after January 1, 2025.

## Primary Sources

- New Hampshire DRA, [Interest and Dividends Tax](https://www.revenue.nh.gov/taxes-glance/interest-dividends-tax).
- New Hampshire DRA, [Interest and Dividends Tax FAQs](https://www.revenue.nh.gov/resource-center/frequently-asked-questions/interest-dividends-tax-frequently-asked-questions).
- New Hampshire DRA, [repeal notice](https://www.revenue.nh.gov/news-and-media/repeal-nh-interest-and-dividends-tax-now-effect).

## Encoded Parameters

- Rate: 5% for 2017-2022, 4% for 2023, 3% for 2024, and zero from 2025.
- Filing threshold and exemption: $2,400 single / $4,800 joint.
- Additional exemption: $1,200 for each age-65-or-older or blind filer.
- PUF proxy tax base: taxable interest plus ordinary and qualified dividends.

## Worksheet Tests

- 2024 basic interest/dividend tax calculation.
- 2024 age-65 additional exemption.
- 2025 repeal.

## Known Differences

- The PUF proxy omits taxable annuity and cash/property distributions included
  by NH and cannot fully isolate federally exempt interest subject to NH tax.
- The disability exemption and some source-specific exclusions are unobserved.
- No aggregate or cross-model validation is claimed before state weights land.

## Next Validation

- Compare pre-2025 micro results against historical NH forms/TAXSIM where a
  comparable Interest and Dividends Tax output is available.
- Reconcile weighted 2017-2024 liability to DRA receipts after state weights
  are available.
