# Tennessee Source Packet

State: `TN`
Status: `encoded_initial_validation`
Last updated: `2026-07-13`

## Scope

- Tax years covered: 2017-forward.
- Profile: `narrow_investment_iit`.
- Encoded law: Hall Income Tax in 2017-2020 and full repeal from 2021.

## Primary Sources

- Tennessee DOR, [Hall Income Tax](https://www.tn.gov/revenue/taxes/hall-income-tax.html).
- Tennessee DOR, [Hall Income Tax rate history](https://revenue.support.tn.gov/hc/en-us/articles/360057355792-HIT-4-Hall-Income-Tax-Rate).
- Tennessee DOR, [Hall Income Tax Manual (August 2021)](https://www.tn.gov/content/dam/tn/revenue/documents/tax_manuals/august-2021/Hall-Income-Tax.pdf).

## Encoded Parameters

- Rate: 4% in 2017, 3% in 2018, 2% in 2019, 1% in 2020, zero from 2021.
- Filing threshold and exemption: $1,250 single / $2,500 joint.
- Full exemption for age-65-or-older filers below $37,000 single / $68,000
  joint total annual income; age-100 exemption begins in 2018.
- Blindness is modeled as a full exemption; a jointly filed single-blind-spouse
  allocation is not separately identifiable.

## Worksheet Tests

- 2017 statutory rate and personal exemption.
- 2020 phase-down rate and joint exemption.
- Age-65 low-income exemption and 2021 repeal.

## Known Differences

- The PUF proxy base is taxable interest plus dividends. It cannot identify
  all legal source and ownership exclusions.
- Total income for the senior exemption uses AGI plus nontaxable Social
  Security, rather than the full statutory all-source-income measure.
- Angel-investor credit, blind-spouse ownership allocation, and other
  special exemptions are omitted or approximated.

## Next Validation

- Compare 2017-2020 worksheets with historical Tennessee returns and TAXSIM,
  where comparable.
- Reconcile weighted Hall-tax liability to historical receipts after state
  weights are available.
