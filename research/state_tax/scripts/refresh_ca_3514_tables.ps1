<#
Refreshes the committed California FTB 3514 CalEITC lookup data from the
official annual HTML instructions. Review the resulting CSV diff whenever FTB
revises a historical page. This is a research-maintenance script, not runtime
model code.
#>

$ErrorActionPreference = 'Stop'

$sources = [ordered]@{
  2017 = 'https://www.ftb.ca.gov/forms/2017/17-3514-instructions.html'
  2018 = 'https://www.ftb.ca.gov/forms/2018/18-3514-instructions.html'
  2019 = 'https://www.ftb.ca.gov/forms/2019/2019-3514-instructions.html'
  2020 = 'https://www.ftb.ca.gov/forms/2020/2020-3514-instructions.html'
  2021 = 'https://www.ftb.ca.gov/forms/2021/2021-3514-instructions.html'
  2022 = 'https://www.ftb.ca.gov/forms/2022/2022-3514-instructions.html'
  2023 = 'https://www.ftb.ca.gov/forms/2023/2023-3514-booklet.html'
  2024 = 'https://www.ftb.ca.gov/forms/2024/2024-3514-booklet.html'
  2025 = 'https://www.ftb.ca.gov/forms/2025/2025-3514-booklet.html'
}

$records = foreach ($entry in $sources.GetEnumerator()) {
  $year = [int]$entry.Key
  $html = (Invoke-WebRequest -Uri $entry.Value -UseBasicParsing).Content
  $table = [regex]::Matches($html, '(?is)<table\b.*?</table>') |
    Sort-Object Length -Descending |
    Select-Object -First 1
  if ($null -eq $table) {
    throw "No FTB 3514 table found for $year"
  }

  foreach ($row in [regex]::Matches($table.Value, '(?is)<tr\b.*?</tr>')) {
    $cells = [regex]::Matches($row.Value, '(?is)<t[dh]\b.*?</t[dh]>') |
      ForEach-Object {
        (($_.Value -replace '(?is)<[^>]+>', ' ' -replace '&nbsp;', ' ' -replace '\s+', ' ').Trim())
      }
    if ($cells.Count -lt 6 -or $cells[0] -notmatch '\d' -or $cells[1] -notmatch '\d') {
      continue
    }

    $lower = [int]($cells[0] -replace '[^0-9]', '')
    $upper = [int]($cells[1] -replace '[^0-9]', '')
    # FTB 3514 uses inclusive $50 bands. The 2020 HTML has one malformed
    # lower bound (086 for the $1,101-$1,150 row), so reconstruct only ranges
    # whose printed endpoints fail that invariant and retain the raw upper end.
    if ($upper - $lower -ne 49) {
      if ($lower -ge $upper) {
        throw "Invalid FTB 3514 income band in ${year}: $lower-$upper"
      }
      $lower = $upper - 49
    }
    foreach ($childCount in 0..3) {
      $amountText = $cells[$childCount + 2] -replace '[^0-9]', ''
      if ([string]::IsNullOrEmpty($amountText) -or [int]$amountText -eq 0) {
        continue
      }
      [pscustomobject]@{
        credit_id   = 'independent_earned_income'
        state       = 'CA'
        year        = $year
        income_lower = $lower
        income_upper = $upper
        child_count = $childCount
        amount      = [int]$amountText
      }
    }
  }
}

# The 2017 table encodes these final $1 ranges in footnotes rather than cells.
$tailStarts = @{
  0 = 15001
  1 = 22301
  2 = 22301
  3 = 22301
}
$tailEnds = @{
  0 = 15008
  1 = 22322
  2 = 22309
  3 = 22302
}
$records = $records | Where-Object {
  -not ($_.year -eq 2017 -and $_.child_count -in $tailStarts.Keys -and
        $_.income_lower -eq $tailStarts[$_.child_count])
}
foreach ($childCount in 0..3) {
  $records += [pscustomobject]@{
    credit_id   = 'independent_earned_income'
    state       = 'CA'
    year        = 2017
    income_lower = $tailStarts[$childCount]
    income_upper = $tailEnds[$childCount]
    child_count = $childCount
    amount      = 1
  }
}

$output = Join-Path $PSScriptRoot '..\..\..\config\scenarios\tax_law_state\baseline\ca\credit_tables.csv'
$records |
  Sort-Object year, child_count, income_lower |
  Select-Object credit_id, state, year, income_lower, income_upper, child_count, amount |
  Export-Csv -LiteralPath $output -NoTypeInformation -Encoding utf8

Write-Output "Wrote $($records.Count) rows to $output"
