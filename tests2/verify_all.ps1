$ErrorActionPreference = "Stop"
$root = Split-Path -Parent $MyInvocation.MyCommand.Path
$cases = Get-ChildItem -Path $root -Directory -Recurse | Where-Object { Test-Path (Join-Path $_.FullName "expected.txt") }
$failed = 0

foreach ($case in $cases) {
  Push-Location $case.FullName
  try {
    $rql = Get-ChildItem -Filter "t*.rql" | Select-Object -First 1
    if ($null -eq $rql) {
      Write-Host "MISSING RQL: $($case.FullName)"
      $failed++
      continue
    }

    $output = @(stack exec -- rdf-query-dsl-interpreter $rql.Name)
    [System.IO.File]::WriteAllLines((Join-Path (Get-Location) "actual.txt"), [string[]]$output)

    $expected = [System.IO.File]::ReadAllLines((Join-Path (Get-Location) "expected.txt"))
    $actual = [System.IO.File]::ReadAllLines((Join-Path (Get-Location) "actual.txt"))

    if ($expected.Count -eq 0 -and $actual.Count -eq 0) {
      Write-Host "PASS: $($case.FullName)"
      continue
    }

    if ($expected.Count -eq 0 -or $actual.Count -eq 0) {
      Write-Host "FAIL: $($case.FullName)"
      Write-Host "Expected lines: $($expected.Count), actual lines: $($actual.Count)"
      $failed++
      continue
    }

    $diff = Compare-Object $expected $actual

    if ($diff) {
      Write-Host "FAIL: $($case.FullName)"
      $diff | Format-Table | Out-String | Write-Host
      $failed++
    } else {
      Write-Host "PASS: $($case.FullName)"
    }
  } finally {
    Pop-Location
  }
}

if ($failed -gt 0) {
  Write-Host "$failed case(s) failed."
  exit 1
} else {
  Write-Host "All cases passed."
}