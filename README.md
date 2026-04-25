## Navigating to a task folder - for part 2
 
```powershell
cd "$(git rev-parse --show-toplevel)\tests\task<task_number>"
```
 
Example:
```powershell
cd "$(git rev-parse --show-toplevel)\tests\task9"
```

## Extended Local Tests for Part 2 (Tasks 6–10)
 
Place the `tests2` folder inside your repo root, then run each case from inside its folder:
 
```powershell
cd tests2/task7/case01_truth_table
stack exec -- rdf-query-dsl-interpreter t7.rql
```
 
## Comparing against expected output
 
```powershell
stack exec -- rdf-query-dsl-interpreter t7.rql | Set-Content actual.txt
Compare-Object (Get-Content expected.txt) (Get-Content actual.txt)
```
 
If `Compare-Object` prints nothing, the output matches exactly.
 
## Running all cases at once
 
From the repo root:
 
```powershell
powershell -ExecutionPolicy Bypass -File .\tests2\verify_all.ps1
```
 
## Notes
 
- These are local stress tests only
- Do not submit the `.ttl` files unless explicitly asked
- Your Part 2 submission should contain `t6.rql` to `t10.rql`