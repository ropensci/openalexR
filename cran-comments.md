## Submission

This is an urgent patch release submitted shortly after the previous version (3.1.1). 
A recent change in the OpenAlex API added `doi` (and other fields) to location objects, 
which causes `oa2df()` in 3.1.1 to fail with "Column name `doi` must not be duplicated". 
This release fixes the issue by keeping only the known location fields from `primary_location`, 
restoring correct functionality for users.

## R CMD check results

0 errors | 0 warnings | 0 notes
