

# ca_rec_gsi_1988_2002

This is just a place to hold some analyses I did during revision of a paper. 

Much of the previous analysis of genetic data was done by Eric Crandall and Javier Ciancio.

## Make a table of CWT vs GSI assignments:

```r
source("R-main/01-create-tables-etc.R")

# after that you have the file: gsi_vs_cwt_table.csv
```
Note that `pp_lo` is the lowest posterior probability (of assignment) of all the fish
assigned to the repunit from that tag group.  `pp_hi` is the max.  If there is only one
fish in the group `pp_lo = pp_hi`.

## Test if the doubtful assignment fish are different in composition than the good ones
```r
source("R-main/01-create-tables-etc.R")

# after that do this:
CHISQ

# and that returns the results of the chi-squared test
```