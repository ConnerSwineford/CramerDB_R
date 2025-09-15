# cramerdb

**cramerdb** lets staff pull JSON/GeoJSON from our APIs into a data frame (or `sf` tibble) with one call, and write back via API (create, update, upsert).

## Install

### Option A — simple (source build)
```r
install.packages("pak")         # once
pak::pak("ORG_OR_USER/cramerdb")
# or: remotes::install_github("ORG_OR_USER/cramerdb")
library(cramerdb)

# 1) Read (auto-paginates; returns tibble or sf)
visits <- fetch("http://172.18.10.103:8000/api/veg-rec/visit/")
plots  <- fetch("http://172.18.10.103:8000/api/veg-rec/plot/")  # sf with geom

# 2) Write via API (no DB connection)
tok <- "Token XXXXX"  # ask a lead for yours
hdr <- list(Authorization = tok)

# create new rows
create("http://172.18.10.103:8000/api/veg-rec/visit/", visits_new, headers = hdr)

# update existing rows by id
update("http://172.18.10.103:8000/api/veg-rec/visit/", visits_update, headers = hdr)

# upsert (update if id exists, else create)
upsert("http://172.18.10.103:8000/api/veg-rec/visit/", visits_mixed, headers = hdr)
