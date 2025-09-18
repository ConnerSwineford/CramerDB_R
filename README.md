# cramerdb

**cramerdb** lets staff pull JSON/GeoJSON from our APIs into a data frame (or `sf` tibble) with one call, and write back via API (create, update, upsert).

## Install
```r
# one-time: install pak
install.packages("pak")

# install cramerdb from GitHub
options(pkgType = "binary")
install.packages("pak", type = "binary")

# load it
library(cramerdb)
```

## Usage
```r
# 1) Read 
visits <- fetch("http://172.18.10.103:8000/api/veg-rec/visit/")
plots  <- fetch("http://172.18.10.103:8000/api/veg-rec/plot/")  # sf with geom

# 2) Security/Authetication (not implemented yet)
tok <- "Token XXXXX" 
hdr <- list(Authorization = tok)

# create new rows
create("http://172.18.10.103:8000/api/veg-rec/visit/", visits_new, headers = hdr)

# update existing rows by id
update("http://172.18.10.103:8000/api/veg-rec/visit/", visits_update, headers = hdr)

# upsert (update if id exists, else create)
upsert("http://172.18.10.103:8000/api/veg-rec/visit/", visits_mixed, headers = hdr)
```
