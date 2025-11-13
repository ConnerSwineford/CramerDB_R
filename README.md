# cramerdb

**cramerdb** lets staff pull JSON/GeoJSON from our APIs into a data frame with one call, and write back via API (create, update, upsert).

## Install

```r
# one-time: install pak
options(pkgType = "binary")
install.packages("pak", type = "binary")

# install cramerdb from GitHub
pak::pak("ConnerSwineford/cramerdb")

# load it
library(cramerdb)
```

## Usage

### 1) Read data

```r
visits <- fetch("http://172.18.10.199:8000/api/veg-rec/visit/")
plots  <- fetch("http://172.18.10.199:8000/api/veg-rec/plot/")  # sf with geom
```

---

## 2) Authentication

`cramerdb` uses a simple token storage helper so you don't need to manually pass headers everywhere.

### Set your token once per session

```r
set_token("XXXXX")
```

This stores the token inside R’s session options.

### Retrieve your token (for debugging)

```r
get_token()
# [1] "XXXXX"
```

### Use authenticated requests

All write operations (`create`, `update`, `upsert`) automatically use the stored token if `headers` is not provided.

```r
# create new rows
create("http://172.18.10.199:8000/api/veg-rec/visit/", visits_new)

# update existing rows
update("http://172.18.10.199:8000/api/veg-rec/visit/", visits_update)

# upsert (update if id exists, else create)
upsert("http://172.18.10.199:8000/api/veg-rec/visit/", visits_mixed)
```

### Manually override headers (optional)

If you want to supply a different token than the one stored:

```r
hdr <- list(Authorization = "OVERRIDE123")
create("http://172.18.10.199:8000/api/veg-rec/visit/", visits_new, headers = hdr)
```
