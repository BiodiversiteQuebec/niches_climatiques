

library(gbifdb)
library(dplyr)

keep <- c("species", "phylum", "class", "order", "eventdate", "countrycode", "stateprovince", "occurrencestatus","taxonrank","decimallatitude", "decimallongitude", "coordinateuncertaintyinmeters", "occurrencestatus", "institutioncode", "recordedby")

gbif <- gbif_remote(backend = "duckdb")
Sys.time()
g <- gbif |>
  select(any_of(keep)) |>
  filter(countrycode %in% c("CA", "US")) |>
  filter(!class %in% c("Aves")) |>
  count(species) |>
  sample_n() |>
  collect() 
Sys.time() 
g <- st_as_sf(g, coords = c("decimallongitude", "decimallatitude"), crs = 4326)



gbif <- gbif_remote(backend = "duckdb")
Sys.time()
g <- gbif |>
  select(any_of(keep)) |>
  filter(countrycode %in% c("CA")) |>
  filter(!class %in% c("Aves")) |>
  #count(species) |>
  sample_n(3) |>
  collect() 
Sys.time() 




library(gbifdb)
gbif <- gbif_remote()

# Load DuckDB library
library(duckdb)

# Create an in-memory DuckDB connection
con <- dbConnect(duckdb::duckdb())
dbExecute(con, "INSTALL 'httpfs';")
dbExecute(con, "LOAD 'httpfs';")
# Define the S3 URL (make sure the S3 bucket is publicly accessible or you have the right permissions)
s3_url <- 's3://gbif-open-data-us-east-1/occurrence/2025-03-01/occurrence.parquet'

# Query the remote Parquet file stored in S3
query_result <- dbGetQuery(con, paste0("
  SELECT column1, column2
  FROM '", s3_url, "'
  WHERE class = 'Aves'
  ORDER BY column1 DESC
  LIMIT 1
"))

# View the result
print(query_result)

# Close the connection
dbDisconnect(con)


#######################
#######################
#######################
#######################
#######################

s3://bq-io/atlas/parquet/atlas_public_2024-11-07.parquet

# Load DuckDB library
library(duckdb)

# Create an in-memory DuckDB connection
con <- dbConnect(duckdb::duckdb())
dbExecute(con, "INSTALL 'httpfs';")
dbExecute(con, "LOAD 'httpfs';")
# Define the S3 URL (make sure the S3 bucket is publicly accessible or you have the right permissions)
#s3_url <- 's3://gbif-open-data-us-east-1/occurrence/2025-03-01/occurrence.parquet'
s3_url <- 's3://bq-io/atlas/parquet/atlas_public_2024-11-07.parquet'
s3_url <- 'https://object-arbutus.cloud.computecanada.ca/bq-io/atlas/parquet/atlas_public_2024-11-07.parquet'
s3_url <- 'https://gbif-open-data-sa-east-1.s3.sa-east-1.amazonaws.com/occurrence.parquet'
s3_url <- 'https://gbif-open-data-sa-east-1.s3.sa-east-1.amazonaws.com/occurrence/2025-03-01/occurrence.parquet'
s3_url <- 'https://gbif-open-data-us-east-1.s3.us-east-1.amazonaws.com/index.html#occurrence/2025-03-01/occurrence.parquet'
s3_url <- 'https://gbif-open-data-us-east-1.s3.us-east-1.amazonaws.com/occurrence/2025-03-01/occurrence'

dbExecute(con, "CREATE VIEW gbif AS SELECT * FROM read_parquet('s3://gbif-open-data-us-east-1/occurrence/2025-03-01/occurrence.parquet/*')")

dat <- dbGetQuery(con,"SELECT * FROM gbif WHERE scientificname = 'Iris versicolor' ORDER BY RANDOM() LIMIT 10")

# Query the remote Parquet file stored in S3
query_result <- dbGetQuery(con, paste0("
  SELECT class, valid_scientific_name
  FROM '", s3_url, "'
  WHERE class = 'Aves'
  ORDER BY RANDOM()
  LIMIT 10
"))

query_result <- dbGetQuery(con, paste0("
  SELECT class, species
  FROM '", s3_url, "'
  WHERE class = 'Aves'
  ORDER BY RANDOM()
  LIMIT 10
"))

# View the result
print(query_result)

# Close the connection
dbDisconnect(con)
dat <- dbGetQuery(con,"SELECT * FROM atlas WHERE valid_scientific_name = 'Iris versicolor'")





library(duckdb)
library(gbifdb)
con <- dbConnect(duckdb())
dbExecute(con,"INSTALL httpfs; LOAD httpfs;")
pfile=paste("s3:/","gbif-open-data-us-east-1","occurrence",gbif_version(),"occurrence.parquet", "*", sep="/")
Sys.time()
dbGetQuery(con, paste0("COPY (SELECT * FROM read_parquet('",pfile,"') WHERE countrycode IN('CA') AND class !='Aves' LIMIT 10) TO '/home/frousseu/data/niches_climatiques/test.parquet' (FORMAT 'parquet');"))
Sys.time()





library(gbifdb)
library(dplyr)

keep <- c("species", "phylum", "class", "order", "eventdate", "countrycode", "stateprovince", "occurrencestatus","taxonrank","decimallatitude", "decimallongitude", "coordinateuncertaintyinmeters", "occurrencestatus", "institutioncode", "recordedby")

gbif <- gbif_remote(backend = "duckdb")


Sys.time()
g <- gbif |>
  filter(countrycode %in% c("CA", "US")) |> 
  head(10000) |>
  collect() |> 
  as.data.table()
Sys.time() 
table(g$stateprovince, useNA = "always")


Sys.time()
g <- gbif |>
  select(any_of(keep)) |>
  filter(countrycode %in% c("CA", "US")) |>
  filter(!class %in% c("Aves")) |>
  count(species) |>
  sample_n() |>
  collect() 
Sys.time() 


source("https://object-arbutus.cloud.computecanada.ca/bq-io/atlas/parquet/bq-atlas-parquet.R")
d <- duckdbfs::open_dataset("https://object-arbutus.cloud.computecanada.ca/bq-io/ebird/ebd_sampling_relJan-2025.parquet", tblname = "d")
d <- duckdbfs::open_dataset("https://object-arbutus.cloud.computecanada.ca/bq-io/gbif/gbif_2025-03-01.parquet", tblname = "d")

d <- d |>
       count(species) |>
       arrange(desc(n)) |>
       collect()
sum(table(d$n))  