# Setup --------------------------------------------------------------------------------------------
library(data.table)
library(lubridate)
library(jsonlite)
library(tidyr)
library(rvest)
library(httr)


# Variables ----------------------------------------------------------------------------------------
link <- "https://cardano-mainnet.blockfrost.io/api/v0/assets/"
token <- sample(c("mainnetghJieJ39JMUlumgj2HwbxtNmEcY3WzC9",
                  "mainnetYBrF03aUZhsJaexjOO6z7pI6vuNxW0sv",
                  "mainnetvPXz1w2LMEpWQ8VdU5U9TBRsCuuwmh0g"), 1)
policy_id <- "5a2cdc6e3aa9612fe4676672f443e7efd39c309d45e7919a4bf27750"
project <- "Boss Planet Real Estate"
time_now <- as_datetime(now())

RAR <- readRDS("data/RAR.rds")


# Functions ----------------------------------------------------------------------------------------
extract_num <- function(x) as.numeric(gsub("[^0-9\\-]+","",as.character(x)))

loj <- function (X = NULL, Y = NULL, onCol = NULL) {
  if (truelength(X) == 0 | truelength(Y) == 0) 
    stop("setDT(X) and setDT(Y) first")
  n <- names(Y)
  X[Y, `:=`((n), mget(paste0("i.", n))), on = onCol]
}

query <- function(page, url, project, sold) {
  httr::content(httr::POST(
    url = url, 
    body = list(
      search = "", 
      types = c("listing", "offer", "bundle"),
      project = project, 
      sort = list(`_id` = -1L), 
      priceMin = NULL, 
      priceMax = NULL, 
      page = page, 
      verified = TRUE, 
      nsfw = FALSE, 
      sold = sold, 
      smartContract = NULL
    ), 
    encode = "json"
  ), simplifyVector = TRUE)
}

query_n <- function(url, project, sold, n = "all") {
  if (n == "all") n <- query(1L, url, project, sold)[["count"]]
  out <- vector("list", n)
  for (i in seq_len(n)) {
    out[[i]] <- query(i, url, project, sold)[["results"]]
    if (length(out[[i]]) < 1L) return(out[seq_len(i - 1L)])
  }
  out
}

get_asset_data <- function(asset_id, blockforst_link = link, blockforst_token = token) {
  r <- httr::GET(url    = paste0(link, asset_id),
                 config = add_headers(project_id = token),
                 query  = list("asset" = asset_id))
  
  jsonlite::fromJSON(content(r, "text"))
}


# CNFT listings ------------------------------------------------------------------------------------
api_link_cnft <- "https://api.cnft.io/market/listings"

.CNFT <- query_n(api_link_cnft, project, sold = FALSE) |>
  lapply(data.table) |> rbindlist(fill = TRUE)

.CNFT[, link := paste0("https://cnft.io/token/", `_id`)]

# Initialize data.table
CNFT <- data.table(asset = NA, type = NA, price = NA,
                   last_offer = NA, sc = NA, market = NA, link = NA)

for (i in 1:nrow(.CNFT)) {
  CNFT <- rbindlist(list(CNFT, data.table(
    asset          = .CNFT[i, assets[[1]]$metadata$name],
    type           = .CNFT[i, type],
    price          = .CNFT[i, price/length(assets[[1]]$metadata$name)],
    last_offer     = .CNFT[i, offers],
    sc             = .CNFT[i, smartContractTxid],
    market         = "cnft.io",
    link           = .CNFT[i, link],
    district       = .CNFT[i, assets[[1]]$metadata$District]
  )), fill = TRUE)
}

CNFT <- CNFT[2:nrow(CNFT)] # Clear first row from initialization
CNFT[, price        := price/10**6]
CNFT[, sc           := ifelse(is.na(sc), "no", "yes")]

for (i in 1:nrow(CNFT)) {
  .offers <- CNFT[i, last_offer[[1]]]
  if (nrow(.offers) == 0) {
    CNFT[i, last_offer := NA]
  } else {
    CNFT[i, last_offer := max(.offers$offer/10**6)]
  }
}

CNFT[, xcoord := extract_num(strsplit(asset, ",")[[1]][1]), 1:nrow(CNFT)]
CNFT[, ycoord := extract_num(strsplit(asset, ",")[[1]][2]), 1:nrow(CNFT)]


# CNFT sales ---------------------------------------------------------------------------------------
.CNFTS <- query_n(api_link_cnft, project, sold = TRUE, n = 11) |>
  lapply(data.table) |> rbindlist(fill = TRUE)

.CNFTS <- .CNFTS[!is.na(soldAt)]

if(nrow(.CNFTS) != 0) {
  # Initialize data.table
  CNFTS <- data.table(asset = NA, price = NA, market = NA, sold_at = NA)

  for (i in 1:nrow(.CNFTS)) {
    CNFTS <- rbindlist(list(CNFTS, data.table(
      asset          = .CNFTS[i, assets[[1]]$metadata$name],
      type           = .CNFTS[i, type],
      price          = .CNFTS[i, price/length(assets[[1]]$metadata$name)],
      market         = "cnft.io",
      sold_at        = .CNFTS[i, soldAt],
      district       = .CNFT[i, assets[[1]]$metadata$District]
    )), fill = TRUE)
  }

  CNFTS <- CNFTS[2:nrow(CNFTS)] # Clear first row from initialization
  CNFTS[, price         := price/10**6]
  CNFTS[, market        := "cnft.io"]
  CNFTS[, sold_at       := as_datetime(sold_at)]
  CNFTS[, sold_at_hours := difftime(time_now, sold_at, units = "hours")]
  CNFTS[, sold_at_days  := difftime(time_now, sold_at, units = "days")]
  CNFTS[, xcoord := extract_num(strsplit(asset, ",")[[1]][1]), 1:nrow(CNFTS)]
  CNFTS[, ycoord := extract_num(strsplit(asset, ",")[[1]][2]), 1:nrow(CNFTS)]

  CNFTS <- CNFTS[order(-sold_at), .(asset, type, price, district,
                                    sold_at, sold_at_hours, sold_at_days, market, xcoord, ycoord)]
  CNFTS <- CNFTS[sold_at_hours <= 24*3]
} else {
  CNFTS <- .CNFTS
}


# Extract information from jpg.store ---------------------------------------------------------------
# jpg.store/api/policy - all supported policies
# jpg.store/api/policy/[id]/listings - listings for a given policy
# jpg.store/api/policy/[id]/sales - sales for a given policy
api_link <- sprintf("jpg.store/api/policy/%s/listings", policy_id)

JPG <- data.table(fromJSON(rawToChar(GET(api_link)$content)))
JPG[, asset_id       := asset]
JPG[, link           := paste0("https://www.jpg.store/asset/", asset)]
JPG[, price          := price_lovelace]
JPG[, asset          := asset_display_name]
JPG[, price          := price/10**6]
JPG[, sc             := "yes"]
JPG[, market         := "jpg.store"]
JPG[, xcoord         := extract_num(strsplit(asset, ",")[[1]][1]), 1:nrow(JPG)]
JPG[, ycoord         := extract_num(strsplit(asset, ",")[[1]][2]), 1:nrow(JPG)]

loj(JPG, RAR[, .(asset, district)], "asset")

for (.asset_id in JPG[is.na(district), asset_id]) {
  JPG[asset_id == .asset_id, district := get_asset_data(asset_id)$onchain_metadata$District]
}

JPG <- JPG[, .(asset, type = "listing", price, district, last_offer = NA, sc, market, link,
               xcoord, ycoord)]


# JPG sales ----------------------------------------------------------------------------------------
api_link <- sprintf("jpg.store/api/policy/%s/sales", policy_id)

JPGS <- data.table(fromJSON(rawToChar(GET(api_link)$content)))
JPGS[, asset_id       := asset]
JPGS[, price          := price_lovelace]
JPGS[, asset          := asset_display_name]
JPGS[, price          := price/10**6]
JPGS[, market         := "jpg.store"]
JPGS[, sold_at        := as_datetime(purchased_at)]
JPGS[, sold_at_hours  := difftime(time_now, sold_at, units = "hours")]
JPGS[, sold_at_days   := difftime(time_now, sold_at, units = "days")]
JPGS[, xcoord         := extract_num(strsplit(asset, ",")[[1]][1]), 1:nrow(JPGS)]
JPGS[, ycoord         := extract_num(strsplit(asset, ",")[[1]][2]), 1:nrow(JPGS)]

loj(JPGS, RAR[, .(asset, district)], "asset")

for (.asset_id in JPGS[is.na(district), asset_id]) {
  JPGS[asset_id == .asset_id, district := get_asset_data(asset_id)$onchain_metadata$District]
}

JPGS <- JPGS[order(-sold_at), .(asset, type = "listings", price, district,
                                sold_at, sold_at_hours, sold_at_days, market, xcoord, ycoord)]
JPGS <- JPGS[sold_at_hours <= 24*3]


# Merge markets data -------------------------------------------------------------------------------
# Listings
DT <- rbindlist(list(CNFT, JPG), fill = TRUE, use.names = TRUE)

# Sales
DTS <- rbindlist(list(CNFTS, JPGS), fill = TRUE, use.names = TRUE)

# Add data collection timestamp
DT[, data_date := time_now]
DTS[, data_date := time_now]


# Add districs -------------------------------------------------------------------------------------
DT[, district_txt := factor(district, labels = paste("District", 1:max(district)),
                            levels = 1:max(district))]
DT[, price_rank   := as.numeric(as.integer(factor(price)))]
DT[, price_rank   := (price_rank-min(price_rank))/(max(price_rank)-min(price_rank))]

DTS[, district_txt := factor(district, labels = paste("District", 1:max(district)),
                             levels = 1:max(district))]
DTS[, price_rank   := as.numeric(as.integer(factor(price)))]
DTS[, price_rank   := (price_rank-min(price_rank))/(max(price_rank)-min(price_rank))]


# Save ---------------------------------------------------------------------------------------------
saveRDS(DT, file = "data/DT.rds")
saveRDS(DTS, file = "data/DTS.rds")


# Database evolution -------------------------------------------------------------------------------
DTE <- copy(DT)
.file_name <- "data/DTE.rds"
if (file.exists(.file_name)) {
  cat("File data/DTE exists:", file.exists(.file_name), "\n")
  DTE_old <- readRDS(.file_name)
  DTE <- rbindlist(list(DTE, DTE_old), use.names = TRUE)
  DTE <- DTE[difftime(time_now, data_date, units = "hours") <= 24] # Only retain last 24 hours
}
saveRDS(DTE, file = .file_name)
