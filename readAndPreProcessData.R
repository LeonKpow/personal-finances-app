#For testing purposes:
#setwd("C:/Users/wooge/OneDrive/Documents/GitHub/personal-finances-app")

# STEP 1 - read in the data
# ---------------------------------------------------------- #

#financials <- drive_get(id = drive_find(pattern = "Finances Tracking 2015")[[2]][[1]])
drive_auth(email = "gary.katselas@gmail.com")
sheets_auth(email = "gary.katselas@gmail.com")
financials <- drive_get(id = "1LSsQXhMyZgwH_IY14kbhFf2hU8rJzJPT23iwCGx1QUw") # unsure if this id changes

financials_NAB_plat <- read_sheet(financials, sheet = "NAB Platinum CC", col_types = "Dcccccdddd")
financials_GE <- read_sheet(financials, sheet = "GE Credit Card", col_types = "Dcccccdddd")
financials_NAB_CC <- read_sheet(financials, sheet = "NAB Credit Card", col_types = "Dcccccdddd")
financials_cash <- read_sheet(financials, sheet = "Cash", col_types = "Dcccccddddd")

# STEP 2 - adjust column names
# ---------------------------------------------------------- #
colnames(financials_NAB_plat) <- c("date", "detailedDescription", "counterparty", "category1", "category2", "category3", "debit", "credit", "undrawn", "drawn")
colnames(financials_GE) <- c("date", "detailedDescription", "counterparty", "category1", "category2", "category3", "debit", "credit", "undrawn", "drawn")
colnames(financials_NAB_CC) <- c("date", "detailedDescription", "counterparty", "category1", "category2", "category3", "debit", "credit", "undrawn", "drawn")
colnames(financials_cash) <- c("date", "detailedDescription", "counterparty", "category1", "category2", "category3", "debit", "credit", "totalDebitsToDate", "totalCreditsToDate", "netInflowToDate")

# STEP 3 - remove superfluous rows
# ---------------------------------------------------------- #
financials_NAB_plat <- financials_NAB_plat %>% filter(!is.na(date))
financials_GE <- financials_GE %>% filter(!is.na(date))
financials_NAB_CC <- financials_NAB_CC %>% filter(!is.na(date))
financials_cash <- financials_cash %>% filter(!is.na(date))

# STEP 4 - Replace numeric NA's with 0's and deal with numbers that have been stored as factors
# ------------------------------------------------------------------------------------------------- #
financials_NAB_plat$debit <- ifelse(is.na(financials_NAB_plat$debit), 0, financials_NAB_plat$debit)
financials_NAB_plat$credit <- ifelse(is.na(financials_NAB_plat$credit), 0, financials_NAB_plat$credit)
financials_NAB_plat$undrawn <- ifelse(is.na(financials_NAB_plat$undrawn), 0, financials_NAB_plat$undrawn)
financials_NAB_plat$drawn <- ifelse(is.na(financials_NAB_plat$drawn), 0, financials_NAB_plat$drawn)

financials_GE$debit <- ifelse(is.na(financials_GE$debit), 0, financials_GE$debit)
financials_GE$credit <- ifelse(is.na(financials_GE$credit), 0, financials_GE$credit)
financials_GE$undrawn <- ifelse(is.na(financials_GE$undrawn), 0, financials_GE$undrawn)
financials_GE$drawn <- ifelse(is.na(financials_GE$drawn), 0, financials_GE$drawn)

financials_NAB_CC$debit <- ifelse(is.na(financials_NAB_CC$debit), 0, financials_NAB_CC$debit)
financials_NAB_CC$credit <- ifelse(is.na(financials_NAB_CC$credit), 0, financials_NAB_CC$credit)
financials_NAB_CC$undrawn <- ifelse(is.na(financials_NAB_CC$undrawn), 0, financials_NAB_CC$undrawn)
financials_NAB_CC$drawn <- ifelse(is.na(financials_NAB_CC$drawn), 0, financials_NAB_CC$drawn)

financials_cash$debit <- ifelse(is.na(financials_cash$debit), 0, financials_cash$debit)
financials_cash$credit <- ifelse(is.na(financials_cash$credit), 0, financials_cash$credit)
financials_cash$totalDebitsToDate <- ifelse(is.na(financials_cash$totalDebitsToDate), 0, financials_cash$totalDebitsToDate)
financials_cash$totalCreditsToDate <- ifelse(is.na(financials_cash$totalCreditsToDate), 0, financials_cash$totalCreditsToDate)
financials_cash$netInflowToDate <- ifelse(is.na(financials_cash$netInflowToDate), 0, financials_cash$netInflowToDate)

# STEP 5 - Create transaction source field
# ------------------------------------------------------------------ #
financials_NAB_plat$transactionSource <- as.factor(c("NAB Platinum Credit Card"))
financials_GE$transactionSource <- as.factor(c("28 Degrees Credit Card"))
financials_NAB_CC$transactionSource <- as.factor(c("NAB Standard Credit Card"))
financials_cash$transactionSource <- as.factor(c("Cash"))


# STEP 6 - Remove accounting entries and aggregate transactions
# ------------------------------------------------------------------ #
# remove accounting balancing entries
financials_NAB_plat <- financials_NAB_plat[!(financials_NAB_plat$category1 == c("Opening Balance") | financials_NAB_plat$category1 == c("Credit Card Bill")), ]
financials_GE <- financials_GE[!(financials_GE$category1 == c("Opening Balance") | financials_GE$category1 == c("Credit Card Bill")), ]
financials_NAB_CC <- financials_NAB_CC[!(financials_NAB_CC$category1 == c("Opening Balance") | financials_NAB_CC$category1 == c("Credit Card Bill")), ]
financials_cash <- financials_cash[!(financials_cash$category1 == c("Opening Balance") | financials_cash$category1 == c("Credit Card Bill")), ]

# stack data sets
fieldsToRetain <- c("date", "detailedDescription", "counterparty", "category1", "category2", "category3", "debit", "credit", "transactionSource")
financials_combined <- rbind(financials_NAB_plat[, fieldsToRetain],
                             financials_GE[, fieldsToRetain],
                             financials_NAB_CC[, fieldsToRetain],
                             financials_cash[, fieldsToRetain])

# rename debit and credit fields
colnames(financials_combined)[colnames(financials_combined) == c("debit")] <- "inflow"
colnames(financials_combined)[colnames(financials_combined) == c("credit")] <- "outflow"

# create net inflow field
financials_combined$netInflow <- financials_combined$inflow - financials_combined$outflow

# order by date of entries
financials_combined <- financials_combined[order(financials_combined$date), ]

# create truncated monthly values
financials_combined$monthOfTransaction <- floor_date(financials_combined$date, unit = "month")

# declare fields as factors
financials_combined$counterparty <- as.factor(financials_combined$counterparty)
financials_combined$category1 <- as.factor(financials_combined$category1)
financials_combined$category2 <- as.factor(financials_combined$category2)
financials_combined$category3 <- as.factor(financials_combined$category3)

# STEP 7 - determine the min/max dates in the data as well as most recent month of data possible
# ------------------------------------------------------------------------------------------------- #
minDateInData <- min(financials_combined$date)
maxDateInData <- max(financials_combined$date)

currentDate <- Sys.Date()
latestMonth = floor_date(currentDate, unit = "month")