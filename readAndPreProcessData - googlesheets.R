# STEP 1 - read in the data
# ---------------------------------------------------------- #
financials <- gs_title("Finances Tracking 2015/16")

financials_NAB_plat <- gs_read(financials, ws = "NAB Platinum CC")
financials_GE <- gs_read(financials, ws = "GE Credit Card")
financials_NAB_CC <- gs_read(financials, ws = "NAB Credit Card")
financials_cash <- gs_read(financials, ws = "Cash")

# STEP 2 - adjust column names
# ---------------------------------------------------------- #
colnames(financials_NAB_plat) <- c("date", "detailedDescription", "counterparty", "category1", "category2", "category3", "debit", "credit", "undrawn", "drawn")
colnames(financials_GE) <- c("date", "detailedDescription", "counterparty", "category1", "category2", "category3", "debit", "credit", "undrawn", "drawn")
colnames(financials_NAB_CC) <- c("date", "detailedDescription", "counterparty", "category1", "category2", "category3", "debit", "credit", "undrawn", "drawn")
colnames(financials_cash) <- c("date", "detailedDescription", "counterparty", "category1", "category2", "category3", "debit", "credit", "totalDebitsToDate", "totalCreditsToDate", "netInflowToDate")

# STEP 3 - remove superfluous rows
# ---------------------------------------------------------- #
financials_NAB_plat <- financials_NAB_plat[!(financials_NAB_plat$date==""|is.na(financials_NAB_plat$date)), ]
financials_GE <- financials_GE[!(financials_GE$date==""|is.na(financials_GE$date)), ]
financials_NAB_CC <- financials_NAB_CC[!(financials_NAB_CC$date==""|is.na(financials_NAB_CC$date)), ]
financials_cash <- financials_cash[!(financials_cash$date==""|is.na(financials_cash$date)), ]

# STEP 4 - convert date field from string to dates
# ---------------------------------------------------------- #
financials_NAB_plat$date <- as.Date(financials_NAB_plat$date, "%d/%m/%Y")
financials_GE$date <- as.Date(financials_GE$date, "%d/%m/%Y")
financials_NAB_CC$date <- as.Date(financials_NAB_CC$date, "%d/%m/%Y")
financials_cash$date <- as.Date(financials_cash$date, "%d/%m/%Y")

# STEP 5 - Replace numeric NA's with 0's and deal with numbers that have been stored as factors
# ------------------------------------------------------------------------------------------------- #
financials_NAB_plat$debit <- as.numeric(gsub(",", "", as.character(financials_NAB_plat$debit)))
financials_NAB_plat$debit <- ifelse(is.na(financials_NAB_plat$debit), 0, financials_NAB_plat$debit)
financials_NAB_plat$credit <- as.numeric(gsub(",", "", as.character(financials_NAB_plat$credit)))
financials_NAB_plat$credit <- ifelse(is.na(financials_NAB_plat$credit), 0, financials_NAB_plat$credit)
financials_NAB_plat$undrawn <- as.numeric(gsub(",", "", as.character(financials_NAB_plat$undrawn)))
financials_NAB_plat$undrawn <- ifelse(is.na(financials_NAB_plat$undrawn), 0, financials_NAB_plat$undrawn)
financials_NAB_plat$drawn <- as.numeric(gsub(",", "", as.character(financials_NAB_plat$drawn)))
financials_NAB_plat$drawn <- ifelse(is.na(financials_NAB_plat$drawn), 0, financials_NAB_plat$drawn)

financials_GE$debit <- as.numeric(gsub(",", "", as.character(financials_GE$debit)))
financials_GE$debit <- ifelse(is.na(financials_GE$debit), 0, financials_GE$debit)
financials_GE$credit <- as.numeric(gsub(",", "", as.character(financials_GE$credit)))
financials_GE$credit <- ifelse(is.na(financials_GE$credit), 0, financials_GE$credit)
financials_GE$undrawn <- as.numeric(gsub(",", "", as.character(financials_GE$undrawn)))
financials_GE$undrawn <- ifelse(is.na(financials_GE$undrawn), 0, financials_GE$undrawn)
financials_GE$drawn <- as.numeric(gsub(",", "", as.character(financials_GE$drawn)))
financials_GE$drawn <- ifelse(is.na(financials_GE$drawn), 0, financials_GE$drawn)

financials_NAB_CC$debit <- as.numeric(gsub(",", "", as.character(financials_NAB_CC$debit)))
financials_NAB_CC$debit <- ifelse(is.na(financials_NAB_CC$debit), 0, financials_NAB_CC$debit)
financials_NAB_CC$credit <- as.numeric(gsub(",", "", as.character(financials_NAB_CC$credit)))
financials_NAB_CC$credit <- ifelse(is.na(financials_NAB_CC$credit), 0, financials_NAB_CC$credit)
financials_NAB_CC$undrawn <- as.numeric(gsub(",", "", as.character(financials_NAB_CC$undrawn)))
financials_NAB_CC$undrawn <- ifelse(is.na(financials_NAB_CC$undrawn), 0, financials_NAB_CC$undrawn)
financials_NAB_CC$drawn <- as.numeric(gsub(",", "", as.character(financials_NAB_CC$drawn)))
financials_NAB_CC$drawn <- ifelse(is.na(financials_NAB_CC$drawn), 0, financials_NAB_CC$drawn)

financials_cash$debit <- as.numeric(gsub(",", "", as.character(financials_cash$debit)))
financials_cash$debit <- ifelse(is.na(financials_cash$debit), 0, financials_cash$debit)
financials_cash$credit <- as.numeric(gsub(",", "", as.character(financials_cash$credit)))
financials_cash$credit <- ifelse(is.na(financials_cash$credit), 0, financials_cash$credit)
financials_cash$totalDebitsToDate <- as.numeric(gsub(",", "", as.character(financials_cash$totalDebitsToDate)))
financials_cash$totalDebitsToDate <- ifelse(is.na(financials_cash$totalDebitsToDate), 0, financials_cash$totalDebitsToDate)
financials_cash$totalCreditsToDate <- as.numeric(gsub(",", "", as.character(financials_cash$totalCreditsToDate)))
financials_cash$totalCreditsToDate <- ifelse(is.na(financials_cash$totalCreditsToDate), 0, financials_cash$totalCreditsToDate)
financials_cash$netInflowToDate <- as.numeric(gsub(",", "", as.character(financials_cash$netInflowToDate)))
financials_cash$netInflowToDate <- ifelse(is.na(financials_cash$netInflowToDate), 0, financials_cash$netInflowToDate)

# STEP 6 - Create transaction source field
# ------------------------------------------------------------------ #
financials_NAB_plat$transactionSource <- as.factor(c("NAB Platinum Credit Card"))
financials_GE$transactionSource <- as.factor(c("28 Degrees Credit Card"))
financials_NAB_CC$transactionSource <- as.factor(c("NAB Standard Credit Card"))
financials_cash$transactionSource <- as.factor(c("Cash"))


# STEP 7 - Remove accounting entries and aggregate transactions
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
financials_combined$monthOfTransaction <- as.Date(format(financials_combined$date, format = "%Y-%m-01"))

# declare fields as factors
financials_combined$counterparty <- as.factor(financials_combined$counterparty)
financials_combined$category1 <- as.factor(financials_combined$category1)
financials_combined$category2 <- as.factor(financials_combined$category2)
financials_combined$category3 <- as.factor(financials_combined$category3)

# STEP 8 - determine the min/max dates in the data as well as most recent month of data possible
# ------------------------------------------------------------------------------------------------- #
minDateInData <- min(financials_combined$date)
maxDateInData <- max(financials_combined$date)

currentDate <- Sys.Date()

if (currentDate == as.Date(timeLastDayInMonth(currentDate))){
  latestMonth = as.Date(format(currentDate, format = "%Y-%m-01"))
} else {
  latestMonth = as.Date(as.yearmon(currentDate) - (1/12), frac = 0)
}