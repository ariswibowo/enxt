## PD modelling #############################################################################################################
############################################################################################################################

# Parameters ################################################################################################################
dev_cutoff_date <- as.Date("2023-12-31")
reporting_date <- as.Date("2023-12-31")

decktype <- "Simulation"
deckname <- "ELY - Historical PD MEV Dec 23 - R10 1"

historical_pd_deck_calculation_id <- toupper("854becb8-0578-40e4-be90-0549adad7c4e")

mevactualsvariant <- "BASE"
mevdataset <- "UAT 2023 Q4"

segmentname <- "Car"
# Car - 42 Months
# Dana Syariah - 18 Months
# Pradana - 18 Months (proxy ke Dana syariah)
# Multi Product - 2 Years
# Motorcycle Restru - 2 Years
# Dana Syariah Restru - 2 Years
# Motorcycle - 3 Years
# Agriculture - 3 Years
# Car Restru - 3 Years
# Used Motorcycle - 3 Years

directory <- "~/BAF-Modelling/"
setwd(directory)

library(readxl)
library(IDPmisc)
library(odbc)

#MevMatrix <- read_excel("/home/rstudio/BAF-Modelling/BAF-MEF_Correlation_1.xlsx")
#MevMatrix <- NaRV.omit(MevMatrix)

ExpectedTrend <- read_excel("/home/rstudio/BAF-Modelling/BAF-MEF_Correlation_2.xlsx")
ExpectedTrend <- NaRV.omit(ExpectedTrend)


# correlation_cap <- 0.80

# CorrelationThreshold 
CorrelationThreshold <- 0.50
#correlation_floor <- 0.50

# Confidencelevel 90%
adfnonestat.criticalv <- -1.60 
adfdriftstat.criticalv <- -2.62 
adftrendstat.criticalv <- -3.24 
kpsslevelstat.criticalv <- 0.347
kpsstrendstat.criticalv <- 0.119

############################################################################################################################
## Install Packages 
############################################################################################################################
packages <- c("odbc","DescTools","base","tidyr","reshape2","xlsx","openxlsx",
              "readxl","plyr","dplyr","rlang","stringr","car","CADFtest","MuMIn",
              "lmtest","jsonlite","magrittr","rjson","zoo","rlist","IDPmisc",
              "caret","glmnet","svDialogs","snow","parallel","rowr","MLmetrics","psych","stringdist")
suppressMessages(invisible(lapply(packages, library, character.only = TRUE)))

############################################################################################################################
## Get Data 
############################################################################################################################
conFdr <- dbConnect(
  odbc(),
  Driver = "PostgreSQL",
  Server =  "172.16.7.12", 
  Database = "Fdr",
  UID = "Empyrean", 
  PWD = "Durian@EMP5000", 
  Port = 5432
)

conIfrs9Ui <- dbConnect(
  odbc(),
  Driver = "PostgreSQL",
  Server =  "172.16.7.12",
  Database = "Ui",
  UID = "Empyrean", 
  PWD = "Durian@EMP5000",
  Port = 5432
)

conHistoricalPd <- dbConnect(
  odbc(),
  Driver = "PostgreSQL",
  Server =  "172.16.7.12",
  Database = "HistoricalPd",
  UID = "Empyrean", 
  PWD = "Durian@EMP5000",
  Port = 5432
)

conModels <- dbConnect(
  odbc(),
  Driver = "PostgreSQL",
  Server =  "172.16.7.12",
  Database = "Models",
  UID = "Empyrean", 
  PWD = "Durian@EMP5000",
  Port = 5432
)

#Fetch the data 
src.Entities <-
  dbGetQuery(conFdr,' SELECT * FROM public."Entities"')

src.Decks <-
  dbGetQuery(conIfrs9Ui,' SELECT * FROM public."Decks"')
src.DeckTypes <-
  dbGetQuery(conIfrs9Ui,' SELECT * FROM public."DeckTypes"')
src.DeckHistoricalPdCalculations <-
  dbGetQuery(conIfrs9Ui,' SELECT * FROM public."DeckCalculations"')

src.Calculations <-
  dbGetQuery(conHistoricalPd, ' SELECT * FROM public."Calculations"')
src.ResultTransitionMatrix <-
  dbGetQuery(conHistoricalPd, ' SELECT * FROM public."ResultTransitionMatrix"')

src.EcFactors <-
  dbGetQuery(conFdr, 'SELECT * FROM public."EcValues"')
src.EcValues <-
  dbGetQuery(conFdr, 'SELECT * FROM public."EcFactors"')
src.EcVariants <-
  dbGetQuery(conFdr, 'SELECT * FROM public."EcVariants"')
src.EcDatasets <-
  dbGetQuery(conFdr, 'SELECT * FROM public."EcDatasets"')

#src.PdMethods <-
#  dbGetQuery(conPdModels, ' SELECT * FROM public."PdMethods"')

src.PdVersions <-
  dbGetQuery(conModels, ' SELECT * FROM public."Versions"')
src.PdApproaches <-
  dbGetQuery(conModels, ' SELECT * FROM public."GetApproachSegment"()')

#src.PdApproachSegments <-
# dbGetQuery(conPdModels, ' SELECT * FROM public."PdApproachSegments"')

#Fetch Transition Matrix
HistoricalSource <-
  merge(
    merge(
      merge(src.Decks,
            src.DeckHistoricalPdCalculations,
            by = "DeckId"),
      src.Calculations,
      by = "CalculationId"
    ),
    src.ResultTransitionMatrix,
    by =  "CalculationId"
  )

HistoricalSource$CreatedBy.x <- NULL
HistoricalSource$CreatedWhen.x <- NULL
HistoricalSource$CreatedBy.y <- NULL
HistoricalSource$CreatedWhen.y <- NULL
HistoricalSource$LastModifiedBy.y <- NULL
HistoricalSource$LastModifiedWhen.y <- NULL
HistoricalSource$LastModifiedBy.x <- NULL
HistoricalSource$LastModifiedWhen.x <- NULL

HistoricalSource <-
  select(
    HistoricalSource,
    c(
      CalculationId,
      Entity,
      ReportingDate = "ReportingDate.x",
      DeckName = "Name",
      Type,
      SegmentId,
      SegmentName,
      FromObservationDate,
      ToObservationDate,
      FromBucketType,
      ToBucketType,
      NumContracts,
      FromOutstandingAmount,
      ToOutstandingAmount
    )
  )

EconomicValueSource <-
  merge(
    merge(
      merge(src.EcFactors,
            src.EcVariants,
            by = "VariantId"),
      src.EcValues,
      by = "FactorId"
    ),
    src.EcDatasets,
    by =  "DatasetId"
  )

EconomicValueSource <-
  select(EconomicValueSource, c(DatasetName = "Name", Mev = "Name.y", Variant = "Name.x", "ObservationDate", "Value"))

EconomicValueSource <-
  arrange(EconomicValueSource, Variant,ObservationDate, DatasetName,ObservationDate)


############################################################################################################################
## Fetch some configuration items from the database 
############################################################################################################################
calculation_id <- (src.DeckHistoricalPdCalculations %>% filter(CalculationId == historical_pd_deck_calculation_id) %>% select(CalculationId))[1,1]
segment_id <- (HistoricalSource %>% filter(DeckName == deckname)  %>% filter(as.Date(ReportingDate) == reporting_date) %>%filter(CalculationId == calculation_id) %>% filter(Type == decktype) %>% filter(trimws(SegmentName) == segmentname) %>% distinct(SegmentId))[1,1]
version_id <- toupper(fromJSON((src.Decks %>% filter(Name == deckname) %>% filter(as.Date(ReportingDate) == reporting_date))[1,7])$`versions`$`pdModelVersionId`)
#One approach one method please!
#approach_id_list <- src.PdApproachSegments %>% filter(SegmentId == segment_id) %>% select(ApproachId)
#approach_id <- toupper(src.PdApproaches %>% filter(VersionId == version_id) %>% filter(ApproachId %in% approach_id_list[,1]) %>% select(ApproachId))
#measurement_unit <- fromJSON((src.PdMethods %>% filter(ApproachId == approach_id))[1,6])$`configuration`$measurementUnit
approach_id <- toupper(src.PdApproaches %>% filter(toupper(VersionId) == version_id) %>% filter(toupper(SegmentId) == segment_id) %>% select(ApproachId))
measurement_unit <- (src.PdApproaches %>% filter(toupper(ApproachId) == approach_id) %>% filter(toupper(SegmentId) == segment_id) %>% select(MeasurementUnit))

############################################################################################################################
## Functions
############################################################################################################################
#Log log(x,basevalue)
f.log <- function(data, columns, basevalue) {
  data %>%
    mutate_at(columns, funs(log = log(., base = basevalue))) %>%
    rename_at(vars(matches('_log')), funs(sub('(\\w+)_(\\w+)', '\\2_\\1', .))) %>%
    rename_at(vars(matches('log_')), funs(sub('log', paste0(
      'log', as.character(basevalue)
    ), .))) %>%
    rename_at(vars(matches('log2.71828182845905')), funs(sub('log2.71828182845905', 'ln', .)))
}

#Inverse 1/x
f.inverse <- function(data, columns) {
  data %>%
    mutate_at(columns, funs(inv = 1 / .)) %>%
    rename_at(vars(matches('inv')), funs(sub('(\\w+)_(\\w+)', '\\2_\\1', .)))
}

#Power x^exponent
f.power <- function(data, columns, exponent) {
  data %>%
    mutate_at(columns, funs(power = . ^ exponent)) %>%
    rename_at(vars(matches('_power')), funs(sub('(\\w+)_(\\w+)', '\\2_\\1', .))) %>%
    rename_at(vars(matches('power_')), funs(sub(
      'power', paste0('power', as.character(exponent)), .
    )))
}

#Exponential EXP(x)
f.exponent <- function(data, columns) {
  data %>%
    mutate_at(columns, funs(exponent = exp(.))) %>%
    rename_at(vars(matches('exponent')), funs(sub('(\\w+)_(\\w+)', '\\2_\\1', .)))
}

#Exponential EXP(x)
f.inversexponent <- function(data, columns) {
  data %>%
    mutate_at(columns, funs(inex = 1/exp(.))) %>%
    rename_at(vars(matches('inex')), funs(sub('(\\w+)_(\\w+)', '\\2_\\1', .)))
}

#Period Lag
f.lag <- function(data, columns, rows) {
  data %>%
    mutate_at(columns, funs(lag = lag(., rows))) %>%
    rename_at(vars(matches('_lag')), funs(sub('(\\w+)_(\\w+)', '\\2_\\1', .))) %>%
    rename_at(vars(matches('lag_')), funs(sub('lag', paste0(
      'lag', as.character(rows)
    ), .)))
}

#Difference from prior period
f.difference <- function(data, columns, rows) {
  data %>%
    mutate_at(columns, funs(diff = (.) - lag(., rows))) %>%
    rename_at(vars(matches('_diff')), funs(sub('(\\w+)_(\\w+)', '\\2_\\1', .))) %>%
    rename_at(vars(matches('diff_')), funs(sub(
      'diff', paste0('diff', as.character(rows)), .
    )))
}

#Growth rate ((x-past x)/past x)
f.growth <- function(data, columns, rows) {
  data %>%
    mutate_at(columns, funs(grw = ((.) - lag(., rows)) / lag(., rows))) %>%
    rename_at(vars(matches('_grw')), funs(sub('(\\w+)_(\\w+)', '\\2_\\1', .))) %>%
    rename_at(vars(matches('grw_')), funs(sub('grw', paste0(
      'grw', as.character(rows)), .
    )))
}

#Standardize ((x-mean(x))/sd(x))
f.standardize <- function(data, columns) {
  data %>%
    mutate_at(columns, funs(stdze = (. - mean(.))/sd(.))) %>%
    rename_at(vars(matches('stdze')), funs(sub('(\\w+)_(\\w+)', '\\2_\\1', .)))
}

#Logit LN(x/(1-x))
f.logit <- function(data, columns) {
  data %>%
    mutate_at(columns, funs(logit = log(. / (1 - .)))) %>%
    rename_at(vars(matches('logit')), funs(sub('(\\w+)_(\\w+)', '\\2_\\1', .)))
}

f.addmonthstodate <- function(date, n = 0){
  if (n == 0){return(date)}
  if (n %% 1 != 0){stop("Input Error: argument 'n' must be an integer.")}
  
  # Check to make sure we have a standard Date format
  if (class(date) == "character"){date = as.Date(date)}
  
  # Turn the year, month, and day into numbers so we can play with them
  y = as.numeric(substr(as.character(date),1,4))
  m = as.numeric(substr(as.character(date),6,7))
  d = as.numeric(substr(as.character(date),9,10))
  
  if (d==28 && m==2)
  {
    d = 31
  }
  
  if (d==30 && m %in% c(4, 6, 9, 11))
  {
    d = 31
  }  
  
  # Run through the computation
  i = 0
  # Adding months
  if (n > 0){
    while (i < n){
      m = m + 1
      if (m == 13){
        m = 1
        y = y + 1
      }
      i = i + 1
    }
  }
  # Subtracting months
  else if (n < 0){
    while (i > n){
      m = m - 1
      if (m == 0){
        m = 12
        y = y - 1
      }
      i = i - 1
    }
  }
  
  # If past 28th day in base month, make adjustments for February
  if (d > 28 & m == 2){
    # If it's a leap year, return the 29th day
    if ((y %% 4 == 0 & y %% 100 != 0) | y %% 400 == 0){d = 29}
    # Otherwise, return the 28th day
    else{d = 28}
  }
  
  #Make Adjustments for End of Month
  else if (d == 30){if (m %in% c(1, 3, 5, 7, 8, 10, 12) == FALSE){d = 30}}
  else if (d == 30){if (m %in% c(4, 6, 9, 11) == FALSE){d = 31}}
  else if (d == 31){if (m %in% c(1, 3, 5, 7, 8, 10, 12) == FALSE){d = 30}}
  else if (d == 31){if (m %in% c(4, 6, 9, 11) == FALSE){d = 31}}
  
  # Turn year, month, and day into strings and put them together to make a Date
  y = as.character(y)
  
  # If month is single digit, add a leading 0, otherwise leave it alone
  if (m < 10){m = paste('0', as.character(m), sep = '')}
  else{m = as.character(m)}
  
  # If day is single digit, add a leading 0, otherwise leave it alone
  if (d < 10){d = paste('0', as.character(d), sep = '')}
  else{d = as.character(d)}
  
  # Put them together and convert return the result as a Date
  return(as.Date(paste(y,'-',m,'-',d, sep = '')))
}

f.finddatecolumn <- function (dataset, date) {
  which(names(dataset) ==
          as.character(format(
            as.Date(date,
                    format = '%Y-%m-%d',
                    origin = '18991230'),
            '%Y-%m-%d'
          )))
}


############################################################################################################################
## Start Process - Data Preparation - ODR 
############################################################################################################################
#Filter dataset
HistSrc.filtered <-
  HistoricalSource %>% filter(CalculationId == calculation_id) %>% filter(trimws(SegmentName) == segmentname)

if (measurement_unit == "NumContracts")
{
  Odr <-
    HistSrc.filtered %>% 
    filter(CalculationId == calculation_id) %>%
    group_by(FromObservationDate, ToObservationDate, FromBucketType, ToBucketType) %>%
    summarise(n = sum(NumContracts)) %>%
    arrange(FromObservationDate, FromBucketType, ToBucketType) %>%
    group_by(FromObservationDate) %>%
    mutate(sum = sum(n)) %>%
    ungroup() %>%
    mutate(odr = n/sum) %>%
    filter(FromBucketType == "Standard" & ToBucketType == "Default") %>%
    ungroup() %>%
    select(ObservationDate = ToObservationDate, odr)
}

if (measurement_unit == "OutstandingAmount")
{
  Odr <-
    HistSrc.filtered %>% 
    filter(CalculationId == calculation_id) %>%
    group_by(FromObservationDate, ToObservationDate, FromBucketType, ToBucketType) %>%
    summarise(n = sum(FromOutstandingAmount)) %>%
    arrange(FromObservationDate, FromBucketType, ToBucketType) %>%
    group_by(FromObservationDate) %>%
    mutate(sum = sum(n)) %>%
    ungroup() %>%
    mutate(odr = n/sum) %>%
    filter(FromBucketType == "Standard" & ToBucketType == "Default") %>%
    ungroup() %>%
    select(ObservationDate = ToObservationDate, odr)
}

Odr <-
  f.logit(Odr, c("odr"))

Odr$logit[is.infinite(Odr$logit)] <- 4.59511985013459

# Import Odr for AR Transfer
if (segmentname == "Indirect AR Transfer Loan")
  Odr <- read_excel("C:/Users/adanggoro/Documents/RResult/Input/Odr for AR Transfer MA6M.xlsx")
Odr <- NaRV.omit(Odr)

# Stop Processing if Odr observation is less than 12 rows
if (nrow(Odr) <= 6) {
  message("Stop modelling process due to an insufficient historical data") 
}
#OdrTemp <- Odr
#OdrTemp <- ldply(OdrTemp, rbind)
xlsx::write.xlsx2(Odr, 
                  file = paste0(directory, "01-Odr-",segmentname,".xlsx"), 
                  sheetName= paste0("Odr-",segmentname),
                  col.names=TRUE,
                  row.names=TRUE,
                  append=FALSE)

#############################################################################################################################
## Start Process - Data Preparation - MEVS 
############################################################################################################################
# Start processing the MEVs
ec_effective_date_dataset <- as.Date((src.EcDatasets %>% filter(Name %in% mevdataset))$EffectiveDate)

mevfilterlist <- 
  EconomicValueSource %>% filter(DatasetName %in% mevdataset) %>% filter(Variant == mevactualsvariant) %>% filter(!Mev %like% "%lag%" & !Mev %in% "WTI") 


mevfilterlist <- unique(mevfilterlist$Mev) %>% unlist()

EV.filtered <-
  EconomicValueSource %>% filter(DatasetName %in% mevdataset) %>% filter(Variant == mevactualsvariant) %>% filter(Mev %in% mevfilterlist)

EV.filtered$DatasetName <- NULL

Ev.widedataset <- recast(EV.filtered,
                         ObservationDate ~ Mev,
                         fun = mean,
                         measure.var = c("Value"))

## take actual mef only based on effective date of dataset
full.dataset <- subset(Ev.widedataset,ObservationDate <= as.Date(ec_effective_date_dataset))

full.dataset <- full.dataset[order(full.dataset$ObservationDate,decreasing=FALSE),]

full.dataset <- merge(Odr,
                      full.dataset,
                      by = "ObservationDate",
                      all.y = TRUE)

# Column names of those we want transformed, numeric only please!
column.names <- colnames(full.dataset[,!colnames(full.dataset) %in% c('ObservationDate','logit','odr','difflogit')])


# Difference (dataset,columns,number of rows behind) 1,2,3,4
full.dataset <-
  f.difference(full.dataset, column.names, 1)
full.dataset <-
  f.difference(full.dataset, column.names, 2)
full.dataset <-
  f.difference(full.dataset, column.names, 3)
full.dataset <-
  f.difference(full.dataset, column.names, 6)
full.dataset <-
  f.difference(full.dataset, column.names, 9)
full.dataset <-
  f.difference(full.dataset, column.names, 12)

# Growth Rate (dataset,columns,number of rows behind) 1,2,3,4
full.dataset <-
  f.growth(full.dataset, column.names, 1)
full.dataset <-
  f.growth(full.dataset, column.names, 2)
full.dataset <-
  f.growth(full.dataset, column.names, 3)
full.dataset <-
  f.growth(full.dataset, column.names, 6)
full.dataset <-
  f.growth(full.dataset, column.names, 9)
full.dataset <-
  f.growth(full.dataset, column.names, 12)

# Refetch Column Names
column.names <- colnames(full.dataset[,!colnames(full.dataset) %in% c('ObservationDate','odr','logit','difflogit')])

# Lag (dataset,columns,number of rows behind)
full.dataset <-
  f.lag(full.dataset, column.names, 1)
full.dataset <-
  f.lag(full.dataset, column.names, 2)
full.dataset <-
  f.lag(full.dataset, column.names, 3)
full.dataset <-
  f.lag(full.dataset, column.names, 6)
full.dataset <-
  f.lag(full.dataset, column.names, 9)
full.dataset <-
  f.lag(full.dataset, column.names, 12)

#Log (dataset,columns,basevalue)
full.dataset <-   
  f.log(full.dataset, column.names, exp(1))

# Refetch Column Names
column.names <- colnames(full.dataset[,!colnames(full.dataset) %in% c('ObservationDate','odr','logit','difflogit')])

# Stored temporary value before standardized
#full.dataset_temp <- full.dataset 

# Filter based on ODR observation date
startenddate <- Odr %>% filter(row_number() %in% c(1, n())) %>% select(ObservationDate) %>% as.data.frame()
#full.dataset <- subset(full.dataset,ObservationDate <= as.Date(ec_effective_date_dataset))

# Standardization with 5-year Long Run average
# full.dataset <- f.standardize(full.dataset, column.names) 
full.dataset <- stdize(full.dataset,center = TRUE, scale = TRUE, omit.cols = c("ObservationDate","odr","logit"))

# Filter based on ODR observation date
full.dataset <- subset(full.dataset,ObservationDate >= as.Date(startenddate[1,1]))
full.dataset <- subset(full.dataset,ObservationDate <= as.Date(startenddate[2,1]))

# Remove NaN/Inf/Null/NA
removenan <- sapply(full.dataset, function(x) any(is.nan(x)))
full.dataset <- full.dataset[,!removenan]

removeinf <- sapply(full.dataset, function(x) any(is.infinite(x)))
full.dataset <- full.dataset[,!removeinf]

removenull <- sapply(full.dataset, function(x) any(is.null(x)))
full.dataset <- full.dataset[,!removenull]

# Remove ColSum = 0
full.dataset <- full.dataset[,which(!apply(full.dataset == 0,2,all))]
#full.dataset <- NaRV.omit(full.dataset)

# Remove duplicate values
full.dataset <-  Filter(var,full.dataset)

# Remove Near Zero Variance values
# dropnzv <-
#   as.data.frame(nearZeroVar(full.dataset,saveMetrics = TRUE,names = TRUE, foreach = TRUE, allowParallel = FALSE))
# dropnzv <- as.list(rownames(subset(dropnzv,freqRatio > 1)))
# full.dataset <- full.dataset[ , !(names(full.dataset) %in% dropnzv)]

names(full.dataset) <- gsub(names(full.dataset), pattern = "z.", replacement = "")

development.dataset <- full.dataset %>% filter(as.Date(ObservationDate) <= dev_cutoff_date)
validation.dataset <- full.dataset %>% filter(as.Date(ObservationDate) > dev_cutoff_date)

mevvariable <- names(development.dataset)[4:ncol(development.dataset)]

xlsx::write.xlsx2(full.dataset, 	
                  file = paste0(directory, "02-full_dataset-",segmentname,".xlsx"), 	
                  sheetName= paste0("full_dataset-",segmentname),	
                  col.names=TRUE,	
                  row.names=TRUE,	
                  append=FALSE)

############################################################################################################################
## Single Factor Analysis 
############################################################################################################################
## Correlation
correlation.results <- as.data.frame(cor(development.dataset[, names(development.dataset) %in% c(mevvariable)],
                                         development.dataset$logit,
                                         method = "pearson"))

correlation.results <- correlation.results %>% 
  mutate(var = rownames(correlation.results)) %>% 
  rename(Correlation = V1,MEV = var) %>% 
  select(MEV, everything())


# Filter expected trend for one segment
ExpectedTrend <- ExpectedTrend %>%
  select(MEV, segmentname)
colnames(ExpectedTrend)[2] <- "ExpectedTrend"

# Expected trend for all transformation MEV
right = function (string, char) {
  substr(string,nchar(string)-(char-1),nchar(string))
}

ExpectedTrend.full <-data.frame(MEV = correlation.results$MEV,
                                ExpectedTrend = ExpectedTrend[amatch(right(correlation.results$MEV,11), ExpectedTrend$MEV, maxDist = 8),2])

removenull <- sapply(ExpectedTrend.full, function(x) any(is.null(x)))
ExpectedTrend.full <- ExpectedTrend.full[,!removenull]

# Merge trend with correlation
SingleFactorAnalysis <- merge(correlation.results,
                              ExpectedTrend.full,
                              by = "MEV",
                              all = TRUE)

SingleFactorAnalysis <- SingleFactorAnalysis[,c(1,3,2)]

# Workaround for data prep!!!!!
#SingleFactorAnalysis[is.na(SingleFactorAnalysis)] <- 'Negative'

development.dataset.pvalue <- development.dataset[4:ncol(development.dataset)]

# P-value
pvalue <- vector("numeric", length(colnames(development.dataset[which(names(development.dataset) %in% c(mevvariable))])))
for (i in 1:length(colnames(development.dataset[which(names(development.dataset) %in% c(mevvariable))])))
{
  pvalue[i] <-
    cor.test(unlist(development.dataset.pvalue[i],use.names = TRUE),development.dataset$logit)$p.value
}


pvalue <- as.data.frame(cbind(colnames(development.dataset[which(names(development.dataset) %in% c(mevvariable))]),format(pvalue,scientific = FALSE)))
colnames(pvalue)[1] <- "MEV"
colnames(pvalue)[2] <- "P-Value"


pvalue <- pvalue %>%
  mutate(pValueTest = ifelse(pvalue[2] <= 0.05,"Pass","Not Pass")) %>% as.data.frame(.)
colnames(pvalue)[3] <- "pValueTest"

SingleFactorAnalysis <- merge(SingleFactorAnalysis,
                              pvalue,
                              by = "MEV",
                              all = TRUE)

# Sign Test
SingleFactorAnalysis <- SingleFactorAnalysis %>%
  mutate(SignTest = ifelse((ExpectedTrend == 'Negative' & Correlation < 0)| 
                             (ExpectedTrend == 'Positive' & Correlation >= 0),
                           "Y","N")) %>% as.data.frame(.)

# Correlation Test
SingleFactorAnalysis <- SingleFactorAnalysis %>% 
  mutate(CorrelationTest = ifelse(Correlation >= CorrelationThreshold|Correlation <= -CorrelationThreshold,"Y","N")) 

# Final Test
SingleFactorAnalysis <- SingleFactorAnalysis %>% 
  mutate(FinalTest = ifelse(pValueTest == 'Pass' & SignTest == 'Y' & CorrelationTest == 'Y',1,0)) 

# Workaround for data testing!!!!!
#SingleFactorAnalysis$FinalTest[SingleFactorAnalysis$FinalTes == 0] <- 1
#SingleFactorAnalysis$FinalTest[1] <- 0

xlsx::write.xlsx2(SingleFactorAnalysis, 
                  file = paste0(directory, "03-SFA-",segmentname,".xlsx"), 
                  sheetName= paste0("SFA-",segmentname),
                  col.names=TRUE,
                  row.names=TRUE,
                  append=FALSE)


# MEV that Final test equals 1
mevvariable <- SingleFactorAnalysis %>% filter(FinalTest == 1) %>% .[1] %>% unlist()


############################################################################################################################
## Stationarity Test
############################################################################################################################
# Parallel processing
clusters <- try(makeCluster(getOption("cl.cores", 8L), type = "SOCK"))
clusterCall(clusters, function() library(tseries))
clusterCall(clusters, function() library(CADFtest))
clusterExport(clusters,c('development.dataset','mevvariable'))

#  KPSS Test
kpsslevelstat <-
  parLapply(clusters,development.dataset[, names(development.dataset) %in% c(mevvariable,"odr","logit")], function(x)
    tryCatch(kpss.test(x[!is.na(x)], null = c("Level"))$statistic,error=function(e) NULL))

kpsslevelstat <- as.data.frame(unlist(kpsslevelstat))
kpsslevelstat <- as.data.frame(unlist(kpsslevelstat)) %>% mutate(var = rownames(kpsslevelstat)) 
kpsslevelstat$var <- gsub(".KPSS Level","",kpsslevelstat$var)

kpsstrendstat <-
  parLapply(clusters,development.dataset[, names(development.dataset) %in% c(mevvariable,"odr","logit")], function(x)
    tryCatch(kpss.test(x[!is.na(x)], null = c("Trend"))$statistic,error=function(e) NULL))

kpsstrendstat <- as.data.frame(unlist(kpsstrendstat))
kpsstrendstat <- as.data.frame(unlist(kpsstrendstat)) %>% mutate(var = rownames(kpsstrendstat)) 
kpsstrendstat$var <- gsub(".KPSS Trend","",kpsstrendstat$var)    

kpss.results <-
  as.data.frame(merge(
    kpsslevelstat,
    kpsstrendstat,
    by = "var",
    all = TRUE
  )
  )

colnames(kpss.results)[1] <- "var"
colnames(kpss.results)[2] <- "kpsslevelstat"
colnames(kpss.results)[3] <- "kpsstrendstat"

kpss.results.pass <-
  kpss.results %>%
  filter(kpsslevelstat < kpsslevelstat.criticalv
         |
           kpsstrendstat < kpsstrendstat.criticalv
  )

# ADF Tests
adfnonestat.results <-
  parLapply(clusters,development.dataset[, names(development.dataset) %in% c(mevvariable,"odr","logit")], function(x)
    tryCatch(CADFtest(x[!is.na(x)], NULL, type=c("none"),development.dataset)$statistic,error=function(e) NULL))

adfdriftstat.results <-
  parLapply(clusters,development.dataset[, names(development.dataset) %in% c(mevvariable,"odr","logit")], function(x)
    tryCatch(CADFtest(x[!is.na(x)], NULL, type=c("drift"),development.dataset)$statistic,error=function(e) NULL))

adftrendstat.results <-
  parLapply(clusters,development.dataset[, names(development.dataset) %in% c(mevvariable,"odr","logit")], function(x)
    tryCatch(CADFtest(x[!is.na(x)], NULL, type=c("trend"),development.dataset)$statistic,error=function(e) NULL))

stopCluster(clusters)

stationarity.results <-
  as.data.frame(merge(
    merge(
      as.data.frame(unlist(adfnonestat.results)),
      as.data.frame(unlist(adfdriftstat.results)),
      by = "row.names",
      all = TRUE
    ),
    as.data.frame(unlist(adftrendstat.results)),
    by.x = "Row.names",
    by.y = "row.names",
    all = TRUE
  ))

colnames(stationarity.results)[1] <- "var"
colnames(stationarity.results)[2] <- "adfnonestat"
colnames(stationarity.results)[3] <- "adfdriftstat"
colnames(stationarity.results)[4] <- "adftrendstat"
stationarity.results$var <- gsub(".ADF\\(1)","", stationarity.results$var)


stationarity.results.pass <-
  stationarity.results %>%
  filter(adfnonestat < adfnonestat.criticalv
         |
           adfdriftstat < adfdriftstat.criticalv
         |
           adftrendstat < adftrendstat.criticalv
  )

## Remove the model that fails ADF test
rm(adfnonestat.results,adfdriftstat.results,adftrendstat.results)

univariate.results <- merge(kpss.results,
                            stationarity.results, 
                            by = "var", all = TRUE)

univariate.results.pass <- merge(kpss.results.pass,
                                 stationarity.results.pass, 
                                 by = "var", all = TRUE)

univariate.results.pass <- merge(univariate.results.pass,
                                 SingleFactorAnalysis, 
                                 by.x = "var", by.y = "MEV", all.x = TRUE)

xlsx::write.xlsx2(univariate.results, 
                  file = paste0(directory, "04-Univariate-",segmentname,".xlsx"), 
                  sheetName= paste0("Univariate-",segmentname),
                  col.names=TRUE,
                  row.names=TRUE,
                  append=FALSE)

removena <- vector("character", nrow(univariate.results.pass))
for (i in 1:nrow(univariate.results.pass))
  if (rowSums(is.na(univariate.results.pass[i,2:6])) == 5 & !univariate.results.pass$var[i] %in% c("odr","logit")) {
    removena[i] <- univariate.results.pass[i,1]
  }

univariate.results.final <- filter(univariate.results.pass,!var %like% removena)

if (nrow(univariate.results.pass) > 30) {
  univariate.results.final <-
    univariate.results.final %>% top_n(., 30, abs(as.numeric(Correlation)))
}

xlsx::write.xlsx2(univariate.results.final, 
                  file = paste0(directory, "05-Univariate-Final-",segmentname,".xlsx"), 
                  sheetName= paste0("Univariate-",segmentname),
                  col.names=TRUE,
                  row.names=TRUE,
                  append=FALSE)


###########################################################################################################################
## Multivariate analysis
############################################################################################################################
# Prepare the dataset, filter out those that do not pass univariate analysis and correlation analysis
variable.list <- (univariate.results.final$var) %>% append(.,'logit')

development.datasetformev <- as.data.frame(development.dataset[, names(development.dataset) %in% c(variable.list,"ObservationDate")])
validation.datasetformev <- as.data.frame(validation.dataset[, names(validation.dataset) %in% c(variable.list,"ObservationDate")])

development.dataset <- as.data.frame(development.dataset[, names(development.dataset) %in% variable.list])
development.dataset$odr <- NULL

# Change this default action, doesn't work with dredge
options(na.action = "na.fail")

# Define the model
model <- lm(logit ~ ., data = development.dataset, singular.ok = TRUE)

# Check if there are any NA coefficients
mevnotna <- vector("character", length(model[["coefficients"]]))

for (i in 1:length(model[["coefficients"]]))
  if (!is.na(model[["coefficients"]][[i]])) {
    mevnotna[i] <- names(model[["coefficients"]][i])
  }

development.datasetformev <- as.data.frame(development.datasetformev[, names(development.datasetformev) %in% c(mevnotna,"logit","odr","ObservationDate")])
validation.datasetformev <- as.data.frame(validation.datasetformev[, names(validation.datasetformev) %in% c(mevnotna,"logit","odr","ObservationDate")])

development.dataset <- as.data.frame(development.dataset[, names(development.dataset) %in% c(mevnotna,"logit")])

# Define the model again after removing NA coefficients
model <- lm(logit ~ ., data = development.dataset, singular.ok = TRUE)

clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
clusters <- try(makeCluster(getOption("cl.cores", 8L), type = clusterType))
clusterCall(clusters, function() library(MuMIn))
clusterExport(clusters, c('development.dataset','model'))

models.all <-
  get.models(pdredge(model, cluster = clusters,
                     evaluate = TRUE, rank = "AICc",
                     m.lim = c(2,3), ## can change the parameter
                     extra  = list("R^2", "adjR^2", "AIC", "BIC", "ICOMP", "Cp")),
             subset = TRUE)

stopCluster(clusters)

models.temp <- models.all
models.all <- models.temp

# Remove models with more than 1 same base variables or NA coefficients
for (i in length(models.all):1) 
  if (sum(duplicated(gsub(".*_(.*)", "\\1", names(models.all[[i]][["model"]])))) >= 1 | sum(complete.cases(models.all[[i]][["coefficients"]])) != length(models.all[[i]][["coefficients"]])) {
    models.all[i] <- NULL
  } 

# Put this back to the default option
options(na.action = "na.omit")

# Coefficients p-value	
coeff.pval <- models.all 	
for (i in 1:length(coeff.pval))	
{	
  coeff.pval[[i]]<- list.append(coeff.pval[[i]], coefficients.pval = summary(coeff.pval[[i]])$coefficients[,4])	
}


###########################################################################################################################
## Model Selection Based on Tests
############################################################################################################################
# Shapiro Wilk (Normality)
# Ho: the samples come from a Normal distribution
for (i in 1:length(models.all))
{
  models.all[[i]]["shapiro.wilk.p"] <-
    shapiro.test(residuals(models.all[[i]], type = 'response'))$p
}

# Breusch-Pagan (Herteroskedasticity) with chi-squared distributed
# Ho: variance residual is constant (homoskedasticity)
for (i in 1:length(models.all))
{
  models.all[[i]]["bptest.p"] <-
    bptest(summary(models.all[[i]])$terms,
           data = development.dataset,
           studentize = TRUE)$p.value
}

# Durbin Watson (Autocorrelation)
# Ho: there is no correlation among residuals
for (i in 1:length(models.all))
{
  models.all[[i]]["dwtest1.p"] <-
    durbinWatsonTest((models.all[[i]]),
                     max.lag = 1,
                     simulate = TRUE,
                     reps = 1000,
                     method = c("normal"),
                     alternative = c("positive")
    )$p
}

# Variance Inflation Factor (Multicollinearity)
for (i in 1:length(models.all))
{
  if (nrow(as.data.frame(models.all[[i]][["assign"]])) > 2)
  {
    vif <- vif((models.all[[i]]))
    
    names(vif) <- paste0("vif_", names(vif))
    
    models.all[[i]]["Adjusted R-squared"] <- summary(models.all[[i]])$adj.r.squared
    
    models.all[[i]]["p-value Intercept"] <- summary(models.all[[i]])$coefficients[1,4] 
    
    models.all[[i]] <- append(models.all[[i]],vif)
    
    rm(vif)
    
    models.all[[i]] <-
      list.append(models.all[[i]], correlation.matrix = corr.test(models.all[[i]][["model"]][2:ncol(models.all[[i]][["model"]])])$r)
    
    models.all[[i]] <-
      list.append(models.all[[i]], pvalue.matrix = corr.test(models.all[[i]][["model"]][2:ncol(models.all[[i]][["model"]])])$ci)
    
  }
  else 
  {
    message("Single variable, skipped for model ", i, " name:", names(models.all[i])) 
  }
}

ind.check <- list()
ind.test <- list()
for (i in 1:length(models.all)) {
  for (j in 1:nrow(models.all[[i]][["pvalue.matrix"]])) 
    if ((abs(models.all[[i]][["pvalue.matrix"]][["r"]][j]) >= 0.8) &
        (models.all[[i]][["pvalue.matrix"]][["p"]][j] <= 0.05)) {
      ind.check[[j]] <- 1 
    } else {
      ind.check[[j]] <- 0
    }
  ind.test[[i]] <- Reduce("+",ind.check)
}

##for (i in length(models.all):1) 
##  if (ind.test[i] >= 1) {
##    models.all[i] <- NULL
##  } 

###########################################################################################################################
## PASS OR FAIL CHECK
###########################################################################################################################
for (i in 1:length(models.all))
{
  models.all[[i]][["shapiro.pof"]] <-
    if(models.all[[i]][["shapiro.wilk.p"]] > 0.05){
      'PASS'
    } else {
      'FAIL'
    }
}

for (i in 1:length(models.all))
{
  models.all[[i]][["shapiro.statvalue"]] <- models.all[[i]][["shapiro.wilk.p"]]
}

for (i in 1:length(models.all))
{
  models.all[[i]][["bptest.pof"]] <-
    if(models.all[[i]][["bptest.p"]] > 0.05){
      'PASS'
    } else {
      'FAIL'
    }
}

for (i in 1:length(models.all))
{
  models.all[[i]][["bptest.statvalue"]] <- models.all[[i]][["bptest.p"]]
}

for (i in 1:length(models.all))
{
  models.all[[i]][["dwtest1.pof"]] <-
    if(models.all[[i]][["dwtest1.p"]] > 0.05){
      'PASS'
    } else {
      'FAIL'
    }
}

for (i in 1:length(models.all))
{
  models.all[[i]][["dwtest1.statvalue"]] <- models.all[[i]][["dwtest1.p"]]
}

vifrow <- vector("numeric", length(models.all))
for (i in 1:length(models.all))
{
  vifresult <- as.data.frame(names(models.all[[i]]) %like% "vif_%")
  names(vifresult) <- "vifresult" 
  vifresult$row <- seq.int(nrow(vifresult))
  vifresult <- vifresult %>% dplyr::filter(vifresult == 'TRUE') 
  vifresult <- (vifresult[,2])
  vifrow[i] <- as.data.frame(append(i,vifresult))
  vifrow[i] <- if (i==1) {vifrow} else {cbind(vifrow[i],vifrow)}
}


for (i in 1:length(models.all))
{
  for (j in 2:length(vifrow[[i]])) 
  { 
    models.all[[i]][["vif.pof"]][[j]] <-
      if (models.all[[i]][[vifrow[[i]][[j]]]] < 10) {
        'PASS'
      } else {
        'FAIL'
      }
  }
}

for (i in 1:length(models.all))
{
  for (j in 2:length(vifrow[[i]])) 
  { 
    models.all[[i]][["vif.test"]][[j]] <-
      if (models.all[[i]][[vifrow[[i]][[j]]]] < 1000) {
        models.all[[i]][[vifrow[[i]][[j]]]]
      } else {
        'FAIL'
      }
  }
}

maxvif <- vector("numeric", length(models.all))
for(i in 1:length(models.all)) {
  maxvif[i] <- max(as.numeric(unlist(models.all[[i]][["vif.test"]])))
}

viftest <- vector("character", length(models.all))
for(i in 1:length(models.all)) {
  viftest[i] <- if (max(as.numeric(unlist(models.all[[i]][["vif.test"]]))) < 10)
  {'PASS'}
  else
  {'FAIL'}
}


# test1 <- ##all(models.all[[i]][["vif.pof"]] == 'PASS')
#   if (all(models.all[[3]][["vif.test"]] < 10 ,na.rm = TRUE)) {
#     viftest[i] <- 'PASS'
#   } else {
#     viftest[i] <- 'FAIL'
#   }


for (i in 1:length(models.all))
{
  models.all[[i]][["intercept.pof"]] <-
    if(models.all[[i]][["p-value Intercept"]] < 0.05){
      'PASS'
    } else {
      'FAIL'
    }
}

## Repeat again for viftest
# viftest <- vector("character", length(models.all))
for(i in 1:length(models.all)) {
  viftest[i] <- if (max(as.numeric(unlist(models.all[[i]][["vif.test"]]))) < 10)
  {'PASS'}
  else
  {'FAIL'}
}

for (i in 1:length(models.all))
{
  models.all[[i]][["numberofpass"]] <-
    sum(as.integer(length(which(models.all[[i]][["shapiro.pof"]] == 'PASS'))),
        as.integer(length(which(models.all[[i]][["bptest.pof"]] == 'PASS'))), 
        as.integer(length(which(models.all[[i]][["dwtest1.pof"]] == 'PASS'))),
        # as.integer(which(all(models.all[[i]][["vif.pof"]] == 'PASS', na.rm = TRUE)))
        as.integer(viftest[i] == 'PASS')
        # as.integer(length(which(models.all[[i]][["intercept.pof"]] == 'PASS')))
    )
}

###########################################################################################################################
## Create summary table
###########################################################################################################################
Model <- vector("numeric", length(models.all))
for(i in 1:length(models.all)) {
  Model[i] <- names(models.all[i])  
}

Numberofpass <- vector("numeric", length(models.all))
for(i in 1:length(models.all)) {
  Numberofpass[i] <- models.all[[i]][["numberofpass"]]
}

MaxPass <- vector("numeric", length(models.all))
for(i in 1:length(models.all)){
  if (max(Numberofpass) ==
      tryCatch((models.all[[i]][["numberofpass"]]),error = function(e) paste("<NA>"))){
    MaxPass[[i]] = max(Numberofpass)
  }
}

AdjRsqr <- vector("numeric", length(models.all))
for(i in 1:length(models.all)){
  AdjRsqr[[i]] <-  round(models.all[[i]][["Adjusted R-squared"]], digits = 10)
}

MaxAdjRsqr <- vector("numeric", length(models.all))
for(i in 1:length(models.all)){
  if (max(AdjRsqr) ==
      tryCatch(round(models.all[[i]][["Adjusted R-squared"]], digits = 10), 
               error = function(e) paste("<NA>"))){
    MaxAdjRsqr[[i]] = max(AdjRsqr)
  }
}

shapiro <- vector("character", length(models.all))
bptest <- vector("character", length(models.all))
dwtest <- vector("character", length(models.all))
# viftest <- vector("character", length(models.all))
# intercept <- vector("character", length(models.all))
shapiro.statvalue <- vector("character", length(models.all))
bptest.statvalue <- vector("character", length(models.all))
dwtest.statvalue <- vector("character", length(models.all))
# vif.statvalue <- vector("character", length(models.all))

for(i in 1:length(models.all)){
  shapiro[i] <- models.all[[i]][["shapiro.pof"]]
  bptest[i] <- models.all[[i]][["bptest.pof"]]
  dwtest[i] <- models.all[[i]][["dwtest1.pof"]]
  # if (all(models.all[[i]][["vif.pof"]] == 'PASS',na.rm = TRUE)) {
  #   viftest[i] <- 'PASS'
  # } else {
  #   viftest[i] <- 'FAIL'
  # }
  # intercept[i] <- models.all[[i]][["intercept.pof"]]
}

for(i in 1:length(models.all)){
  shapiro.statvalue[i] <- models.all[[i]][["shapiro.statvalue"]]
  bptest.statvalue[i] <- models.all[[i]][["bptest.statvalue"]]
  dwtest.statvalue[i] <- models.all[[i]][["dwtest1.statvalue"]]
  # vif.statvalue[i] <- models.all[[i]][["vif.statvalue"]]
  # intercept[i] <- models.all[[i]][["intercept.pof"]]
}

# summarymodel <-
#   data.frame(cbind(Model,Numberofpass,MaxPass,AdjRsqr,MaxAdjRsqr,shapiro.statvalue,shapiro,bptest.statvalue,bptest,dwtest.statvalue,dwtest,maxvif,viftest,intercept)) %>% .[order(-AdjRsqr),]
# summarymodel[summarymodel==0] <- NA

summarymodel <- 
  data.frame(cbind(Model,Numberofpass,MaxPass,AdjRsqr,MaxAdjRsqr,shapiro.statvalue,shapiro,bptest.statvalue,bptest,dwtest.statvalue,dwtest,maxvif,viftest)) %>% .[order(-AdjRsqr),]
summarymodel[summarymodel==0] <- NA


## In case you want to see only the models with MaxAdjRsqr or MaxPass 
## summarymodel <- summarymodel %>% dplyr::filter(!is.na(MaxAdjRsqr)|!is.na(MaxPass))

## Generate summarized models results to Excel in order to select a model
xlsx::write.xlsx2(summarymodel, 
                  file = paste0(directory, "06-summarymodel-",segmentname,".xlsx"), 
                  sheetName= paste0("summarymodel-",segmentname),
                  col.names=TRUE,
                  row.names=FALSE,
                  append=FALSE)

# Generate top50 models with MEV and coefficients
top50model <- head(summarymodel,50) #select only top 50 models
mevrow <- list() # because we limit max. MEVs at 3 + intercept + model number
coeffrow <- list() # because we limit max. MEVs at 3 + intercept + model number
coeffpvalrow <- list()
# vifrow <- list()
top50details <- NULL

mevrow <- lapply(as.character(top50model$Model), 
                 function(x) append(x,rownames(as.data.frame(models.all[[x]][["coefficients"]]))))

coeffrow <- lapply(as.character(top50model$Model), 
                   function(x) append(x,as.data.frame(models.all[[x]][["coefficients"]])[,1]))

coeffpvalrow <- lapply(as.character(top50model$Model), 	
                       function(x) append(x,as.data.frame(coeff.pval[[x]][["coefficients.pval"]])[,1]))

# vifrow <- lapply(as.character(top50model$Model), 	
#                        function(x) append(x,as.data.frame(models.all[[x]][["vif.test"]])[,1]))

top50details <- do.call(rbind,list(mevrow,coeffrow,coeffpvalrow))
top50details[sapply(top50details,is.null)] <- NULL
top50details <- ldply(top50details, rbind)
names(top50details)[1] <- "Model"

xlsx::write.xlsx2(top50details,
                  file = paste0(directory, "07-top50details-",segmentname,".xlsx"),
                  sheetName= paste0("top50details-",segmentname),
                  col.names=TRUE,
                  row.names=FALSE,
                  append=FALSE)

