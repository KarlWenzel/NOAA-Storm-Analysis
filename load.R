library(plyr)

RAW = read.csv("repdata-data-StormData.csv.bz2")

# We will store our processed data in STORMS
STORMS = data.frame(
  date =  strptime( RAW$BGN_DATE, format="%m/%d/%Y" ),
  state = RAW$STATE,
  evnt =  as.character( RAW$EVTYPE ),
  dead =  RAW$FATALITIES,
  hurt =  RAW$INJURIES,
  prop =  RAW$PROPDMG,
  pexp =  tolower( RAW$PROPDMGEXP ),
  crop =  RAW$CROPDMG,
  cexp =  tolower( RAW$CROPDMGEXP )
)

# Filter out extraneous data
row.filter = STORMS$dead + STORMS$hurt + STORMS$prop + STORMS$crop > 0
STORMS = STORMS[ row.filter, ]

# Store year as a number
STORMS$year = as.numeric( format(STORMS$date, "%Y") )

# see http://www.ncdc.noaa.gov/stormevents/details.jsp
# Since all data prior to 1955 is only for tornados, this data is discarded to avoid skewing
STORMS = STORMS[ STORMS$year > 1955, ]

# Prepare exponent reference data; use to join a multiplier based on the exponent key
my.exp = c( 1:9, "h", "k", "m", "b" )
my.mul = c( 10^(1:9), 100, 1000, 1000000, 1000000000 )
PEXP = data.frame( pexp = my.exp, pmul = my.mul )
CEXP = data.frame( cexp = my.exp, cmul = my.mul )

# They both look the same except for column names
print( names(PEXP) )
print( CEXP )

# Join the multipliers by matching the exponent keys
STORMS = join( STORMS, PEXP, by="pexp", type="left", match="first" )
STORMS = join( STORMS, CEXP, by="cexp", type="left", match="first" )

# Any remaining unset conditions for the multipliers are set to 0
STORMS[ is.na(STORMS$pmul),]$pmul = 0
STORMS[ is.na(STORMS$cmul),]$cmul = 0

# Scale the property damage and crop damage values by their multipliers
STORMS$prop = STORMS$prop * STORMS$pmul
STORMS$crop = STORMS$crop * STORMS$cmul

# Prepare category reference data; this provides us with regex patterns 
CATEGORIES = read.csv("category-reference.csv", stringsAsFactors=FALSE)
print( head(CATEGORIES) )

# Use the patterns to assign categories

STORMS$category = NA
for (i in 1:nrow(CATEGORIES)) {
  filter = is.na(STORMS$category) & grepl( CATEGORIES[i,]$patterns, STORMS$evnt )
  STORMS[filter,]$category = CATEGORIES[i,]$category
}

# Any unset categories are grouped into their own category
STORMS[ is.na(STORMS$category), ]$category = "Other"
STORMS$category = as.factor(STORMS$category)

# Remove the columns that are no longer necessary
STORMS$cexp = NULL
STORMS$pexp = NULL
STORMS$X = NULL
STORMS$pmul = NULL
STORMS$cmul = NULL

# We now have our tidy data set.
print( head(STORMS) )

# This is a good save point if you are running my code.
#   It creates a 17.8 MB file as opposed to the 548 MB uncompressed raw file
storms.file = "storms-tidy-dataset.csv"
write.csv(STORMS, storms.file)