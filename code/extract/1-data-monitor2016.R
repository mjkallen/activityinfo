#####################################################
#  ActivityInfo Monitoring analysis script          #
#####################################################

required.packages <- c("activityinfo", "reshape2")

for (pkg in required.packages) {
  if (!require(pkg, character.only = TRUE)) {
    stop("package '", pkg, "' is required by this script, but is not installed")
  }
}

# authenticate
activityInfoLogin()

### JOR 2016 Monitoring Database Jordan db 5026
database.id <- 5026

# Set the following to 'TRUE' if you also want to export the comment field of each site:
include.comments <- TRUE

#-------------------------------------------------------------------------------
# Script body: data extraction from ActivityInfo
#-------------------------------------------------------------------------------

values <- getDatabaseValues(database.id, include.comments = include.comments)


# Write out the full data extract to CSV file:
write.csv(
  values,
  file.path("out", sprintf("%s_db%d_all_data_extract.csv",
                           format(Sys.Date(), "%Y-%m-%d"),
                           database.id)),
  na = ""
)

#-------------------------------------------------------------------------------
# Script body: data preparation for dashboards
#-------------------------------------------------------------------------------

# First selection of columns to output (format of column names is not entirely
# consistent, but must match variables used in dashboards at
# https://github.com/unhcr-jordan/unhcr-jordan.github.io/tree/master/sectors/2016):
output <- data.frame(
  Indicator  = values[["indicator.name"]],
  Units      = values[["indicator.units"]],
  Value      = values[["indicator.value"]],
  Partner    = values[["partner.label"]],
  StartDate  = format(as.Date(values[["start_date"]], "%Y-%m-%d"), "%d/%m/%Y"),
  appeal     = values[["2-3RP Implementation Type"]],
  Fundedby   = values[["3-3RP appeal through"]],
  allocation = values[["4-Allocation according to 3RP"]],
  activity   = values[["form"]],
  region     = values[["location.adminlevel.region"]],
  stringsAsFactors = FALSE
)

sec.cat.obj <- local({
  # Determine sector, category and objective based on form category names:
  form.categories <- unique(values$form.category)
  
  do.call(rbind, lapply(form.categories, function(s) {
    m <- regexec("^([A-Z /]+)(\\[([A-Z]*)(.+)\\])(.+)$", s)[[1]]
    if (length(m) == 1L && m == -1) {
      warning("could not find sector, category and objective in form category name '",
              s, "'")
      data.frame(
        form.category = s,
        Sector = NA_character_,
        Category = NA_character_,
        Objective = NA_character_,
        stringsAsFactors = FALSE
      )
    } else {
      matches <- mapply(function(start, length) {
        substr(s, start, start + length - 1)
      }, m, attr(m, "match.length"))
      # Matched expressions:
      # matches[1] = complete string
      # matches[2] = sector label
      # matches[3] = code (in brackets)
      # matches[4] = code: category label (REF or RES)
      # matches[5] = code: category number
      # matches[6] = objective
      sector.label <- trimws(matches[2])
      sector <- switch(sector.label,
                       "BN" = "BASICNEEDS",
                       "EDU" = "EDUCATION",
                       "FOOD/LIV" = "FOOD",
                       "HLTH" = "HEALTH",
                       "PROT" = "PROTECTION",
                       "SHLT" = "SHELTER",
                       # 'WASH' and 'JUS' remain as they are:
                       sector.label
      )
      category.label <- matches[4]
      category <- switch(category.label,
                         "RES" = "Resilience",
                         "REF" = "Refugee",
                         NA_character_)
      data.frame(
        form.category = s,
        Sector = sector,
        Category = category,
        Objective = trimws(matches[6]),
        stringsAsFactors = FALSE
      )
    }
  }))
})

output$Sector <- sec.cat.obj$Sector[match(values$form.category,
                                          sec.cat.obj$form.category)]

# Create a 'Governorate' column which contains the (capitalized) name of the
# governorate OR the name of a camp OR 'Countrywide':
output$Governorate <- local({
  regionactivityinfocode <- read.csv(
    file = file.path("data", "regionactivityinfocode.csv"),
    colClasses = "character",
    stringsAsFactors = FALSE
  )
  gov <- regionactivityinfocode$gov[match(
    values$location.adminlevel.governorate,
    regionactivityinfocode$governorate
  )]
  
  # Identify special locations and replace the governorate name:
  gov[grepl("^Zaatari (District|Camp)", values$location.name)] <- "ZaatariCamp"
  gov[grepl("^Azraq Camp", values$location.name)] <- "AzraqCamp"
  gov[values$location.name == "Emirati Jordanian Camp (EJC)"] <- "EJ Camp"
  gov[values$location.name == "Country Wide"] <- "Countrywide"
  gov
})

# Create a table to use for identification of gender and population type in the
# indicator names:
gender.poptype <- read.csv(text = "#
string,gender,poptype
SYRIAN_WOMEN_IN_URBAN,Women,Urban
SYRIAN_WOMEN_IN_CAMPS,Women,Camp
SYRIAN_MEN_IN_URBAN,Men,Urban
SYRIAN_MEN_IN_CAMPS,Men,Camp
SYRIAN_GIRLS_IN_URBAN,Girls,Urban
SYRIAN_GIRLS_IN_CAMPS,Girls,Camp
SYRIAN_BOYS_IN_CAMPS,Boys,Camp
SYRIAN_BOYS_IN_URBAN,Boys,Urban
EXISTING FEMALE SYRIAN CHILDREN (0 to 17) IN CAMPS,Girls,Camp
EXISTING FEMALE SYRIAN CHILDREN (0 to 17) IN URBAN,Girls,Urban
EXISTING MALE SYRIAN CHILDREN (0 to 17) IN CAMPS,Boys,Camp
EXISTING MALE SYRIAN CHILDREN (0 to 17) IN URBAN,Girls,Urban
MALE_IN_HOST_COMMUNITIES,Men,Host Community
FEMALE_IN_HOST_COMMUNITIES,Women,Host Community
WOMEN_IN_HOST_COMMUNITY,Women,Host Community
MEN_IN_HOST_COMMUNITY,Men,Host Community
BOYS_IN_HOST_COMMUNITY,Boys,Host Community
GIRLS_IN_HOST_COMMUNITY,Girls,Host Community",
                           comment.char = "#",
                           stringsAsFactors = FALSE,
                           colClasses = "character")

output <- cbind(output, local({
  tmp <- data.frame(Gender = rep(NA_character_, nrow(output)),
                    poptype = rep(NA_character_, nrow(output)),
                    stringsAsFactors = FALSE)
  
  for (row in seq(nrow(gender.poptype))) {
    is.match <- grepl(gender.poptype$string[row], values$indicator.name)
    tmp$Gender[is.match] <- gender.poptype$gender[row]
    tmp$poptype[is.match] <- gender.poptype$poptype[row]
  }
  tmp
}))

# Write dashboard data out per sector:
for (sector in unique(output$Sector)) {
  write.csv(
    # Keep only those indicators for the sector AND for which we have determined
    # the gender (and population type):
    subset(output, Sector == sector & !is.na(Gender)),
    file.path("out", "monitor", "2016", tolower(sector), "data.csv"),
    na = "(unknown)"
  )
}

# Write dashboard data out for camps and country wide:
write.csv(
  subset(output, Governorate == "AzraqCamp" & !is.na(Gender)),
  file.path("out", "monitor", "2016", "azraq", "data.csv"),
  na = "(unknown)"
)
write.csv(
  subset(output, Governorate == "ZaatariCamp" & !is.na(Gender)),
  file.path("out", "monitor", "2016", "zaatari", "data.csv"),
  na = "(unknown)"
)
write.csv(
  subset(output, Governorate == "Countrywide" & !is.na(Gender)),
  file.path("out", "monitor", "2016", "countrywide", "data.csv"),
  na = "(unknown)"
)
