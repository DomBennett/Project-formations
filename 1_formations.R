# 02/06/2015
# Dom Bennett
# Add new formation information

# DIRS AND FILES
input.dir <- '0_data'
output.dir <- '1_formations'
formations.file <- 'formations_test.csv'
geodata.file <- 'USGSTesting.csv'
# if the output.dir doesn't exist, create it
if (!file.exists (output.dir)) {
  dir.create (output.dir)
}

# FUNCTIONS
findFormation <- function (formation.description) {
  # avoid locale errors by converting to UTF-8
  formation.description <- enc2native (formation.description)
  # loop through each formation
  for (form in formations) {
    # using regexpr (this returns T/F) to find match
    if (grepl (form, formation.description, ignore.case=TRUE)) {
      # if a match, break out of loop and return the match
      return (form)
    }
  }
  # if loop has completed w/o breakout return NA
  return (NA)
}

# INPUT
# read in list of formations as vector without levels
formations <- read.csv (file.path (input.dir, formations.file),
                        stringsAsFactors=FALSE)[,1]
# read in USGS data without factors
geodata <- read.csv (file.path (input.dir, geodata.file),
                     stringsAsFactors=FALSE)

# ADD TO FORMATIONNEW
# loop through each row in geodata
for (i in 1:nrow (geodata)) {
  # get row data
  row.element <- geodata[i,]
  # get formation description
  formation.description <- row.element[ ,"Formation"]
  # use findFormation() to get matching formation name
  formation <- findFormation(formation.description)
  # add to geodata
  geodata$FormationNew[i] <- formation
}
n.missing <- sum (is.na (geodata$FormationNew))
n.uniques <- length (unique (geodata$FormationNew)) - 1  # ignore NA
cat ('\nFound [', n.uniques, '] formations in USGS formation descriptions.
     [', n.missing, '/', nrow (geodata), '] without formation name.', sep='')

# OUTPUT
geodata.missing <- geodata[is.na (geodata$FormationNew), ]
geodata.complete <- geodata[!is.na (geodata$FormationNew), ]
write.csv (geodata.missing, file.path (output.dir,
                                       'geodata_missing.csv'))
write.csv (geodata.complete, file.path (output.dir,
                                       'geodata_complete.csv'))