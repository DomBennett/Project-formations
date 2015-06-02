# 02/06/2015
# Dom Bennett
# Split rows by species

# DIRS AND FILES
input.dir <- '1_formations'
output.dir <- '2_species'
# if the output.dir doesn't exist, create it
if (!file.exists (output.dir)) {
  dir.create (output.dir)
}

# FUNCTIONS
checkSpecies <- function (name) {
  # check species name before adding to list
  bool <- grepl ('_', name)  # it must be a two part name
  bool <- length (name) > 0 & bool  # must be a string
  bool <- length (name) > 20 & bool  # must be less than 20
  bool
}

getSpecies <- function (species.description) {
  # use capital letters to identify two-part species names
  # clean up the description
  # remove all things in ()
  species.description <- gsub ('\\([^\\)]*\\)', '', species.description)
  # remove all things in []
  species.description <- gsub ('\\[[a-zA-Z]*\\]', '', species.description)
  # remove all things in <>
  species.description <- gsub ('<[^>]*>', '', species.description)
  # remove all ,.;#: and other things
  species.description <- gsub ('\\.|,|;|\\#|[0-9]*|"|:|\\-', '', species.description)
  # remove all ' and ' and ' & '
  species.description <- gsub ('\\s+and\\s+|\\s+&\\s+', '', species.description)
  # replace all spaces with single _
  species.description <- gsub ('\\s+', '_', species.description)
  # identify point in string where species names begin
  locations <- gregexpr ('[A-Z]', species.description)[[1]]
  if (length (locations) > 1) {
    # loop through each location and split string up
    species <- NULL
    for (i in 2:length (locations)) {
      start <- locations[i-1]
      end <- locations[i] - 1
      s <- substr (species.description, start, end)
      s <- gsub("^_|_$", "", s)
      if (checkSpecies (s)) {
        species <- c (species, s)
      }
    }
  } else {
    # only one species
    # remove trailing _
    species <- gsub("^_|_$", "", species.description)
  }
  return (species)
}

# INPUT
geodata <- read.csv (file.path (input.dir, 'geodata_complete.csv'),
                     stringsAsFactors=FALSE)

# BREAK UP SPECIES
# create res dataframe
res <- geodata[1, ]
res$species <- NA
# loop through each row
for (i in 1:nrow (geodata)) {
  # extract row element
  row.element <- geodata[i, ]
  # split fauna string by ','
  species.description <- row.element[ ,'Resume_of_Fauna']
  # avoid locale errors
  species.description <- enc2native (species.description)
  species <- getSpecies (species.description)
  # create new 
  for (s in species) {
    new.row.element <- row.element
    new.row.element$species <- s
    res <- rbind (res, new.row.element)
  }
}
res <- res[-1, ]
uniqspp <- sort (unique (res$species))
cat ('\nCreated dataframe of [', nrow (res), '].
     Identifed [', length (uniqspp), '] unique species.',
     sep='')


# OUTPUT
write.table (uniqspp, file=file.path (output.dir, 'names.txt'),
             row.names=FALSE, col.names=FALSE, quote=FALSE)
write.csv (res, file=file.path (output.dir, 'geodata.csv'))
