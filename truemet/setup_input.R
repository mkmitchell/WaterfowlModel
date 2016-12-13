# R script designed to import Waterfowl ArcGIS Model csv file and export
# an input file for truemet.
#
# Current works with Truemet 2.2
#
# Initial development by Mike Mitchell, Ducks Unlimited

#Import libraries
library(plyr)
library("dplyr")

############################################################################
# Variable designiation
# Workspace directory
workspace = "D:/GIS/projects/Waterfowl model/truemet/"

# Input ArcGIS Model csv file
arcin = "Nov_15_16_table.csv"
# CSV for cover, kg_ha (harvested and unharvested), tme, and decomp
coverin = "cover_values.csv"
# Foraging time vector for each habitat type based on winter water periods.
foragetimein = "forage_time_vector.csv"

# Forage time vector in days
forage_time_vector = c(1, 105, 210)

# List of foraging guilds by name
guilds = c("Dabbling Ducks")

# Time vector for for guild populations
popvector = c(1, 16, 32, 47, 62, 77, 93, 108, 124, 139, 152, 167, 183, 210)

# A population curve matching the length of the population vector
# each guild should have it's own variable name followed by an incredmental number.  guildpop1, guildpop2, guildpop3
guildpop1 = c(143170, 401889, 836745, 1539851, 2489874, 2748556, 3127486, 3329976, 3433648, 3393342, 1802482, 881385, 881385, 881385)

############################################################################

# Read in data
arcData = read.csv(paste(workspace, arcin, sep=""), header=TRUE)
coverData = read.csv(paste(workspace, coverin, sep=""), header=TRUE)
forageTime = read.csv(paste(workspace, foragetimein, sep=""), header=FALSE)

# Join arcData to coverData based on cover_type
combined = merge(arcData, coverData, by.x = "COVER_TYPE", by.y = "COVER")

# Remove rows with < 1ha
combined = combined[!(combined$HECTARES<1),]

# Drop extra columns
combined$FUNCTIONAL <- NULL
combined$FIXID <- NULL

# Fill in all null harvest values with 1.  If redoak is null enter 0
combined$Z_HARVESTE[is.na(combined$Z_HARVESTE)] <- 1
combined$Z_RED_OAK_[is.na(combined$Z_RED_OAK_)] <- 0

# If ST_FED is null input Private
combined$ST_FED <- sub("^$", "Private", combined$ST_FED)

# Calculate woody wetlands
combined$KG_HA=((combined$HECTARES*(combined$Z_HARVESTE*0.01)*combined$HRVST_KG_HA))+((1-combined$Z_HARVESTE*0.01)*combined$HECTARES*combined$UNHRVST_KG_HA)
combined$KG_HA= ifelse(combined$COVER_TYPE == "woody wetlands", combined$HECTARES * (combined$Z_RED_OAK_ * 1.2405 + 41.619), combined$KG_HA)

# Create new habitat type that takes the form (STATE_ST_FED_COVER_TYPE)
combined$COVER = with(combined, paste0(combined$STATE,"_", combined$ST_FED, "_", combined$COVER_TYPE))

# Get weighted mean by habitat type
outputCSV = ddply(combined, ~combined$COVER, function (x) weighted.mean(x$KG_HA, x$HECTARES/sum(x$HECTARES)))
colnames(outputCSV)[colnames(outputCSV)=="V1"] = "FOOD_BIOMASS"
colnames(outputCSV)[colnames(outputCSV)=="combined$COVER"] = "FORAGE_TYPE_NAME"
testsum = ddply(combined, ~combined$COVER, summarize, "TOTAL_AREA_BY_FORAGE_TYPE" = sum(HECTARES))
colnames(testsum)[colnames(testsum)=="combined$COVER"] = "FORAGE_TYPE_NAME"
outputCSV = merge(outputCSV, testsum, by.x = "FORAGE_TYPE_NAME")

#Setup data columns and format
# N_FORAGE_TYPE
outputCSV$N_FORAGE_TYPES = c(nrow(outputCSV))
outputCSV$N_FORAGE_TYPES = ''
outputCSV$N_FORAGE_TYPES[1] = nrow(outputCSV)

# AREA_UNIT
outputCSV$AREA_UNIT = ''
outputCSV$AREA_UNIT[1] = "ha"

# TIME_VECTOR_FORAGE_TYPE
outputCSV$TIME_VECTOR_FORAGE_TYPE = ''
i = 0
for (i in 1:length(forage_time_vector)) {
  outputCSV$TIME_VECTOR_FORAGE_TYPE[i] = forage_time_vector[i]
}

# Create forage_type_avail_vectorN for each forage type.  Automatically fill that columns values based on the imported csv by forage type
temp = subset( combined[, c("COVER_TYPE", "COVER", "TME", "DECOMP")])
temp = unique(temp[c('COVER_TYPE', 'COVER', 'TME', 'DECOMP')])
temp = merge(temp, forageTime, by.x = "COVER_TYPE", by.y = "V1")
outputCSV = merge(outputCSV, temp, by.x = "FORAGE_TYPE_NAME", by.y = "COVER")
i = 0
for (i in 1:nrow(outputCSV)) {
  newvar = paste("forage_type_avail_vector", toString(i), sep="")
  outputCSV[, newvar] = ''
  a = 0
  for (a in 2:4) {
    tempvar = paste("V", toString(a), sep="")
    outputCSV[a-1, newvar]= outputCSV[i, tempvar]
  }
}

#Add additional static columns
outputCSV$FOOD_BIOMASS_UNCERTAINTY = 0.15
outputCSV$METABOLIZABLE_ENERGY = outputCSV$TME * 1000
outputCSV$TME <- NULL
outputCSV$METABOLIZABLE_ENERGY_UNCERTAINTY = 0.05
outputCSV$ENERGY_UNIT = ''
outputCSV$ENERGY_UNIT[1] = 'kcal'


# RATE_OF_CHANGE_RESERVE
outputCSV$RATE_OF_CHANGE_RESERVE = ''
outputCSV$CARRYING_CAPACITY = ''
i = 0
for (i in 1:length(forage_time_vector)) {
  outputCSV$RATE_OF_CHANGE_RESERVE[i] = 0
  outputCSV$CARRYING_CAPACITY[i] = 0
}
colnames(outputCSV)[colnames(outputCSV)=="DECOMP"] = "RATE_OF_CHANGE_AVAILABLE"
outputCSV$V2 <- NULL
outputCSV$V3 <- NULL
outputCSV$V4 <- NULL

#Add guild info
outputCSV$N_FORAGING_GUILDS = ''
outputCSV$N_FORAGING_GUILDS[1] = length(guilds)
outputCSV$FORAGING_GUILD_NAME = ''
i=0
for (i in 1:length(guilds)){
  outputCSV$FORAGING_GUILD_NAME[i] = guilds[i]
}

# Handle multiple forage_type_preferences by guild.  Currently setting all preferences equal.
i = 0
for (i in 1:length(guilds)) {
  newvar = paste("forage_type_pref", toString(i), sep="")
  outputCSV[, newvar] = ''
  a = 0
  for (a in 1:nrow(outputCSV)) {
    outputCSV[a-1, newvar]= 1/nrow(outputCSV)
  }
}

# Handles guild population columns.  This could be done above but keeping separate for ease of editing later.
outputCSV$TIME_VECTOR_POP = ''
i=0
for (i in 1:length(popvector)) {
  outputCSV$TIME_VECTOR_POP[i] = popvector[i]
}

i = 1
for (i in 1:length(guilds)) {
  tempvar = paste("guildpop", toString(i),sep="")
  stopifnot(length(popvector)==length(get(paste("guildpop", toString(i),sep=""))))
  newvar = paste("population_vector", toString(i), sep="")
  outputCSV[, newvar] = ''
  a = 0
  for (a in 1:length(get(paste("guildpop", toString(i),sep="")))) {
    outputCSV[a-1, newvar]= get(paste("guildpop", toString(i),sep=""))[a]
  }
}


#######################


#Reorder columns.  Will have to use variable
outputCSV = outputCSV[c("N_FORAGE_TYPES", "FORAGE_TYPE_NAME", "TOTAL_AREA_BY_FORAGE_TYPE", "AREA_UNIT", "TIME_VECTOR_FORAGE_TYPE", "FOOD_BIOMASS")]


# Write out data
write.csv(outputCSV, file=paste(workspace, "input_data_R_Test.csv", sep=""), quote=FALSE, row.names=TRUE)