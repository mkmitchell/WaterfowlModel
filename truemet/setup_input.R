# R script designed to import Waterfowl ArcGIS Model csv file and export
# an input file for truemet.
#
# Current works with Truemet 2.2
#
# Initial development by Mike Mitchell, Ducks Unlimited

# Variable designiation
# Workspace directory
workspace = "D:/GIS/projects/Waterfowl model/truemet/"
# Input ArcGIS Model csv file
arcin = "Nov_15_16_table.csv"
coverin = "cover_values.csv"
private_woody

# Read in data
arcData = read.csv(paste(workspace, arcin, sep=""), header=TRUE)
coverData = read.csv(paste(workspace, coverin, sep=""), header=TRUE)

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



combined$KG_HA=((combined$HECTARES*(combined$Z_HARVESTE*0.01)*combined$HRVST_KG_HA))+((1-combined$Z_HARVESTE*0.01)*combined$HECTARES*combined$UNHRVST_KG_HA)
combined$KG_HA= ifelse(combined$COVER_TYPE == "woody wetlands", combined$HECTARES * (combined$Z_RED_OAK_ * 1.2405 + 41.619), combined$KG_HA)

# Write out data
write.csv(df, file=paste(workspace, "input_data.csv", sep=""), quote=FALSE, row.names=TRUE)