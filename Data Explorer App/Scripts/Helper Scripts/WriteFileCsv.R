# Recreating the File Settings File (if it got destroyed or corrupted)
fileSettingPath = "./Data/Settings/Files.csv"


# Initializing Default Settings
dataframe <- data.frame(matrix(nrow = 3, ncol = 0))

# Creating Format Column (to tell us what each row stands for)
dataframe$format <- c("Purpose", "File Name", "File Path")


# Migration Data Column
dataframe$migrationData <- c("Migration Data", "Migration Data.shp", "./Data/Migration Data")

# Raster Layer Column
dataframe$rasterLayers <- c("Raster Layers", "Layers.csv", "./Data/Raster Layers")

# Layer Define Column
dataframe$layerDefine <- c("Layer Define", "DataTypes.csv", "./Data/Raster Layers")

# Biological Year Column
dataframe$bioYear <- c("Biological Year", "Biological Year.csv", "./Data/Settings")


write.csv(dataframe, fileSettingPath, row.names = FALSE)

rm(fileSettingPath)
rm(dataframe)
