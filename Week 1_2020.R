# Link to Challenge: https://preppindata.blogspot.com/2020/01/2020-week-1.html
# Link to Data: https://drive.google.com/file/d/1t1n43wJKa8imKg-2uJWvWWTfsPuaPsLh
# Output: Tab delimited txt file named "PD 2020 WK 1 Output.txt"

rm(list = ls())
dataID <- "1t1n43wJKa8imKg-2uJWvWWTfsPuaPsLh"
profitData <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", dataID))


# dummy variables for heading levels
heading1 <- "^\\d{1}\\.[a-zA-Z\\s]*$" # regex for first level heading i.e. "1."
heading2 <- "^\\d{1}\\.\\d{1}[a-zA-Z\\s]*$" # regex for level 2 heading i.e. 2.2

profitData$isFirstHeading <- ifelse(grepl(heading1, profitData$Item, perl = TRUE),  1, 0)
profitData$isSecondHeading <- ifelse(grepl(heading2, profitData$Item, perl = TRUE), 1, 0)


# label the levels/groups ----
profitData$firstLevel <- substr(profitData$Item, start = 1, stop = 1)

# if not 1st or 2nd level, take first 3 chars as group identifier
profitData$secondLevel <- with(profitData, ifelse(!isFirstHeading == 1 & !isSecondHeading == 1,
                                                  substr(Item, start = 1, stop = 3), NA))

# Calculate Profits ----

profitData[profitData$isFirstHeading == 1,"Profit"] <- tapply(profitData$Profit, profitData$firstLevel, FUN = sum, na.rm = TRUE) # first level profits

profitData[profitData$isSecondHeading == 1, "Profit"] <- tapply(profitData$Profit, profitData$secondLevel, FUN = sum, na.rm = TRUE)
# profitData$secondLevel <- ifelse(!grepl(heading1, profitData$Item, perl = TRUE), substr(profitData$Item, start = ))



# Leading Spaces ----

profitData$Item <- with(profitData, ifelse(!isFirstHeading == 1 & !isSecondHeading == 1,
                                           sprintf(paste0("%", nchar(Item)+10, "s"), Item),
                                           Item)) # 10 spaces for X.X.X items

profitData$Item <- with(profitData, ifelse(isSecondHeading == 1,
                                           sprintf(paste0("%", nchar(Item)+5, "s"), Item),
                                           Item)) # 5 spaces for X.X items


# Write Output (csv will screw up leading whitespaces) ----
profitData <- profitData[,c("Item", "Profit")]

write.table(profitData, "PD 2020 WK 1 Output.txt", sep = "\t", row.names = FALSE)
