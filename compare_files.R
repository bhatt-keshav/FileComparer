# Load Dependencies
library(tidyverse)
library(readtext)
# library(vecsets)
library(magrittr)

# change path to your local dir
path <- "C:/BhattK"
setwd(path)
'%ni%' <- Negate('%in%')

# TODO: Make this generic

# Step 1: Read all files from the location and make checks
pcFiles <- list.files(path = "./outputPowercurve/score",full.names = F)
sdsFiles <- list.files(path = "./outputNBSMg3/score",full.names = F)
ifelse(length(pcFiles) == length(sdsFiles), "Same number of files", "Number of files in each folder is different, CHECK!")

## There should be no falses, if there is a false, fix the unmatched filename and the length should be same
ifelse(sdsFiles %in% pcFiles %>% sum(., na.rm = TRUE) == length(pcFiles), "File names are same in both folders", "Names of files in each folder is different, CHECK!")

# Step 2: Read the files
pcFilesTxt <- readtext("./outputPowercurve/score/*") 
sdsFilesTxt <- readtext("./outputNBSMg3/score/*") 
pcFilesTxt$charL <- nchar(pcFilesTxt$text) 
sdsFilesTxt$charL <- nchar(sdsFilesTxt$text)
merged <- merge(pcFilesTxt, sdsFilesTxt, by = "doc_id") 
merged %<>% rename(pcText = text.x, sdsText = text.y,
                   pcCharL = charL.x, sdsCharL = charL.y) %>% as_tibble()


# Step 3: Compare the text files
merged %<>% mutate(., areFilesL = ifelse(pcCharL == sdsCharL, "Same length files", "Different length files"))

## A difference here signifies a problem
table(merged$areFilesL)

merged$sameVals <- NA
merged$sameNames <- NA
merged$mismatch <- NA
valMis <- NA
nameMis <- NA

largerVector <- function(v1, v2) {
  ifelse(max(length(v1), length(v2)) == length(v1), 
         largerV <- v1, largerV <- v2)
  return(largerV)
}

smallerVector <- function(v1, v2) {
  ifelse(min(length(v1), length(v2)) == length(v1), 
         smallerV <- v1, smallerV <- v2)
  return(smallerV)
}

for (i in 1: nrow(merged)) {
# Initializing
  # Testi <- 1
  p <- merged$pcText[i]
  s <- merged$sdsText[i]
  
  # Checking names
  pName <- str_extract_all(p, "result_.*\t") %>% unlist
  sName <- str_extract_all(s, "result_.*\t") %>% unlist
  largerName <- largerVector(pName, sName)
  ifelse(length(intersect(pName, sName)) == length(largerName), 
         merged$sameNames[i] <- "Names are same",
         merged$sameNames[i] <- "Names are different, CHECK!") 

  # Checking values
  pVal <- str_split(p, "result_.*\t") %>% unlist
  sVal <- str_split(s, "result_.*\t") %>% unlist
  pVal <- pVal[-1]; sVal <- sVal[-1]
  d <- abs(length(pVal) - length(sVal))
  ## Make both vectors of same length, if that's not the case
  if (d>0) {
    largerVal <- largerVector(pVal, sVal)
    smallerVal <- smallerVector(pVal, sVal)
    smD <- length(smallerVal)
    smallerVal[seq(smD+1, smD+d)] <- NA  } else
    {
      largerVal <- pVal
      smallerVal <- sVal
    }
  
  # Get position of the mismatch
  pos <- which(largerVal != smallerVal)
  posChar <- as.character(pos) %>% paste(.,collapse = ", ")
  ifelse(length(pos)>0, 
         merged$sameVals[i] <- paste("Values are different at positions:", posChar),
         merged$sameVals[i] <- "Values are same")

  # Display exact mismatch that has occured
  valMisLrgr <- str_replace_all(largerVal[pos], c("\n" = "", "\t" = ""))
  valMisSmlr <- str_replace_all(smallerVal[pos], c("\n" = "", "\t" = ""))
  nameMis <- gsub("\t", "", largerName[pos])
  merged$mismatch[i] <- paste("Mismatch for:", nameMis, valMisLrgr, "vs", valMisSmlr, sep = " ", collapse = " & ")
  if (length(pos) == 0) {merged$mismatch[i] <- "No mismatch"}
}

