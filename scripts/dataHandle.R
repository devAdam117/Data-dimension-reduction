# prepojenie dvoch datovych suborov, kde sa najoinuju jeden s druhym cez id a zobere sa
# akesi zjednotenie informacii 
accountData <- read.csv('../data/ethAccs.csv', stringsAsFactors = TRUE, colClasses = c("id" = "character", "entity" = "character", "addressType" = "character"))
labeledAccounts <- read.csv('../data/labeledAddresses.csv')
accountData[accountData[,4] == '1', 4] <- 'Mining' 
accountData[, 12] <- tolower(accountData[, 12])
matchesIds <- accountData[accountData[, 12] %in% labeledAccounts[, 1],12]
matchIdx <- which(accountData[, 12] %in% matchesIds)
infoAcc <- labeledAccounts[labeledAccounts[, 1] %in% matchesIds, c(3, 5)]
accountData[matchIdx, 14] <- infoAcc[, 1]
accountData[matchIdx, 4] <- infoAcc[, 2] 