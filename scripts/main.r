accountData <- read.csv('../data/mainData.csv', stringsAsFactors = TRUE, colClasses = c("id" = "character", "entity" = "character", "addressType" = "character"))
# Prehlad dat
  # prvych par riadkov bez prveho stlpca, prvy stlpec ukayuje priebeh balancu v kazdom bloku. z toho sa pocital priemer a ostal v datovej sade kdyby 'neco',, tj. mohol mat nejake vyuzitie.
  accountData[1:5, -c(1) ]
  # nejake statistiky
    # pocet roznych uctov
    length(accountData[,1])
    # kolko je z danych penazeniek istotne s oznacecim (miner,exchange,defi), tj mame olablovanych 39 entit a ostatne su nezname ale podla znamych statistik penazenky sukr. osob
    length(accountData[accountData[,4] ==  'Mining', 1])
    length(accountData[accountData[,4] ==  'DeFi', 1])
    length(accountData[accountData[,4] ==  'Exchange', 1])
    length(accountData[accountData[,14] ==  'Wallet', 1])
    length(accountData[accountData[,14] ==  'Smart Contract', 1])
    # priemery a mediany pre
    # minBalance maxBalance countAsSender totalSent totalReceived coununtAsReceiver  avgSent avgReceived avgBalance
    # pre countAsSender, countAsReceiver nie je jednoznacna jednotka, mozno som pri parsingu prehliadol a urobil nejaku malu chybicku
    colMeans(accountData[, c(2, 3, 5, 6, 7, 8, 9, 10, 13)])
    apply(accountData[, c(2, 3, 5, 6, 7, 8, 9, 10, 13)], 2, median)
    # teraz si pozrieme plot velkosti balancu vsetkych penazeniek v log skale 
    plot(sort(log(accountData[,13]), decreasing = TRUE),
            xlab = 'Poradie adresy',
            ylab = 'avgBalance (ETH)',
            xlim = c(0,10000),
            type = 'l',
            lwd = 2,
            axes = FALSE
         )
    axis(1, at = seq(0,10000, by = 1000), 
         labels = c("0k", "1k", "2k", "3k", "4k", "5k", "6k", "7k", "8k", "9k", "10k"))
    xValues <- seq(1,10001,1000)
    yValues <- sort(log(accountData[,13]), decreasing = TRUE)[xValues]
    points(xValues, yValues, col='red', pch= 19)
    labels <- sort(accountData[,13], decreasing = TRUE)[xValues]
    text(1700, yValues[1]  , floor(labels[1] * 1000) / 1000, pos = 2)
    text(xValues[2:6], yValues[2:6], floor(labels[2:6] * 1000) / 1000, pos = 3)
    text(xValues[7:10], yValues[7:10], floor(labels[7:10] * 100000) / 100000, pos = 3)
    # zameriame sa iba na prvych 50 v log skale
    numOfOutlierRemoval <- 1:300
    meanBalancesWithoutOutliers <- sapply(numOfOutlierRemoval, function(num){
      balancesWuthoutOutliers <- accountData[!(accountData[, 13] %in% sort((accountData[, 13]), decreasing = TRUE)[1:num]), 13]
      return(mean(balancesWuthoutOutliers))
    })
    plot(meanBalancesWithoutOutliers,
         xlab = 'Pocet vynechanych najobj. adries',
         ylab = 'Priemerny balance nevynechanych adries',
         type = 'l')
    abline(h=0, col="red")

# PCA
    # vyberieme len data, s ktorymi v pca budeme narabat
    pcaData <- accountData[,c(2,3,5,6,7,8,9,10,13)]
    colors <- rep('black', 10049)
    colors[accountData[,4] == 'Exchange'] <- 'blue'
    colors[accountData[,4] == 'Mining'] <- 'red'
    colors[accountData[,4] == 'DeFi'] <- 'green'
    
    # pozrieme si ako jednotlive premenne spolu koreluju
    library(corrplot)
    corrplot(cor(pcaData), method = "circle")
    pcaResult <- prcomp(pcaData, scale = TRUE)  
    summary(pcaResult)
    # lepsie pca zobrazenie 
    library(ggfortify)
    autoplot( pcaResult, 
              data=pcaResult, 
              col = colors, 
              loadings=TRUE,
              loadings.label = TRUE)
   
    # to je fajn ale ten jeden ma obrovsky balance a kvoli tomu ostatnych balance sa tazsie vie navzajom provnavat, tak ho odstranime
    maxIdx <- which.max(pcaData[,9])
    pcaData <- pcaData[-maxIdx,]
    pcaResult <- prcomp(pcaData, scale = TRUE)  
    colors <- colors[-maxIdx]
    autoplot( pcaResult, 
              data=pcaResult, 
              col = colors, 
              loadings=TRUE,
              loadings.label = TRUE)
    # ti co robia posielacie ukony, tak maju vacsi balance uctu ako ti co prijimaju?
    count <- 0 
    for(i in 0:33){
      # countSent vs countReceived
      if(mean(pcaData[pcaData[,3] > i, 9]) >= mean(pcaData[pcaData[,6] > i, 9])) count <- count + 1
      # totalSent vs totalReceived
      if(mean(pcaData[pcaData[,4] > (i * 100), 9]) >= mean(pcaData[pcaData[,5] > (i * 100), 9])) count <- count + 1
      # avgSent vs avdReceived
      if(mean(pcaData[pcaData[,7] > i , 9]) >= mean(pcaData[pcaData[,8] > i, 9])) count <- count + 1
    }
    # v kolkych pripadoch bol mean(pcaData[,4])_posielacie >  mean(pcaData[,4])_prijimacie  
    sendingScore <- count/((i + 1) * 3)
    # 3D PCA
    library(rgl)
    scale <- 100
    options(rgl.printRglwidget = TRUE)
    plot3d(pcaResult$x[,1:3], col=colors)
    coords <- NULL
    for (i in 1:nrow(pcaResult$rotation)) {
      coords <- rbind(coords, rbind(c(0,0,0),pcaResult$rotation[i,1:3]))
    }
    
    text3d(pcaResult$rotation[,1:3] * scale , 
           texts=rownames(pcaResult$rotation), 
           col="red", 
           cex=0.8)
    coords <- coords * scale
    lines3d(coords, col="red", lwd=2)
    
# Predikcny model s vyuzitim kombinacie LASSA a validacnej vzorky
    set.seed(123)
    # data rozdelime na 70/30, 70 bude v kazdom pripade trenovacia cast a 30 bude validacna pre pouzitie lassa a testovacia pre pouzitie finalnych reg. modelov
    sampledData <- accountData[sample(nrow(accountData)),]
    trainData <- sampledData[1:floor(.7*nrow(sampledData)),]
    # data entity zmenime na kategorialne - zaujimava adresa/ nezaujimava adresa, kde zaujimava %in% c('Exchange','DeFi', 'Mining'), nezaujimava opacne.
    validateData <- sampledData[(floor((.7*nrow(sampledData)) + 1) : nrow(sampledData)),]
    validateData[validateData[,4] == '', 4] <- 0
    validateData[validateData[,4] != 0, 4] <- 1
    # toto moze vyzerat ako hriech ale nizsie to je vysvetlene
    testData <- validateData
    numericColumnsIdxs <- c(2,3,5,6,7,8,9,10,13)
    # Najprv vytvorime logisticky model na trenovacich datach a ukazeme uspesnost na testovacich s vyuzitim vsetkych premennych
    logisticModelAll <- glm(factor(entity) ~ . + 1, data = trainData[, c(4,numericColumnsIdxs)], family = binomial)
    predictions <- predict(logisticModelAll, testData[, c(4,numericColumnsIdxs)], type = 'response')
    # preklasifikujeme pravdepodobnosti
    predictions[predictions >= 0.5] <- 1
    predictions[predictions < 0.5] <- 0
    # ukazeme vysledky klasifikacie na test. datach. 3002 spravne oznacilo nulou, 4 sparvne oznacilo 1 tkou ale 9 nespravne oznacilo 0 aj ked boli jednotkove
    contingencyTable <- table(Actual = as.numeric(testData[,4]), Predicted = predictions)
    
    
    # teraz ale chceme chceme extrahovat premenne, ktore su tam mozno zbytocne, myslime si, ze by sme to mohli dosiahnut dosledkom vacsich korelacnych koeficientov medzi premennymi
    library(glmnet)
    #  vytvorime viacero LASSO modelov
    X <- as.matrix(trainData[, numericColumnsIdxs])
    Y <- trainData[,4]
    Y[trainData[,4] == ''] <- 0
    Y[trainData[,4] != ''] <- 1
    Y <- as.numeric(Y)
    # po par iteraciach sa lambdy nemenia, tak skonci skorej ako pri 100ke
    lassoRet <- glmnet(X, Y, family = "binomial", alpha = 1, intercept = FALSE, standardize = TRUE)
    # pouzijeme ho ako predikciu na validacne data
    pred <- as.matrix(predict(lassoRet, as.matrix(validateData[, numericColumnsIdxs]), type = 'class'))
    # zvolime si index lasso modelu s najlepsim vysledkom na validacnych datach, ten co najmenejkrat missklasifikoval
    idx <- NULL
    values <- c()
    minMissClassified <- Inf
    for (i in 1:ncol(pred)) {
      missClassified <- 0 
      vec <- as.vector(pred[,i])
      missClassified <- sum(vec != validateData[, 4])
      values <- append(values,missClassified)
      if(missClassified <= minMissClassified) {
        minMissClassified <- missClassified
        idx <- i
      }
    }
    par( mfrow= c(1,2) )
    plot(lassoRet$lambda, values,  type="l", lwd=2, ylab = "# missklasifikacie", main="# missklasifikacii")   
    abline(h = minMissClassified,  col = 'green')
    points(lassoRet$lambda[idx], values[idx], pch=16, cex=1.5, col="red")
    axis(2, at = minMissClassified, labels = minMissClassified)
    
    poctyPrem <- apply(abs(lassoRet$beta)>1e-9, 2, sum)
    plot(lassoRet$lambda, poctyPrem, type="l", lwd=2, main="Nenulove koeficienty")
    points(lassoRet$lambda[idx], poctyPrem[idx], pch=16, cex=1.5, col="red")
    
    # pre zaujimavost najlepsie LASSO klasifikovalo takto 
    contingencyTable2 <- table(Act = as.numeric(testData[,4]), Predic = pred[,idx])
    # ... to znamena ze oznacil vsetkych ako 0
    
    #vieme ktore LASSO je opt, kukneme ho a podla neho zostavime logisticku reg
    optLasoModel <- lassoRet$beta[,idx]
    # pomocou danych troch premennych postavame povodny log model cisto z trenovacich dat
    logisticModel <- glm(factor(entity) ~ 1 + avgReceived + avgSent + countAsSender, data = trainData[, c(4,5,9,10)], family = binomial)
    # takyto logisticky model ma najsilnejsiu vieru v countAsSender, mozno aj kvoli tomu lebo to zahrna informaciu balancu....
    # uvedmme si krok, lasso modely boli vytvorene na trenovacich datach (70%) a vybraty najlepsi bol na validacnych=testovacich datach (30%) pomocou neho sa vytvorila log. regresia na tych istych 70% tren. datach a vyhodnotena bola
    # na zvysnych 30% testovacich = trenovacich datach
    predictions <- predict(logisticModel, testData[, c(4, 5, 9, 10)], type = 'response')
    predictions[predictions >= 0.5] <- 1
    predictions[predictions < 0.5] <- 0
    # uspesnost zobrazena v kontingecke, o nieco horsi ako v prvom modeli ale na to ze predikuje pomocou 3 premennych, to nie je vobec hrozne
    contingencyTable2 <- table(Actual = as.numeric(testData[, 4]), Predicted = predictions)
    
    