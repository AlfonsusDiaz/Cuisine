chefmoz <- read.csv('chefmozcuisine.csv', sep = ',', fileEncoding = "UTF-8-BOM")
geo <- read.csv('geoplaces2.csv', sep = ',')
rating <- read.csv('rating_final.csv', sep = ',', fileEncoding = "UTF-8-BOM")

#di merge / di join
chefmozxgeo <- merge(chefmoz, geo, by = "placeID")
#di cek lur
nrow(chefmozxgeo)

#1a
#ngitung kuantitas jenis makanan yg plg sering dijual di restoran
table(chefmozxgeo$Rcuisine)
#divalidasi / filter coy
num1 <- table(chefmozxgeo$Rcuisine)
num1 <- num1[num1>5]
num1
pie(num1, main = "Cuisines Distribution")

#1b
#ngitung di restoran ini ada brp byk jenis makanan 
#berapa makanan yg ada di masing" restoran
chefmozxgeo$placeID
num1b <- table(chefmozxgeo$placeID)
num1b
hist(num1b,
     main = "Cuisines Count Frequency based on Restaurant",
     xlab = "Cuisines Count",
     col = "light blue"
     )

#1c
ratingxgeo <- merge(geo, rating, by = "placeID")
#dicek-------
nrow(ratingxgeo)
nrow(geo)
#------------
ratingxgeo$state
#bwt kolom baru
ratingxgeo$avg_ratings <- (ratingxgeo$rating + 
                             ratingxgeo$food_rating + 
                             ratingxgeo$service_rating) / 3
ratingxgeoabove <- ratingxgeo[ratingxgeo$avg_ratings > 1.2, ]
#cara NGECEK INGET
table(ratingxgeoabove$state)

#ubah semua state jadi huruf kecil
ratingxgeoabove$state <- tolower(ratingxgeoabove$state)
#ubah jadi fix slp
ratingxgeoabove$state <- ifelse(ratingxgeoabove$state=='s.l.p.'|
                                  ratingxgeoabove$state=='san luis potos'|
                                  ratingxgeoabove$state=='san luis potosi',
                                'slp',
                                ratingxgeoabove$state)
#dicek lagi bwt barplot-nya
table(ratingxgeoabove$avg_ratings, ratingxgeoabove$state)

#bkin variable baru
avgrat_state_barplot <-table(ratingxgeoabove$avg_ratings, ratingxgeoabove$state)
barplot(avgrat_state_barplot, beside = TRUE, 
        main = "Average Ratings Distribution\nbased on the State of Restaurant",
        xlab = "State",
        col = rainbow(nrow(avgrat_state_barplot))
        )
nrow(avgrat_state_barplot)
#t() --> dibalik

#2 APRIORI
#13.20 -->180mnt 16.20 -->20mnt 16.40
#a. data preprocessing
#di join dlu sama geo, soalnay ya emg gtu soalnya
#RemoveFranchise
chefmozxgeo <- merge(chefmoz, geo, by = "placeID")
table(chefmozxgeo$franchise)
rmdata <- chefmozxgeo[chefmozxgeo$franchise=='f', ]
nrow(rmdata)
rmdata

#RemoveOtherServices
table(rmdata$other_services)
rmdata <- rmdata[rmdata$other_services=='none', ]
rmdata

#RemoveCountryNotDefined
table(rmdata$country)
rmdata <- rmdata[rmdata$country!='?', ]

#ReplaceCuisineName
table(rmdata$Rcuisine)
rmdata$Rcuisine <- gsub(pattern = "_", replacement = " ", rmdata$Rcuisine)
table(rmdata$Rcuisine)


#b data transformation
datatrans <- split(rmdata$Rcuisine, rmdata$placeID)
datatrans

library(arules)
freqcui <- apriori(data = datatrans, parameter = list(supp=0.008,
                                                      target = "frequent itemsets"))
inspect(freqcui)

#associationRules
ruleass <- ruleInduction(freqcui, confidence=0.8)
inspect(ruleass)