#libraries
library(caret)
library(corrplot)
library(ggplot2)
library(e1071)
library(class)
library(DescTools)

df <- read.csv("world-happiness-report-2017.csv", stringsAsFactors = F)
summary(df)

#knn

#sredjivanje NA vrednosti
colSums(is.na(df))


shapiro.test(df$Health.Life.Expectancy)
shapiro.test(df$Generosity)

df$Health.Life.Expectancy <- ifelse(is.na(df$Health.Life.Expectancy), 
                                    median(df$Health.Life.Expectancy, na.rm = T),
                                    df$Health.Life.Expectancy)

df$Generosity <- ifelse(is.na(df$Generosity), 
                                    median(df$Generosity, na.rm = T),
                                    df$Generosity)

#kreiranje target varijable
perc20 <- quantile(df$Happiness.Rank,0.2)

df$Top.20.perc <- ifelse(df$Happiness.Rank<perc20,"Yes","No")
df$Top.20.perc <- factor(df$Top.20.perc, levels = c("Yes", "No"))
table(df$Top.20.perc)

#odabir promenljivih za predvidjanje
  #ne mozemo predvidjati pomocu happiness rank i score zato sto su direktno povezane sa
  #target varijablom top.20.perc. To bi bilo kao kad bismo koristili target promenljivu
  #za predvidjanje same sebe. Isto zbog toga ne mozemo koristiti whisker high and whisker
  #low.

  #country ne mozemo koristiti jer nije numericka promenljiva i nije ordinalna kategoricka

  #ostale promenljive cemo ispitati scatterplotom i corrplotom

  #posmatracemo da li postoji korelacija numerickih promenljivih sa happiness score
  #kako su happiness score i happiness rank direktno povezani, ako je veza promenljivih
  #sa njima znacajna, bice znacajna i za Top.20.perc

#biramo manji dataset sa promenljivama koje su nam bitne
df.new <- subset(df, select = c(Happiness.Score, Economy.GDP.per.Capita, Family, Health.Life.Expectancy,
                                Freedom,Generosity, Trust.Government.Corruption, Dystopia.Residual))

corrplot(cor(df.new), type="upper", method = "number", tl.cex = 0.75, number.cex = 0.85)

#Gledamo prvi redcorrplota. Promenljive od znacaja su nam Economy.GDP.per.Capita
#Family, Health.Life.Expectancy, Freedom


#za sve promenjive crtamo scatterplot

#za ove promenljive: Trust.Government.Corruption, Generosity, Dystopia.Residua
#dijagram rasturanja je razbacan, ne uocava se linearna ni neka
#druga zavisnost sa happiness.score
ggplot(data = df.new, aes(x=Trust.Government.Corruption, y=Happiness.Score))+geom_point()
ggplot(data = df.new, aes(x=Generosity, y=Happiness.Score))+geom_point()
ggplot(data = df.new, aes(x=Dystopia.Residual, y=Happiness.Score))+geom_point()

#za ove promenljive:Economy.GDP.per.Capita, Family, Health.Life.Expectancy,
#Freedom se vidi linearna zavisnost
#lin zavisnost je najjaca za economy grp i family i health life expectancy
#i slabija je za freedom
ggplot(data = df.new, aes(x=Economy.GDP.per.Capita, y=Happiness.Score))+geom_point()
ggplot(data = df.new, aes(x=Family, y=Happiness.Score))+geom_point()
ggplot(data = df.new, aes(x=Health.Life.Expectancy, y=Happiness.Score))+geom_point()
ggplot(data = df.new, aes(x=Freedom, y=Happiness.Score))+geom_point()

#selektujemo promenljive
df.use <- subset(df.new, select = c(Economy.GDP.per.Capita, Family,
                                         Health.Life.Expectancy, Freedom))

df.try <- subset(df.new, select = -c(Happiness.Score))

#sredjujemo outliere
apply(X=df.try,2,FUN = function(x){length(boxplot.stats(x)$out)})
df$Family <- Winsorize(df$Family, probs = c(0.024,1))
df$Trust.Government.Corruption <- Winsorize(df$Trust.Government.Corruption, probs = c(0,0.99))

#radimo shapiro test kako bismo skalirali promenljive
apply(df.use, 2, shapiro.test)
#nijedna nije normalno rasporedjena, skaliramo ih prema medijani jer prosek
#nije dobra statistika za opis promenljive koja nije normalna

df.use <- apply(X = df.use, MARGIN = 2, FUN = function(x){scale(x,center = median(x), scale=IQR(x))})
df.use <- data.frame(df.use)

df.try <- apply(X = df.try, MARGIN = 2, FUN = function(x){scale(x,center = median(x), scale=IQR(x))})
df.try <- data.frame(df.try)

#dodajemo target promenljivu
df.use$Top.20 <- df$Top.20.perc
df.try$Top.20 <- df$Top.20.perc

#splitujemo dataset na train i test
set.seed(12345)
#stavili smo seed zbog reprodukcije, delimo na 80% train i 20% test premma
#promenljvoj Top.20 jer je ona target
i <- createDataPartition(df.try$Top.20, p = 0.8, list=F)
train <- df.try[i,]
test<- df.try[-i,]


#naci cemo najbolju vrednost k preko kros validacije

#biramo metod kros validacije za kontrolu treniranja, biramo 10 particija
numFolds <- trainControl(method="cv", 10)

#testiracemo broj komsija od 3 do 25
cpGrid <- expand.grid(.k = seq(3,25,2))

set.seed(12345)
cv.model <- train(x = train[-8], y = train$Top.20,
                  method = "knn", trControl = numFolds, tuneGrid = cpGrid)

#najbolje k = 3
best.k <- cv.model$bestTune$k

#koristimo ga za pravljenje modela
set.seed(12345)
model <- knn(train = train[-8], test = test[-8], cl = train$Top.20, k= best.k)

#matrica konfuzije
  #Observed - prave vrednosti (iz test seta)
  #Expected - pogadjane vrednosti (iz modela)
confusion.matrix <- table(Observed = test$Top.20, Expected = model)

#Evaluacione metrike
#True Positive TP - stvarno jesu top 20 i model je tako predvideo (3)
#True Negative TN - stvarno nisu top 20 i model je tako predvideo (23)
#False Positive FP - nisu top 20 ali je model predvideo da jesu (1)
#False Negative FN - jesu top 20 ali je model predvideo da nisu (3)

compute.eval.metrics <- function(cmatrix) {
  TP <- cmatrix[1,1] # true positive
  TN <- cmatrix[2,2] # true negative
  FP <- cmatrix[2,1] # false positive
  FN <- cmatrix[1,2] # false negative
  acc <- sum(diag(cmatrix)) / sum(cmatrix)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- 2*precision*recall / (precision + recall)
  c(accuracy = acc, precision = precision, recall = recall, F1 = F1)
}

compute.eval.metrics(confusion.matrix)


