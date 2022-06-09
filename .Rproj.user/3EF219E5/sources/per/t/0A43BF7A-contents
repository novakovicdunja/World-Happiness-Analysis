#ucitavanje paketa
library(bnlearn)
library(caret)
library(corrplot)
library(DescTools)
library(ggplot2)
library(e1071)
library(pROC)

#postavljanje vrednosti koja ce biti koriscena za seed
seed <- 3008

#ucitavanje seta podataka
df <- read.csv("world-happiness-report-2017.csv", stringsAsFactors = F)
summary(df)

#rekli ste da uklonim one zemlje koje imaju economy <=0
df <- subset(df,Economy.GDP.per.Capita>0)

#proveravam da li ima NA vrednosti
colSums(is.na(df))
#health life expectancy ima 2 NA i Generosity ima 1 NA
#provericu da li za kolonu Country ima vrednosti stringa koje su ""," " ili "-"
nrow(subset(df, Country == "" | Country == " " | Country == "-"))
#ne postoje takve vrednosti

#pre nego sto popravim vrednosti za health i generosity
#moram da vidim da li imaju normalnu raspodelu
#shapiro-wilk test
#nulta hipoteza - imaju normalnu raspodelu
#alternativna hipoteza - nemaju normalnu raspodelu
shapiro.test(df$Health.Life.Expectancy)
shapiro.test(df$Generosity)

#za obe varijable je p-value <0.05. To znaci da one nemaju normalnu
#Raspodelu. Zato cu koristiti medijanu da zamenim NA vrednosti jer 
#ta statistika bolje opisuje promenljive koje nemaju normalnu raspodelu
#od srednje vrednosti

#menjam vrednosti
df$Health.Life.Expectancy <- ifelse(is.na(df$Health.Life.Expectancy),
                                    median(df$Health.Life.Expectancy, na.rm=T),
                                    df$Health.Life.Expectancy)

df$Generosity <- ifelse(is.na(df$Generosity),
                                    median(df$Generosity, na.rm=T),
                                    df$Generosity)

#vidimo da vise nema autlajera
colSums(is.na(df))

#pre nego sto uradim sredjivanje outliera (ako takve vrednosti postoje),
#videcu koje promenljive cu da zadrzim

#promenljivu Country necu koristiti u predvidjanju jer je kategoricka ali sama po sebi
#ne govori nista o happiness score

#happiness rank cu da izbacim jer se racuna direktno iz target varijable
#pa je neprimenljiva, ne mozemo racunati target na osnovu promenljive koja
#je funkcija od target varijable

#whisker high i whisker low su gornja i donja granica target varijable
#pa je isto neprimenljiva

#izbacujemo te varijable
df.old <- df
df <- df[6:12]
df$Happiness.Score <- df.old$Happiness.Score

corrplot(cor(df), type="upper", method = "number")
ggplot(df, aes(x = Generosity, y = Happiness.Score)) + geom_point()

#sada cemo proveriti outliere
apply(df, 2, FUN = function(x){length(boxplot.stats(x)$out)})
#Family ima 3 outliera, trust govt corruption ima 2

#family ima outliere ispod minimuma
boxplot(df$Family)
boxplot(Winsorize(df$Family, probs = c(0.02,1)))
#uzecemo vrednost drugog percentila i dodeliti ga outlierima
df.use$Family <- Winsorize(df$Family, probs = c(0.02,1))

#trust.government.corruption ima outliere iznad maksimuma
boxplot(df$Trust.Government.Corruption)
boxplot(Winsorize(df$Trust.Government.Corruption, probs = c(0,0.99)))
#uzecemo vrednost 99og percentila i dodeliti ga outlierima
df$Trust.Government.Corruption <- Winsorize(df$Trust.Government.Corruption, probs = c(0,0.99))

#sada vidimo da nema vise outliera
apply(df, 2, FUN = function(x){length(boxplot.stats(x)$out)})

#pre nego sto krenemo sa naive bayesom, moramo da diskretizujemo
#numericke promenljive koje nisu normalno rasporedjene
#testiramo za normalnu raspodelu
#shapiro-wilk test
#nulta hipoteza - imaju normalnu raspodelu
#alternativna hipoteza - nemaju normalnu raspodelu
apply(df, 2, shapiro.test)
#Dystopia.Residual jedina ima norm raspodelu, ostale nemaju
df.to.discretize <- subset(df, select=-c(Dystopia.Residual))
df.discretized <- discretize(df.to.discretize, method = "quantile", breaks = c(5,5,5,5,5,5,5))
head(df.discretized)
df.use <- cbind(df.discretized, Dystopia.Residual = df$Dystopia.Residual)

#izmisljam neku izlaznu varijablu
happiness.3q <- quantile(df.old$Happiness.Score, 0.75)
df.use$Happy.Country <- ifelse(df.old$Happiness.Score >= happiness.3q, "Yes", "No")
table(df.use$Happy.Country)
df.use$Happy.Country <- factor(df.use$Happy.Country, levels = c("Yes", "No"))
df.use$Happiness.Score <- NULL
#delimo dataset na train i test (odnos 80% train, 20% test)

set.seed(seed)
#kreiranje indeksa
i <- createDataPartition(df.use$Happy.Country, p = 0.8, list  = F)
#kreiranje seta za treniranje
train <- df.use[i,]
#kreiranje seta za testiranje
test <- df.use[-i,]

#pravimo model
model1 <- naiveBayes(Happy.Country ~ ., data = train)
#koristimo ga za predikciju
pred1<- predict(model1, newdata = test, type = "class")

#pravimo matricu konfuzije
#Observed - realni podaci koji su u test datasetu
#Expected - ono sto je model predvideo
cm1 <- table(Observed = test$Happy.Country, Expected = pred1)

#pravim funkciju za izracunavanje eval. metrika

#objasnjenje eval. metrika
#TP - opservacije pripadaju pozitivnoj klasi i model ih je obelezio kao takve
  #jeste srecna zemlja i model je obelezio tako
#TN - opservacije pripadaju negativnoj klasi i model ih je obelezio kao takve
  #nije srecna zemlja i model je obelezio tako
#FP - opservacije pripadaju negativnoj klasi ali ih je model obelezio da pripadaju
#pozitivnoj klasi
  #nije srecna zemlja ali je model pogresno zakljucio da jeste
#FN - opservacije pripadaju pozitivnoj klasi ali ih je model obelezio da pripadaju
#negativnoj klasi
  #jeste srecna zemlja ali je model pogresno zakljucio da nije

#accuracy - odnos broja tacno klasifikovanih opservacija i ukupnog broja opservacija
#precision - od svih opservacija koje smo obelezili da pripadaju pozitivnoj klasi
  #koliko njih zaista pripada pozitivnoj
#recall - od svih opservacija koje su pozitivne, koliko njih je model zaista
  #prepoznao kao pozitivne

#f1 - mera koja predstavlja odnos precisiona i recalla. Sto je jedna metrika veca,
  #to je druga manja. F1 je metrika pomocu koje se odredjuje zadovoljavajuci odnos
  #izmedju precision i recall

compute.eval.metrics <- function(cm){
  tp <- cm[1,1]
  tn <- cm[2,2]
  fp <- cm[2,1]
  fn <- cm[1,2]
  acc <- (tp+tn)/(tp+tn+fp+fn)
  pr <- tp/(tp+fp)
  re <- tp/(tp+fn)
  f1 <- 2*pr*re/(pr+re)
  c(Accuracy = acc, Precision = pr, Recall = re, F1 = f1)
}

eval.model1 <- compute.eval.metrics(cm1)

#videcemo da optimizujemo model za bolji precision i recall
pred1.prob <- predict(model1, newdata = test, type = "raw")
roc.params <- roc(response = test$Happy.Country, predictor = pred1.prob[,1])
roc.params$auc
plot.roc(roc.params, print.thres = T, method = "youden")
roc.coords <- coords(roc.params, ret = c("accuracy", "spec", "sens", "thr"), x="best")
threshold <- roc.coords[1,4]

pred2 <- ifelse(pred1.prob[,1]>=threshold, yes = "Yes", no= "No")
pred2 <- factor(pred2, levels = c("Yes", "No"))
table(pred2)
#opet pravimo matricu konfuzije
cm2 <- table(Observed = test$Happy.Country, Expected = pred2)
eval.model2 <- compute.eval.metrics(cm2)

#uporedicemo 2 modela
model.comparison <- rbind(eval.model1, eval.model2)
#drugi model ima bolje sve metrike osim recall koji je isti
#accuracy je drasticno povecan
#precision je 1 sto znaci da nema lazno pozitivnih opservacija
#recall je isti kao i pre
#f1 pokazuje da su statistike precision
#i recall u boljem odnosu (smanjio se broj netacnih predikcija)