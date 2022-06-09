#ucitavanje paketa
library(ggplot2)
library(corrplot)
library(DescTools)
library(clusterSim)
library(ggplot2)

#postavljanje seed vrednosti zbog reprodukcije rezultata
seed <- 3008

#ucitavanje utility datoteke
source("Utility.R")

df <- read.csv("world-happiness-report-2017.csv", stringsAsFactors = F)
summary(df)
#trazite da se uzmu u obzir zemlje sa economy >0, filtriram podatke
df <-subset(df, Economy.GDP.per.Capita>0)

#sredjivanje podataka

#proveravam da li ima NA vrednosti
colSums(is.na(df))

#postoje 2 NA za Health i 1 NA za Generosity
#provericu raspodelu ove dve varijable i ako je normalna, zamenicu vrednost
#prosekom, ako nije, zamenicu vrednost medijanom
#razlog za to je sto prosek kao statistika dobro predstavlja varijablu
#sa norm raspodelom
#kad raspodela nije normalna, funkcija gustine obicno naginje na levu ili desnu stranu
#i tu prosek nije dobar pokazatelj nego medijana

apply(subset(df, select = c(Health.Life.Expectancy, Generosity)), 2, shapiro.test)

#nulta hipoteza - ima normalnu raspodleu
#altern. hipoteza - nema normalnu raspodelu
#p-value < 0.05, odbacujemo prvu hipotezu
#NA vrednosti menjamo medijanom

df$Health.Life.Expectancy <- ifelse(is.na(df$Health.Life.Expectancy),
                                    median(df$Health.Life.Expectancy, na.rm = T), 
                                    df$Health.Life.Expectancy)

df$Generosity <- ifelse(is.na(df$Generosity),
                                    median(df$Generosity, na.rm = T), 
                                    df$Generosity)

#sada vise nema NA vrednosti
colSums(is.na(df))

#prvo cu da odaberem promenljive za clusterovanje pa cu onda da resim outliere
#videcu da li postoje neke visoko korelisane promenljive pa cu da ih uklonim
#ako postoje

corrplot(cor(df[2:12]), method="number", type = "upper", bg = "black", 
         tl.cex = 0.75, number.cex = 0.85)

#happiness rank, happiness score, whisker high i whisker low su izuzetno
#jakomedjusobno korelisani, sto ima smisla jer su oni izracunati jedni na osnovu
#drugih

#dovoljno je zadrzati jednu meru od ovih i ja biram happiness score
#razlog> whisker high i whisker low samo opisuju granice od happiness score
#pa nisu bitni koliko sam score
#happiness rank je izveden iz happiness score rangiranjem pa je bolje imati
#originalnu informaciju nego rangiranje te orig. informacije

#uklanjam 3 kolone koje sam navela
df <- subset(df, select = -c(Happiness.Rank, Whisker.high, Whisker.low))
#!!! Takodje necu koristiti ime zemlje za klasterizaciju ali cu ga
#zadrzati u datasetu zbog potencijalne kasnije analize zemalja
#po klasterima

#sada cu da sredim outliere jer je analiza osetljiva na to

apply(X = df[2:9], 2, FUN = function(x){length(boxplot.stats(x)$out)})

#Family ima 3 outliera i Trust govt corruption ima 2 outliera

#proveravam gde su outlieri, za family su dole ispod "minimuma"
boxplot(df$Family)
#pomocu funkcije winsorize tim vrednostima dodeljujem vrednost
#drugog percentila
boxplot(Winsorize(df$Family, probs = c(0.02, 1)))
df$Family <- Winsorize(df$Family, probs = c(0.02, 1))

#outlieri su gore iznad maksimuma
#njima dodeljujem vrednost 99-og percentila
boxplot(df$Trust.Government.Corruption)
boxplot(Winsorize(df$Trust.Government.Corruption, probs = c(0, 0.99)))
df$Trust.Government.Corruption <- Winsorize(df$Trust.Government.Corruption, probs = c(0, 0.99))

#sad se vidi da nema vise outliera
apply(X = df[2:9], 2, FUN = function(x){length(boxplot.stats(x)$out)})

#potrebno je da normalizujem podatke
df.not.norm <- df
df[2:9] <- data.Normalization(df[2:9], type = "n4", normalization = "column")

#napravicu nekoliko modela sa razlicitim brojem klastera i utvrditi koji je najbolji
#pravim prazan dataframe u koji cu posle da dodam statistike za svaki model
eval <- data.frame()
for(i in 2:10){
  #pravim modele od 2 kohorte do 10 kohorti
  #postavljam vrednost seeda za reprodukciju rezultata
  set.seed(seed)
  #pravim model, maks broj iteracija je 20, broj random setova je 1000
  model <- kmeans(df[2:9], centers = i, iter.max = 20, nstart = 1000)
  eval <- rbind(eval, c(i, model$tot.withinss, model$betweenss/model$totss))
}

names(eval) <- c("k","total.within.ss","ratio")

#pomocu scree testa (elbow metode) pogledacu da li mogu da odredim optimalan
#broj klastera

ggplot(eval, aes(x=k, y=total.within.ss))+geom_line()
#prelom ("lakat") je u tacki k=3 (tri kohorte)
#pogledacu da li je to dobra solucija i pomocu compute differences funkcije

differences <- data.frame(K=c(2:10), 
                          tot.w.ss.difference = compute.difference(eval$total.within.ss),
                          ratio.difference = compute.difference(eval$ratio))

#Kako bismo interpretirali rezultate, moramo znati sta su sledeci pojmovi
  #within sum of squares - suma kvadrata udaljenosti opservacije u klasteru
    #od centroida tog klastera
    #ovo je mera "kohezivnosti klastera", tj koliko su opservacije u klasteru
    #blizu jedna druge

  #between sum of squares - suma kvadrata udaljenosti centroida svakog klastera
    #od prosecne vrednosti celog uzorka
    #ovo je mera toga koliko su centroidi (samim tim i klasteri) udaljeni jedni od drugih
    #ako su centroidi mnogo bliski, to dovodi u pitanje opravdanost
    #postojanja tih klastera kao odvojenih entiteta

  #total sum of squares - suma kvadrata udaljenosti svake opservacije od prosecne
    #vrednosti na celom uzorku
    #ovo je mera rasprsenosti opservacija uopste, bez uzimanja pripadnosti klasteru
    #u obzir

  #between ss/total ss - mera koja odredjuje koliko su dobro opservacije rasporedjene
  #po klasterima


#iz differences dataseta vidimo da je najvece smanjenje u vrednostima total
#within ss i ratio napravljeno kad je broj klastera presao sa 2 na 3
#posle se povecavanjem broja klastera ove metrike jako malo menjaju
#to znaci da je doslo do "preloma"/lakta za k=3
#stoga se odlucujem da napravim model sa 3 klastera

set.seed(seed)
new.model <- kmeans(df[2:9], centers = 3, iter.max = 20, nstart = 1000)

#interpretacija modela
new.model
df$cluster <- new.model$cluster

#pravljenje grafikona
create_comparison_plots(df[2:5], as.factor(df$cluster))
create_comparison_plots(df[6:9], as.factor(df$cluster))

#racunanje sumarnih statistika
summary <- summary.stats(df.not.norm[2:9], df$cluster, 3)
#broj instanci po klasteru
  #prvi klaster - 29 instanci
  #drugi klaster - 76 instanci
  #treci klaster -49 instanci

  #klasteri nisu medjusobno balansirani po broju instanci koje im pripadaju

#centri klastera
  #prvi klaster se istice po najboljim vrednostima za sve varijable osim
  #dystopia residual
  #ovom klasteru dakle pripadaju visoko razvijene zemlje koje imaju 
  #visok stepen srece, visoko razvijenu ekonomiju, dug zivotni vek,
  #slobodu, poverenje u vladu i drugo
  df$Country[df$cluster == 1]
  #vidi se da su to uglavnom bogate zemlje u kojima vlada red i zakon
  #kao sto su Norveska, Svedska, Nemacka, Svajcarska, Austrija, Island, Holandija
  #ima izuzetaka kao sto su uzbekistan i turkmenistan koje nisu bogate,
    #ali ocigledno imaju zadovoljno stanovnistvo, slobodu, poverenje u vladu
  
  #drugi klaster je "srednji klaster" - ove zemlje nisu najbolje, ali nisu ni
  #najgore
  #kotiraju se losije u svemu od prvog klastera osim za dystopia residual,
  #gde su bolje za 0.03 (prakticno beznacajno)
  #od treceg klastera su bolje u svemu osim u darezljivosti (manje su dare
    #zljive zemlje od zemalja treceg klastera) i u korupciji i poverenju vladi
    #zemlje srednjeg klastera su izgleda vise korumpirane i narod je izgubio poverenje
    #u drzavne organe
  df$Country[df$cluster == 2]
  #medju ovim zemljama se nalaze post-socijalisicke zemlje (ceska republika, ukrajina,
  #litvanije, letonija)
  #zemlje bivse jugoslavije i zemlje balkana,
  #neke zemlje koje imaju slabiji ekonomski rast (spanija,
  #italija, grcka), zemlje latinske amerike (el salvador, nikaragva, kolumbija, gvatemala)
  #nerazvijene zemlje azije (moldavija, kazahstan)
  
  #moze se videti da je stanovnistvo u ovim zemljama srednje zadovoljno,
  #ekonomski rast nije toliko veliki i ne veruje se drzavnim institucijama (korumpirane zemlje)
  
  #treci klaster je "bototm cluster" - najsiromasnije zemlje, zemlje
    #sa slabim ekonomskim rastom, skracenim zivotnim vekom, manjim gradjanskim
    #slobodama
  df$Country[df$cluster == 3]  
  #u ovom klasteru su uglavnom africke zemlje sa slabim ekonomijama
  #(Gana, Sudan, Niger, Zimbabve)
  #takodje azijske zemlje sa losim uslovima za zivot i smanjenim gradjanskim pravima
  #(npr Indija i Kambodza respektivno)
  #u mnogim zemljama vladaju ekstremisticki rezimi i ugrozena su prava stanovnistva
  #(npr zena, verskih i nacionalnih manjina) - Egipat, Afganistan, Irak, Sudan
  
  #u totalu, sa izuzecima, moze se videti da je klasterovanje otprilike sledece
    #prvi klaster - zemlje prvog sveta, razvijene, demokratije
    #drugi klaster - zemlje "drugog" sveta/postsocijalisticke, zemlje latinske amerike
    #ili zemlje manjeg ekonomskog rasta, srednjeg zadovoljstva naroda, korumpirane
    #treci klaster - zemlje treceg sveta, nerazvijene, ljudska prava ugrozena,
      #los zivotni vek
  
#sto se tice disperzije od centra prvi klaster je vrlo koncentrisan,
    #standardna devijacija je jako mala. To znaci da su zemlje uglavnom slicne,
    #imaju jaku pripadnost klasteru i "ne beze" iz njega
    
  #standardna devijacija za drugi klaster je mala za srecu, ekonomiju, porodicu,
  #dok je veca za slobodu, darezljivost, korupciju i distopiju
  #to znaci da se ove zemlje poklapaju sto se tice njihovog ekonomskog rasta,
    #porodicne strukture, slobode stanovnistva
    #ali je razlika u darezljivosti zemalja i korupciji
  
  #treci klaster je najheterogeniji, mnoge promenljive imaju standardnu devijaciju
  #koja je priblizno 50% srednje vrednosti
    #ove zemlje nisu toliko bliske jedne drugima ali ih generalno spajaju losi
    #uslovi za zivot i krsenje ljudskih prava
  