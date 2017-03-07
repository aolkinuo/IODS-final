#Anna-Liina Olkinuora, anna-liina.olkinuora@helsinki.fi,
#5.3.2017. T?m? tiedosto on lopputyön aineistonmuokkausosion 
#sis?lt?? varten.

#Valitaan työhakemisto.
setwd("C:/Users/Anna-Liina/Documents/IODS-project")

#Luetaan aineistot.
math=read.table("data/student/student-mat.csv", sep = ";", header = TRUE)

por=read.table("data/student/student-por.csv", sep = ";", header = TRUE)

#Tutkitaan aineiston rakennetta ja ulottuvuuksia.
str(math)
dim(math)
str(por)
dim(por)

#math-aineisto sis?lt?? 395 havaintoyksikk?? ja 33 muuttujaa. 
#Muuttujista 17 ovat luokiteltuja muuttujia ja loput ovat
#numeerisia muuttujia. por-aineisto sis?lt?? 649 havaintoyksikk??
#ja 33 muuttujaa. Muuttujista 17 ovat luokiteltuja muuttujia ja 
#loput ovat numeerisia muuttujia.

#Yhdistetään aineistot.

library(dplyr)

join_by = c("school","sex","age","address","famsize","Pstatus",
"Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

math_por = inner_join(math, por, by = join_by, suffix=c(".math",
".por"))

#Tutkitaan aineiston rakennetta ja ulottuvuuksia.
str(math_por)
dim(math_por)

#Uudessa aineistossa on 382 havaintoyksikk?? ja 53 muuttujaa. 
#Muuttujista 24 ovat luokiteltuja muuttujia ja loput ovat numeerisia
#muuttujia.

#Tulostetaan sarakkeiden nimet.
colnames(math_por)

#Luodaan aineisto jossa on vain aiemmin yhdistetyt sarakkeet.
alc <- select(math_por, one_of(join_by))

#sarakkeet joita ei k?ytetty aineistojen yhdist?miseen
notjoined_columns = colnames(math)[!colnames(math) %in% join_by]

#sarakkeet joita ei k?ytetty aineistojen yhdist?miseen 
#tulostettu
notjoined_columns

#Jokaisen sarakkeen jota ei k?ytetty aineistojen yhdist?misess?
#nimelle
for(column_name in notjoined_columns) {
#valitaan math_por-aineistosta kaksi saraketta joilla on sama
#alkuper?inen nimi.
two_columns = select(math_por, starts_with(column_name))
#Valitaan sarakkeista ensimm?inen sarakevektori.
first_column = select(two_columns, 1)[[1]]
  
#Jos ensimm?inen sarake on numeerinen
if(is.numeric(first_column)) {
#otetaan jokaisesta kahden sarakkeen rivist? py?ristetty
#keskiarvo ja lis?t??n n?in saatu vektori alc-aineistoon.
alc[column_name] = round(rowMeans(two_columns))
} else { # else Jos ensimm?inen sarake ei ole numeerinen
#lis?t??n ensimm?inen sarakevektori alc-aineistoon.
  alc[column_name] = first_column
  }
}

#M??ritell??n uusi sarake yhdist?m?ll? viikonp?ivien ja
#viikonlopun alkoholink?ytt?.
alc = mutate(alc, alc_use = (Dalc + Walc) / 2)

#M??ritell??n uusi looginen sarake "high_use".
alc = mutate(alc, high_use = alc_use > 2)

#Katsotaan aineistoa. 
glimpse(alc)
head(alc)

#Aineistossa on 382 havaintoyksikk?? ja 35 muuttujaa.

#UUDET MUUTOKSET AINEISTOON

#Poistetaan muut muuttujat paitsi alc_use, absences, health, famrel,
#romantic, studytime, guardian, internet, age ja sex.
keep <- c("alc_use", "absences", "health", "romantic", 
          "studytime", "guardian", "internet", "age", "sex")

alc <- select(alc, one_of(keep))

#Nimetään muuttujat uudestaan suomenkielellä.
colnames(alc)

uudet_nimet_alc <- c("alk_käyttö","poissaolot","terveys",
"parisuhde","opisk_aika","huoltaja","netti","ikä", "sukupuoli")

colnames(alc) <-uudet_nimet_alc

colnames(alc)

#Tulostetaan alc-aineiston täydellisyysindikaattori.
complete.cases(alc)

#Tulostetaan aineisto jossa täydellisyysindikaattori on viimeinen
#sarake.
data.frame(alc[-1], comp = complete.cases(alc))

#Poistetaan rivit joissa on puuttuvia havaintoja.
alc <- filter(alc, complete.cases(alc)==TRUE)

#Poistetaan miehet aineistosta.
alc <- filter(alc, sukupuoli=="F")

# Poistetaan sukupuoli-muuttuja aineistosta.
alc <- select(alc, -sukupuoli)

#Liitetään aineistoon numeeriset versiot parisuhde-, huoltaja- ja 
#netti-muuttujista. Parisuhdemuuttujan arvo 1 tarkoittaa että 
#henkilöllä ei ole parisuhdetta. Huoltajamuuttujan arvo 1 tarkoittaa 
#että huoltaja on isä. Nettimuuttujan arvo 1 tarkoittaa että 
#henkilöllä ei ole nettia.
alc <- mutate(alc, as.numeric(parisuhde))

alc <- mutate(alc, as.numeric(huoltaja))

alc <- mutate(alc, as.numeric(netti))

#Katsotaan aineiston ensimmäisiä havaintoja.
head(alc)

#Poistetaan character-tyyppiset versiot parisuhde-, huoltaja- ja
#nettimuuttujista
alc <- select(alc, -parisuhde)

alc <- select(alc, -huoltaja)

alc <- select(alc, -netti)

#Katsotaan aineiston ensimmäisiä havaintoja.
head(alc)

#Muutetaan numeeristen versioiden parisuhde-, huoltaja- ja
#netti-muuttujista nimiksi parisuhde, huoltaja ja netti.

uudet_nimet_alc <- c("alk_käyttö","poissaolot","terveys",
                   "opisk_aika","ikä", "parisuhde","huoltaja",
                   "netti")

colnames(alc) <-uudet_nimet_alc

head(alc)

#Vaihdetaan työhakemisto.
setwd("C:/Users/Anna-Liina/Documents/IODS-final")

#Tallennetaan aineisto kurssin kansion data-kansioon.
write.table(alc, file="data/alc.txt", sep=",")
