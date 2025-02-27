setwd("/Users/andreeaburca/Desktop/sem 2 cibe/micro manageriala/proiect")
dir()

#import date
date_actiuni<-read.csv("proiect.csv", header = TRUE, sep = ";")
date_actiuni1 <- read.table("proiect.txt", header= TRUE, sep= "\t")

#import txt
date_rent<-read.csv("proiectAB.csv", header = TRUE, sep = ",")
date_rent1 <- read.table("proiectAB.txt", header= TRUE, sep= "\t")


library(moments)


#PREȚ INTEL

#prețul de închidere mediu pentru anul 2023 al companiei INTEL este de 34.40 dolari
#acțiunea înregistrând un preț minim de 24.56 dolari și cu un preț maxim de 50.61 de dolari (cu 16,21 dolari mai mult față de medie)
#prima cuartilă ce se înregistrează la 31.26 dolari (arată că 25% dintre prețuri au fost sub această valoare indicând o distribuție inferioară a prețurilor); 
#cea de-a treia cuartilă are prețul de 41.47 de dolari și indică faptul că 75% dintre prețurile de închidere au fost sub această valoare (reflectând o distribuție superioară a prețurilor)

summary(date_actiuni$Pret_INTEL)

#abaterea standard este de aproximativ 6.43 dolari
#indicând o dispersie relativ mică în jurul mediei de 35.81 dolari 

sd(date_actiuni$Pret_INTEL)

#coeficientul de variație este de aproximativ 17.94%
#sugerând o variabilitate relativ mică în comparație cu media, indicând o relativă omogenitate a datelor 

cv<-sd(date_actiuni$Pret_INTEL)/mean(date_actiuni$Pret_INTEL)
cv

#coeficientului de asimetrie (0.509), datele prezintă o ușoară asimetrie spre dreapta

skewness(date_actiuni$Pret_INTEL)

#coeficientul de aplatizare (2.348) indică o distribuție platicurtică 

kurtosis(date_actiuni$Pret_INTEL)

#histograma sugerează o densitate mai mare a prețurilor mai mici, confirmând asimetria spre dreapta

hist(date_actiuni$Pret_INTEL, col="cyan", main="Histograma Pret_INTEL",ylab="Frecventa", xlab="Pretul actiunii INTEL")

#în boxplot nu există outlieri

boxplot(date_actiuni$Pret_INTEL, col="cyan3", main="Pret_INTEL Boxplot")



#PREȚ AMD

#25% dintre prețuri se încadrează în intervalul [76.61; 97.86] dolari (indicând o distribuție inferioară a prețurilor)
#50% dintre date sunt sub 110.09 dolari
#iar 75% dintre valori sunt mai mici decât 121.77 dolari (reflectând o distribuție superioară a prețurilor)

summary(date_actiuni$Pret_AMD)

#abaterea standard este de aproximativ 24.44 dolari
#indicând o dispersie mai mare în jurul mediei de 114.88 dolari

sd(date_actiuni$Pret_AMD)

#coeficientul de variație este de aproximativ 21.27%
#sugerând o variație relativ mare în comparație cu media, indicând o relativă heterogenitate a datelor

cv<-sd(date_actiuni$Pret_AMD)/mean(date_actiuni$Pret_AMD)
cv

#coeficientului de asimetrie este de 1.067
# astfel datele prezintă o asimetrie semnificativă spre dreapta

skewness(date_actiuni$Pret_AMD)

#coeficientul de aplatizare (3.632) indică o distribuție leptokurtică
#cu o concentrație mai mare a datelor în jurul mediei și cozi mai lungi

kurtosis(date_actiuni$Pret_AMD)

#histograma indică o densitate mai mare a prețurilor mai mici până la aproximativ 115 dolari
#după care densitatea scade abrupt, sugerând o coadă lungă spre dreapta și o asimetrie semnificativă

hist(date_actiuni$Pret_AMD, col="violet",main="Histograma Pret_AMD",ylab="Frecventa", xlab="Pretul actiunii AMD")

#există outlieri în partea superioară a distribuției sugerând prețuri extreme ale acțiunilor

boxplot(date_actiuni$Pret_AMD, col="pink2", main="Pret_AMD Boxplot")



#PREȚ INDICE

#25% dintre valori se situează în intervalul [11139; 12578] dolari (indicând o distribuție inferioară a prețurilor)
#50% sunt sub 13592 dolari
#iar 75% sunt mai mici decât 14235 dolari (reflectând o distribuție superioară a prețurilor)

summary(date_actiuni$Pret_Indice)

#abaterea standard este de aproximativ 1174.092, indicând o dispersie semnificativă în jurul mediei de 13500

sd(date_actiuni$Pret_Indice)

#coeficientul de variație este de aproximativ 8.70%, sugerând o variație relativ mică în comparație cu media și indică o relativă omogenitate a datelor

cv<-sd(date_actiuni$Pret_Indice)/mean(date_actiuni$Pret_Indice)
cv

#coeficientului de asimetrie este de -0.0098 și prezintă o asimetrie aproape simetrică

skewness(date_actiuni$Pret_Indice)

#coeficientul de aplatizare (2.32) indică o distribuție mesocurtică, adică una apropiată de o distribuție normală

kurtosis(date_actiuni$Pret_Indice)

#histograma prezintă o formă în formă de clopot 
#indicând o distribuție simetrică, cu densitatea relativ egală atât în partea stângă, cât și în cea dreaptă a graficului

hist(date_actiuni$Pret_Indice, col="purple",main="Histograma Pret_Indice",ylab="Frecventa", xlab="Pretul indicelui NASDAQ")

#nu există outlieri în boxplot

boxplot(date_actiuni$Pret_Indice, col="purple3", main="Pret_Indice Boxplot")




#RENTABILITATE INTEL


#mediana este de 0.003, iar media este de 0.002498; acest lucru indică că distribuția datelor este aproximativ simetrică în jurul acestor valori centrale
#rezultând că majoritatea datelor sunt apropiate de aceste valori
#valorile cuartilelor sugerează că majoritatea datelor se încadrează între -0.012 și 0.016, cu excepția unor valori extreme

summary(date_rent$Rent_INTEL)

#abaterea standard este de 0.0242, indicând o dispersie relativ mică a datelor în jurul mediei

sd(date_rent$Rent_INTEL)

#coeficientul de variație este de 9.719915, sugerând o variabilitate relativ mare în comparație cu media

cv<-sd(date_rent$Rent_INTEL)/mean(date_rent$Rent_INTEL)
cv

#datele nu prezintă o asimetrie semnificativă având o ușoară asimetrie negativă (-0.141)
#indicând că coada distribuției este mai lungă în partea stângă

skewness(date_rent$Rent_INTEL)

#kurtosis-ul este de 5.873345, indicând o distribuție mai concentrată în jurul mediei și cu cozi mai grele decât distribuția normală (platikurtică)

kurtosis(date_rent$Rent_INTEL)

#histograma are forma unui con, cu vârful situat peste 80 pentru frecvență, indicând o densitate mai mare a datelor în această zonă

hist(date_rent$Rent_INTEL, col="chocolate1", main="Histograma Rentabilitate INTEL",ylab="Frecventa", xlab="Rentabilitatate INTEL")

#observăm că există valori de tip outlier în ambele extremități, sugerând prezența unor valori extreme în date

boxplot(date_rent$Rent_INTEL, col="chocolate3", main="Rentabilate INTEL Boxplot")




#RENTABILITATE AMD


#valoarea minimă e de -0.092 indică limita inferioară a distribuției
#prima cuartilă e de -0.013500, aceasta reprezintă valoarea la care 25% din date sunt mai mici și 75% sunt mai mari
#mediana este foarte aproape de zero (0.001), indicând că jumătate dintre date sunt sub această valoare și jumătate sunt peste
#media este puțin pozitivă, indicând o tendință către valorile mai mari (0.003538)
#a treia cuartilă e de 0.020000, acesta este punctul la care 75% dintre date sunt mai mici și 25% sunt mai mari
#valoarea maximă e de 0.112 indică limita superioară a distribuției

summary(date_rent$Rent_AMD)

#abaterea standard este de aproximativ 0.0287
#iar coeficientul de variație este de aproximativ 8.11%, indicând o variație relativ mare în comparație cu media
#sugerând o relativă heterogenitate a datelor

sd(date_rent$Rent_AMD)
cv<-sd(date_rent$Rent_AMD)/mean(date_rent$Rent_AMD)
cv

#coeficientul de asimetrie este de aproximativ 0.357, indicând o ușoară asimetrie spre dreapta în distribuția datelor

skewness(date_rent$Rent_AMD)

#coeficientul de aplatizare este de aproximativ 4.573, indicând o distribuție leptocurtică, cu o concentrare extrem de mare a datelor în jurul mediei și cozi lungi

kurtosis(date_rent$Rent_AMD)

#histograma indică o distribuție relativ echilibrată a datelor,întrucât indică o creștere inițiala mai mare spre dreapta, apoi spre stânga

hist(date_rent$Rent_AMD, col="palegreen1", main="Histograma Rentabilitate AMD",ylab="Frecventa", xlab="Rentabilitatate AMD")

#există 2 valori de tip outlier în partea inferioară și mai multe în partea superioară a distribuției, sugerând prezența unor valori extreme în date

boxplot(date_rent$Rent_AMD, col="palegreen4", main="Rentabilate AMD Boxplot")




#RENTABILITATE INDICE


#valoarea minimă (-0.024) reprezintă limita inferioară a distribuției
#prima cuartilă (-0.005) este punctul la care 25% dintre date sunt mai mici și 75% sunt mai mari
#mediana (0.001) este aproape de zero, sugerând că jumătate dintre date sunt sub această valoare și jumătate sunt peste
#media (0.001271) este ușor pozitivă, indicând o tendință către valorile mai mari
#a treia cuartilă (0.008) este punctul la care 75% dintre date sunt mai mici și 25% sunt mai mari
#valoarea maximă (0.025) reprezintă limita superioară a distribuției

summary(date_rent$Rent_Indice)

#abaterea standard este de aproximativ 0.0102, iar coeficientul de variație este de aproximativ 8.00%
#sugerând o variație relativ mare în comparație cu media și indică o relativă heterogenitate a datelor

sd(date_rent$Rent_Indice)
cv<-sd(date_rent$Rent_Indice)/mean(date_rent$Rent_Indice)
cv

#coeficientul de asimetrie este de aproximativ -0.089, indicând o ușoară asimetrie negativă spre stânga în distribuția datelor

skewness(date_rent$Rent_Indice)

#coeficientul de aplatizare este de aproximativ 2.737, indicând o distribuție mesocurtică, apropiată de o distribuție normală
#cu o concentrație moderată a datelor în jurul mediei și cozi mai scurte

kurtosis(date_rent$Rent_Indice)

#histograma indică o distribuție echilibrată a datelor, fără a exista un dezechilibru semnificativ între densitățile din diferitele intervale

hist(date_rent$Rent_Indice, col="pink", main="Histograma Rentabilitate Indice",ylab="Frecventa", xlab="Rentabilitatate NASDAQ")

#nu există valori de tip outlier, ceea ce indică absența unor valori extreme în setul de date

boxplot(date_rent$Rent_Indice, col="pink3", main="Rentabilate NASDAQ Boxplot")




#CORELAȚIE


#împreună
#am luat prețurile acțiunilor cu prețul indicelui de piață, dar și rentabilitățile acțiunilor cu rentabilitatea indicelui de piață
#din matricea de corelație se remarcă o corelație pozitivă, însă valorile care nu se vad atat de mult pe grafic indică faptul că acestea nu sunt corelate puternic
#astfel,am ajuns la concluzia că trebuie luate separat pentru interpretare după cum urmează

c<-data.frame(INTEL_pret=date_actiuni$Pret_INTEL, AMD_pret=date_actiuni$Pret_AMD,  Indice_pret=date_actiuni$Pret_Indice, INTEL_rent= date_rent$Rent_INTEL, AMD_rent=date_rent$Rent_AMD, Indice_rent=date_rent$Rent_Indice )
corelatievol <- cor(c)
corelatievol
library(corrplot)
corrplot(corelatievol, type = "upper", method = "number")




#CORELAȚIE


#separat
#coeficientul de corelație de 0.81 (INTEL și AMD) indică o corelație pozitivă puternică
#acest lucru sugerează că atunci când prețul acțiunilor INTEL crește, prețul acțiunilor AMD tinde să crească și el, și invers
#coeficientul de 0.87 (INTEL și NASDAQ) este și mai apropiat de 1, ceea ce indică o corelație și mai puternică între prețul acțiunilor INTEL și indicele NASDAQ
#coeficientul de 0.90 (AMD și NASDAQ) este cel mai apropiat de 1 dintre toate, sugerând că prețul acțiunilor AMD este foarte strâns legat de indicele NASDAQ
#toate corelațiile sunt pozitive, ceea ce evidențiază că mișcările de preț ale acestor acțiuni și indice tind să meargă în aceeași direcție
#valorile de 1.00 pe diagonala principală sunt normale, deoarece reprezintă corelația unei variabile cu ea însăși, care este întotdeauna perfectă

cP<-data.frame(INTEL=date_actiuni$Pret_INTEL,AMD= date_actiuni$Pret_AMD,  NASDAQ=date_actiuni$Pret_Indice )
corelatievolP <- cor(cP)
corelatievolP
library(corrplot)
corrplot(corelatievolP, type = "upper", method = "number")


#INTEL și AMD au un coeficient de corelație de 0.38, indicând o corelație pozitivă moderată
#diagonala principală a matricei arată valori de 1.00, ceea ce indică o corelație perfectă pozitivă pentru fiecare variabilă cu ea însăși
#în timp ce toate cele trei rentabilități sunt corelate pozitiv, NASDAQ are o corelație mai puternică cu AMD (0.63) decât cu INTEL (0.47)

cR<-data.frame(INTEL=date_rent$Rent_INTEL, AMD=date_rent$Rent_AMD,  NASDAQ=date_rent$Rent_Indice)
corelatievolR <- cor(cR)
corelatievolR
library(corrplot)
corrplot(corelatievolR, type = "upper", method = "number")




#Rentabilitatea acțiunii împreună cu rentabilitatea indicelui de piață


#Se poate observa că asupra rentabilității acțiunii INTEL, trendul este ușor crescător, dar cu oscilații micuțe pe perioada înregistrată
#se poate observa că asupra rentabilitatii indicelui de piață, trendul este constant în majoritatea timpului
#rentabilitățile celor două se mențin relativ constante, de la începutul perioadei până la sfârșit
#însă constatăm la INTEL ca a avut o scădere bruscă pe finalul perioadei.
#există perioade când rentabilijtatea acțiunii INTEL crește și scade brusc spre final
#iar rentabilitatea indicelui de piață este stabilă, dar cu câteva scăderi bruște

par(mfrow=c(2,1))
plot.ts(date_rent$Rent_INTEL, col="violetred", main="Rentabilitatea actiunii INTEL", ylab="")
plot.ts(date_rent$Rent_Indice, col="hotpink", main="Rentabilitate Indice de piata", ylab="")




#Prețul acțiunii împreună cu prețul indicelui de piață


#din graficul prețurilor acțiunii INTEL, rezultă un trend crescător, dar cu variații pe anumite intervale de date
#pe perioada selectată, observăm un trend crescător al indicelui de piață NASDAQ, o tendință relativ stabilă
#cu fluctuații ușoare, ceea ce indica stabilitatea pietei
#din cele două grafice putem observa că cei doi indici ai seriilor de date sunt relativ asemănători, au cam aceeași tendință, adică crescătoare
#iar pe parcursul acestui an, au avut aceeași evoluție

par(mfrow=c(2,1))
plot.ts(date_actiuni$Pret_INTEL, col="tan1", main="Pretul actiunii INTEL", ylab="")
plot.ts(date_actiuni$Pret_Indice, col="sienna1", main="Pret indice NASDAQ", ylab="")




#Evoluția prețurilor acțiunilor împreună cu prețul indicelui de piață



#evolutia indicelui NASDAQ este crescătoare pe parcursul anului, existând doar un moment de scădere bruscă
#în ceea ce privește INTEL si AMD evolutia acestora prezintă un trend crescător
#începutul anului a venit cu un trend descendent pentru INTEL și AMD din cauza interesului scăzut prezentat de public pentru achiziționarea de calculatoare
#implicit de procesoare; încă din primul sfert al perioadei s-a observat o scădere, însă cu o revenire impresionantă, atât pentru INTEL, cât și pentru AMD
#altfel spus, in data de 03.03.2023 (26.03590) s-a anunțat că INTEL nu este încă pregătită să investească în construirea unei fabrici de semiconductori în India
#deși țara a luat măsuri pentru a atrage investiții în acest domeniu
#pentru AMD pe 03.05.2023 (81.62) s-a aratat o scădere semnificativă a veniturilor și o pierdere netă, din cauza declinului vânzărilor în segmentul clientului
#apogeul preturilor a fost in 27.12.2023(50.61165) cand investiția semnificativă a INTEL a dus la extinderea fabricii de cipuri din Israel
#în ciuda conflictului din zonă, acesta fiind evenimentul care a adus la creșterea prețului acțiunii
#pe 25.01.2024(180.33) pentru AMD evenimentul a fost reprezentat de creșterea cererii globale pentru inteligența artificială (AI)
#determinată de nevoia companiilor de a utiliza cipuri AI pentru îmbunătățirea transformărilor digitale în afaceri

par(mfrow=c(3,1))
plot.ts(date_actiuni$Pret_INTEL , main="Evolutia preturilor actiunilor INTEL", col="orange",type="l",xlab="Zile", ylab="Preturi")
plot.ts(date_actiuni$Pret_AMD , main="Evolutia preturilor actiunilor AMD", col="violet",type="l",xlab="Zile", ylab="Preturi")
plot.ts(date_actiuni$Pret_Indice , main="Evolutia preturilor actiunilor NASDAQ", col="lightblue",type="l",xlab="Zile", ylab="Preturi")




#Evoluția rentabilității acțiunilor împreună cu rentabilitatea indicelui de piață


#trendurile rentabilităților urmează un trend oscilant, existând atât creşteri ale rentabilităților, cât şi descreşteri
#acestea depind de perioada anului, de exemplu în apropierea Crăciunului mereu există creșteri dar și în luna noiembrie când se organizează Black Friday

par(mfrow=c(3,1))
plot.ts(date_rent$Rent_INTEL , main="Evolutia Rentabilitatilor INTEL", col="purple",type="l",xlab="Zile", ylab="Preturi")
plot.ts(date_rent$Rent_AMD, main="Evolutia Rentabilitatilor AMD", col="tomato",type="l",xlab="Zile", ylab="Preturi")
plot.ts(date_rent$Rent_Indice , main="Evolutia Rentabilitatilor NASDAQ", col="peachpuff1",type="l",xlab="Zile", ylab="Preturi")

#tabele pentru statistici ordonate
# Pentru prețuri
prețuri <- data.frame(
  INTEL = c(sd(date_actiuni$Pret_INTEL), sd(date_actiuni$Pret_INTEL)/mean(date_actiuni$Pret_INTEL), skewness(date_actiuni$Pret_INTEL), kurtosis(date_actiuni$Pret_INTEL)),
  AMD = c(sd(date_actiuni$Pret_AMD), sd(date_actiuni$Pret_AMD)/mean(date_actiuni$Pret_AMD), skewness(date_actiuni$Pret_AMD), kurtosis(date_actiuni$Pret_AMD)),
  NASDAQ = c(sd(date_actiuni$Pret_Indice), sd(date_actiuni$Pret_Indice)/mean(date_actiuni$Pret_Indice), skewness(date_actiuni$Pret_Indice), kurtosis(date_actiuni$Pret_Indice))
)

rownames(prețuri) <- c("Abaterea standard (sd)", "Coeficientul de variație (cv)", "Asimetrie (skewness)", "Aplatizare (kurtosis)")

# Pentru rentabilități
rentabilități <- data.frame(
  INTEL = c(sd(date_rent$Rent_INTEL), sd(date_rent$Rent_INTEL)/mean(date_rent$Rent_INTEL), skewness(date_rent$Rent_INTEL), kurtosis(date_rent$Rent_INTEL)),
  AMD = c(sd(date_rent$Rent_AMD), sd(date_rent$Rent_AMD)/mean(date_rent$Rent_AMD), skewness(date_rent$Rent_AMD), kurtosis(date_rent$Rent_AMD)),
  NASDAQ = c(sd(date_rent$Rent_Indice), sd(date_rent$Rent_Indice)/mean(date_rent$Rent_Indice), skewness(date_rent$Rent_Indice), kurtosis(date_rent$Rent_Indice))
)

rownames(rentabilități) <- c("Abaterea standard (sd)", "Coeficientul de variație (cv)", "Asimetrie (skewness)", "Aplatizare (kurtosis)")

# Afisare tabele
library(knitr)
library(kableExtra)
kable(prețuri, caption = "Statistici pentru prețuri") %>%
  kable_styling(full_width = FALSE) %>%
  column_spec(1, bold = TRUE) %>%
  collapse_rows(columns = 1) %>%
  add_header_above(c(" " = 1, "Prețuri" = 3)) %>%
  footnote(general = NULL)

kable(rentabilități, caption = "Statistici pentru rentabilități") %>%
  kable_styling(full_width = FALSE) %>%
  column_spec(1, bold = TRUE) %>%
  collapse_rows(columns = 1) %>%
  add_header_above(c(" " = 1, "Rentabilități" = 3)) %>%
  footnote(general = NULL)

