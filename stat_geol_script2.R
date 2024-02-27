#zaczynamy od instalacji i zaladowania potrzebnych paczek. Zaladowanie paczek jest konieczne po kazdym nowym uruchomieniu R natomiast instalacje przeprowadzamy tylko raz.
install.packages("modeest") #instalacja paczki
library("modeest") #zaladowanie paczki
install.packages("moments")
library("moments")
install.packages("Rmisc")
library("Rmisc")
install.packages("readxl")
library("readxl")

#rozklady z proby - sposob rozproszenia pomiarow wokol wartosci sredniej
#podzial ze wzgledu na modalnosc (liczbe najczestszych klas)
antymod <- runif(5000, min = 1, max = 10) #rozklad antymodalny, z liczba obserwacji podobna w kazdej z klas 
hist(antymod, probability = TRUE)

antymod2 <- rlnorm(5000, meanlog = 1, sdlog = 1) #rozklad antymodalny, z najliczniejszymi obsesrwacjami w klasie skrajnej (z definicji moda szeregu rozdzielczego to najliczniejsza klasa, z wyjatkiem klasy skrajnej)
hist(antymod2, probability = TRUE)
rug(antymod2)

wielomod <- c(rnorm(200, mean = 2), rnorm(200, mean = 8), rnorm(200, mean = 16)) #rozklad wielomodalny, z wyraznie zaznaczajacymi sie kilkoma najczestszymi klasami
hist(wielomod, probability = TRUE, breaks = 20)
lines(density(wielomod))

bimod <- c(rnorm(200, mean = 2), rnorm(200, mean = 8)) #rozklad bimodalny (dwumodalny), z dwoma klasami najczestszymi
hist(bimod, probability = TRUE)
lines(density(bimod))

jednomod <- rnorm(n = 200) #rozklad jednomodalny, z jedna najliczniejsza klasa. Taki rozklad jest najczesciej spotykany, przynajmniej jesli liczba obserwacji jest duza.
hist(jednomod, probability = TRUE)
lines(density(jednomod))
mlv(jednomod, method = "short") #wartosc mody, poza odczytaniem jej z histogramu, mozna uzyskac dzieki funkcji mlv

#podzial rozkladow jednomodalnych ze wzgledu na symetrycznosc (skosnosc), czyli ze wzgledu na to jak obserwacje rozkladaja sie wokol sredniej

jednomod.s <- rnorm(n = 200) #rozklad symetryczny, czyli mniej wiecej tyle samo klas po jednej i drugiej stronie sredniej
hist(jednomod.s, probability = TRUE)
lines(density(jednomod.s))
abline(v = mean(jednomod.s))
skewness(jednomod.s) #wartosc wskaznika skosnosci jest bliska 0

set.seed(12) #ustawienie parametru dla ciagu losowego
jednomod.as1 <- rbeta(n = 200, 5, 1.5) #rozklad asymetryczny, ujemnie skosny, czyli wartosci wolniej maleja na lewo od mody (rozklad ma ogon po lewej stronie).
hist(jednomod.as1, probability = TRUE)
abline(v = mean(jednomod.as1))
skewness(jednomod.as1) # wartosc wskaznika skosnosci przyjmuje wartosci ponizej 0

set.seed(12)
jednomod.as2 <- rbeta(n = 200, 1.5, 5) #ozklad dodatnio skosny, czyli wartosci wolniej maleja na prawo od mody (rozklad ma ogon po prawej stronie).
hist(jednomod.as2, probability = TRUE)
abline(v = mean(jednomod.as2))
skewness(jednomod.as2) #wartosc wskaznika skosnosci przyjmuje wartosci powyzej 0

#Miara Rozproszenie wynikow wokol sredniej, czyli odchylenie standardowe (odchylenie jest pierwiastkiem innej miary rozproszenia wynikow wokol sredniej, tj. wariancji)
male.sd <- rnorm(n = 10000, mean = 10, sd = 1) #generujemy probe statystyczna o niewielkim odchyleniu standardowym
hist(male.sd)
duze.sd <- rnorm(n = 10000, mean = 10, sd = 10) #generujemy probe statystyczna o duzym odchyleniu standardowym
hist(duze.sd)

sd(male.sd) #dla obliczania odchylenia standardowego stosujemy funkcje sd()
sd(duze.sd)
summary(male.sd)
summary(duze.sd)

#kurtoza - miara splaszczenia rozkladu
mezokurtyczny <- rnorm(5000)
hist(mezokurtyczny) #rozklad mezokurtyczny odznacza sie srednim splaszczeniem; moda wyraznie sie zaznacza, ale udzial wartosci mniejszych i wiekszych jest wciaz istotny
kurtosis(mezokurtyczny) #rozklad mezokurtyczny wg oryginalnego wzoru Pearsona przyjmuje wartosc ok. 3. Czesto spotyka sie modyfikacje tego wzoru, tak zeby rozklady mezokurtyczne mialy wartosc 0.

leptokurtyczny <- c(seq(1, 9, by = 1), rep(10, 30), seq(11, 20, by = 1))
hist(leptokurtyczny, probability = TRUE)
lines(density(leptokurtyczny))#rozklad leptokurtyczny, to taki gdzie istnieje wyraznie zaznaczajaca sie moda, a czestosc wystapien pozostalych wartosci jest niewielka
kurtosis(leptokurtyczny) #kurtoza przy rozkladzie leptokurtycznym osiaga wartosci powyzej 3

platykurtyczny <- c(seq(1, 9, by = 1), rep(10, 2), seq(11, 20, by = 1))
hist(platykurtyczny, breaks = 10, probability = TRUE)
lines(density(platykurtyczny))#rozklad platykurtyczny, to taki gdzie czestosc wystapien dominanty nie jest znaczaco wieksza od czestosci wystapien pozostalych obserwacji
kurtosis(platykurtyczny) #kurtoza przy rozkladzie platykurtycznym osiaga wartosci ponizej 3

#rozklad normalny - tj. rozklad jednomodalny, symetryczny, mezokurtyczny. Czesto spotykany w przyrodzie (czasami inny rozklad dla proby statystycznej wynika ze zbyt malej liczby obserwacji). Ma bardzo korzystne obliczeniowo wlasciwosci - mozemy na podstawie samej tylko wiedzy o sredniej i odchyleniu standardowym powiedziec o tym ile obserwacji miesci sie w jakim przedziale. Ponadto niektore testy statystyczne (tzw. testy parametryczne) wymagaja normalnosci rozkladu
roz.norm <- rnorm(5000, mean = 10, sd = 1)
hist(roz.norm, probability = TRUE)
rug(roz.norm)
lines(density(roz.norm))

abline(v = mean(roz.norm))
abline(v = c((mean(roz.norm) - sd(roz.norm)), (mean(roz.norm) + sd(roz.norm))), col = "red") #w przedziale 1sigma, tj. w zakresie od wartosci sredniej pomniejszona o wartosc odchylenia standardowego do wartosci sredniej powiekszonej o wartosc odchylenia standardowego znajduje sie ok. 68% obserwacji

abline(v = c((mean(roz.norm) - (2*(sd(roz.norm)))), (mean(roz.norm) + (2*sd(roz.norm)))), col = "green") #w przedziale 2sigma, tj. w zakresie od wartosci sredniej pomniejszona o dwukrotnosc wartosci odchylenia standardowego do wartosci sredniej powiekszonej o dwukrotnosc wartosci odchylenia standardowego znajduje sie ok. 95% obserwacji

abline(v = c((mean(roz.norm) - (3*(sd(roz.norm)))), (mean(roz.norm) + (3*sd(roz.norm)))), col = "pink") #w przedziale 3sigma, tj. w zakresie od wartosci sredniej pomniejszona o trzykrotnosc wartosci odchylenia standardowego do wartosci sredniej powiekszonej o trzykrotnosc wartosci odchylenia standardowego znajduje sie ok. 99.7% obserwacji

#Jak rozpoznac rozklad normalny?
#rozpoznanie wstepne
#1 moda ma zblizone wartosci do mediany i sredniej.

hist(roz.norm, probability = TRUE)
rug(roz.norm)
lines(density(roz.norm))
abline(v = median(roz.norm), col = "red") #wyrysowujemy mediane na czerwono
abline(v = mean(roz.norm), col = "green") #wyrysowujemy srednia 
abline(v = mlv(roz.norm, method = "short"), col = "black") #wyrysowujemy  mode na czarno
#2 rozklad jest symetryczny (tyle samo jest pomiarow po lewej i po prawej stronie) = skosnosc jest bliska 0 (skosnosc jest obliczana w rozny sposob; zazwyczaj przedzial wartosci miesci sie w zakresie od -1 do 1, z rozkladem symetrycznym korespondujacym do wartosci w okolicach 0)

skewness(roz.norm)

#3 kurtoza jest bliska 3 = rozklad posiada odpowiedni stopien splaszczenia (kurtoze tez obliczamy w rozny sposob; tutaj korzystamy z algorytmu, dla ktorego rozklad normalny koresponduje z liczba 3, ale czesto mozna sie spotkac z kurtoza o wartosci 0, ktora wskazuje na taki rozklad; konieczne jest dlatego, zeby wiedziec z jakich wzorow sie korzysta!)

kurtosis(roz.norm)

#graficzne sprawdzenie normalnosci poprzez wykreslenie wykresow Q-Q, czyli kwantyl-kwantyl

qqnorm(roz.norm)
qqline(roz.norm, col = "red") #przy rozkladzie normalnym obserwacje na calej dlugosci wykresu beda znajdowac sie w poblizu lini, ktora laczy 1 i 3 kwartyl danych "teoretycznych" o rozkladzie normalnym.

#czesto wykonywane sa takze testy na normalnosc rozkladu. Takich testow jest sporo. Jednym z czesciej wykorzystywanych i skutecznych jest test Shapiro-Wilka.

shapiro.test(roz.norm) #w tescie tym sprawdzana jest hipoteza, mowiaca ze analizowany rozklad jest rozkladem normalnym. Jesli wartosc p (p-value) jest wieksza niz 0.05, przyjmujemy ze hipoteza zostala zweryfikowana pozytywnie, czyli ze mamy faktycznie doczynienia z rozkladem normalnym (wartosc W jest mniej istotna). Test Shapiro-Wilka mozna sotsowac dla prob liczacych od 3 do 5000 obserwacji. Dla wiekszych prob mozna zastosowac test Kolmogorova-Smirnova (funkcja ks.test()). Uwaga: jesli hipoteza zostanie zweryfikowana negatywnie, tj. jesli p < 0.05, to nie mowimy, ze rozklad jest nienormalny, a raczej bimodalny, antymodalny, ujemnoskośny albo inny, w zaleznosci od tego, z jakim rozkladem mamy do czynienia 

#transformacja danych jako mozliwy sposob na otrzymanie rozkladu normalnego. Istnieja rozne sposoby transformacji danych. Jednym z czesciej stosowanych jest transformacja logarytmiczna.
roz.log <- rlnorm(30, meanlog = 1, sdlog = 1) #generujemy ciag liczb o rozkladzie log-normalnym, czyli takich, ktorych oryginalny rozklad co prawda odbiega od normalnego, ale kiedy uwzgledni sie logarytmy tych liczb, to otrzymujemy rozklad normalny
hist(roz.log, probability = TRUE) #histogram dla oryginalnych danych
lines(density(roz.log))
roz.log
hist(log(roz.log), probability = TRUE) #rozklad dla logarytmow oryginalnych danych
log(roz.log)
#przykladem danych dla ktorych zastosowanie transformacji logarytmicznej czesto pozwala na uzyskanie rozkladu normalnego sa dane z analizy uziarnienia. Czesto wyniki tej analizy przedstawia sie w skali phi, czyli skali logarytmicznej. Z kolei dla danych z wartosciami ujemnymi transformacja logarytmiczna nie jest mozliwa, bo nie da sie wyliczyc logarytmu z liczb ujemnych.
qqnorm(roz.log) #qqplot dla oryginalnych danych
qqline(roz.log)

qqnorm(log(roz.log)) 
qqline(log(roz.log)) #qqplot dla danych po transformacji logarytmicznej

shapiro.test(roz.log) #test Shapiro-Wilka
shapiro.test(log(roz.log))

#uwaga o miarach wartosci sredniej - moda, mediana, srednia arytmetyczna: najlatwiej jest w przypadku rozkladow normalnych, wtedy kazda z tych wartosci jest podobna i podanie kazdej z nich mowi nam duzo o populacji. W przypadku innego typu rozkladow warto sie zastanowic jaka miare wartosci sredniej najlepiej wybrac, zeby mowila ona cos istotnego o naszej probie statystycznej.

#jaki jest przedzial ufnosci dla wyliczonej z proby statystycznej wartosci sredniej (sensowne jedynie dla danych z rozkladem normalnym!)?
CI(roz.norm, ci = 0.95) #przedzial ufnosci dla sredniej pokazuje nam jaki jest zakres wartosci z proby statystycznej, ktory z zadanym prawdopodobienstwem (tutaj jest to 95% na co wskazuje liczba 0.95 przy parametrze ci) obejmuje faktyczna wartosc sredniej calej populacji. Ten zakres w wyniku uzyskanym przez wywolanie powyzszej komendy wskazuja wartosci lower i upper.

#podobnie jak nie mozemy miec pewnosci co do wartosci sredniej calej populacji wyznaczonej na podstawie proby statystycznej, tak istnieje rowniez niepewnosc co do liczby obserwacji mieszczacych sie w danym zakresie
pnorm(8, mean = mean(roz.norm), sd = sd(roz.norm), lower.tail = TRUE) #sprawdzenie jaki jest udzial obserwacji o wartosciach mniejszych niz 8 (zeby otrzymac wartosc procentowa nalezy przemnozyc wynik przez 100)

#######zadanie 1

#wczytanie danych
cw1_iryd <- read.csv("cw1_iryd.csv", header = FALSE) #ladujemy dane z pomiarow irydu w osadach analizowanych przez Alvareza
iryd <- cw1_iryd$V1 #przypisujemy zaladowane dane do tego samego typu, co te, ktore wczsniej analizowalismy.
cw1_wiek_intruzji <- read.csv("cw1_wiek_intruzji.csv", header = FALSE) 
wiek_intruzji <- cw1_wiek_intruzji$V1 #to samo co wyzej, tylko dla wieku intruzji granitowych
uran_w_lupkach <- read_excel("uran_w_lupkach.xls")
lupki.U1 <- uran_w_lupkach$U1
lupki.U2 <- uran_w_lupkach$U2[-c(7)]

#prosze okreslic: wartosci minimalne i maksymalne, srednia, mediane, odchlenie standardowe oraz kwartyle dla zestawow danych iryd, wiek_intruzji, lupk1.U1 i lupki.U2. Ktory z tych zestawow danych posiada rozklad normalny? Sprawdz to na podstawie analizy wizualnej histogramow, analizy kurtozy i skosnosci, wykresu Q-Q oraz testu Shapiro-Wilka. Czy te rozklady, ktore nie wykazuja rozkladu normalnego zmieniaja sie pod wplywem transformacji logarytmicznej? Odpowiedzi na pytania wpisz w kolejnych, odnoszacych sie do nich liniach kodu po znaku "#"
#funkcje przydatne do wykonania zadania: summary(), sd(), hist(), kurtosis(), skewness(), qqnorm(), qqline(), log(), shapiro.test()
#dokonaj interpretacji wynikow (nic nie piszemy, do przedyskutowania na kolejnych zajeciach)