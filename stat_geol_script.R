library(tidyverse)
#R jako kalkulator
1 + 1 #nacisnij ctr + enter, zeby wyswietlic wynik
4 * 15
2^2
pi + 1
(1 + 1) * (3 + 8) * 11


#tworzenie obiektow
x <- 1 + 1 # znak "<-" przypisuje wyrazenie po jego prawej stronie do obiektu, ktorego nazwa poprzedza znak. Sam obiekt pojawia sie w panelu "Environment", w gornym prawym oknie
y <- 3 + 8
z <- 11
x * y * z

#wykorzystanie funkcji
log(y) #nazwa funkcji znajduje sie przed nawiasem, w ktory wstawiamy obieky/liczbe. Ten przyklad pokazuje wyliczenie logarytmu naturalnego dla obiektu y
sqrt(2) #pierwiastek liczby 2
log(sqrt(2)) #logarytm z pierwiastka liczby 2

#pomoc dotyczaca funkcji mozna uzyskac wpisujac nazwe funkcji poprzedzona znakiem "?". Pomoc wyswietla sie w prawym dolnym panelu
?log #pomoc dla funkcji log

###podstawowe definicje
#Populacja - zbiór elementów, podlegających badaniu statystycznemu
#próba statystyczna - zbiór obserwacji wylosowanych z populacji
#Celem statystyki jest pozyskanie jak największej wiedzy o populacji na podstawie analizy próby statystycznej 

pstat <- sample(1:20, 20, replace = TRUE) #tworzymy ciag losowych liczb calkowitych z przedzialu 1 do 20

pstat #wyswietlamy obiekt pstat -> znajdz najwieksza i najmniejsza wartosc (tj. wartosci skrajne)

pstat.sort <- sort(pstat) #uporzadkowanie danych wg wartosci zwieksza ich czytelnosc

table(pstat)

plot(pstat.sort) #wyswietlamy wykres, na ktorym kazdej obserwacji odpowiada jeden punkt. Wizualizacja danych pozwala na znacznie bardziej efektywny sposob ich eksploracji

#wykorzystanie szeregow rozdzielczych
#szereg rozdzielczy - w uproszczeniu licznosci/prawdopodobienstwa wystapien pomiarow w klasach (przedzialach) o jednakowej rozpietosci
hist(pstat, breaks = 10) #wyrysowanie histogramu
rug(pstat) #wyswietlenie konkretnych wartosci obserwacji z obiektu pstat
#histogram - wykres słupkowy przedstawiający w graficzny sposób szereg rozdzielczy; słupki obrazują liczność kolejnych klas; na osi Y znajdują się więc liczności (częstości) klas, a na osi X - szereg rozdzielczy (jego kolejne klasy)


#to jak wyglada histogram zalezy od tego ile klas wydzielimy -> zmien wartosci parametru "breaks" (w przypadku obiektu pstat mozliwe jest wybranie 1, 2, 4, 10 lub 20 klas) i sprawdz, jak wplywa to na wyglad histogramu.

#Zadanie: jedna chetna osoba wyrysowuje histogram na tablicy z danych podanych przez inną chętną osobę

#granice miedzy klasami mozna takze wyznaczyc wpisujac je recznie do parametru breaks

hist(pstat, breaks = c(0, 4, 8, 12, 16, 20)) #aby podac ciag liczb wpisujemy je do nawiasu poprzedzonego litera c. Bardzo przydatna funkcja jest seq(), ktore generuje sekwencje liczb w danym zakresie i ustalonym odstepie miedzy nimi.

#mozna dokonac podzialu szeregu na klasy recznie i przedstawic to w tabeli w tym celu:
zakresy <- seq(0, 20, by = 2) #generujemy ciag liczb o stalych odstepach miedzy wartosciami, ktory odpowiada granica miedzy klasami
zakresy.ciecie <- cut(pstat, zakresy, right = TRUE) #przypisujemy wartosci w naszym zestawie danych do ustalonych zakresow; opcja right = TRUE zadaje przydzielenie obserwacji o wartosci rownej wartosci ustalonych granic miedzy przedzialami zawsze do klasy nizszej
czestosciwprzedzialach <- table(zakresy.ciecie)
hist(pstat, breaks = 10)
rug(pstat)

#alternatywnie mozna wykreslic histogram pokazujacy prawdopodobienstwo wystapienia wartosci z danego przedzialu. Wartosci na osi y znajduja sie w zakresie 0 do 1, gdzie klasy, dla ktorych wartosc wynosi 0 maja prawdopodobienstwa wystapienia 0%, a klasy o wartosci 1 maja prawdopodobienstwo wystapienia 100%
hist(pstat, breaks = 10, probability = TRUE)
#szeregi rozdzielcze mozna tez wyswietlic za pomoca lini. 
lines(density(pstat, bw = 2)) #linie przedstawiaja "wygladzone" dane. Ich wartosci na osi y sa jedynie przyblizeniem wynikajacym z przyjetego algorytmu.

#prawdopodobiensto dla obserwaji przy rozkladzie dwumianowym
dbinom(1, 3, 0.5) #oblicza prawdopodobienstwo jednego wystapienia jakiegos zjawiska przy trzech probach, w momencie kiedy prawdopodobienstwo wystapienia tego zjawiska przy pojedynczym zdarzeniu wynosi 50%

histogram_przygotowanie <- tibble(prawdopodobienstwo = c(dbinom(0:3, 3, 0.5)),
                                  liczba_sukcesow = c(0, 1, 2, 3))

ggplot(histogram_przygotowanie, aes(x = liczba_sukcesow, y = prawdopodobienstwo)) +
  geom_col()

#Zadanie: Odbywa się test z geozagrożeń, w którym znajduje się 10 pytań. W każdym pytaniu są cztery możliwe odpowiedzi, z których tylko jedna jest poprawna. Jakie jest prawdopodobieństwo prawidłowej odpowiedzi na przynajmniej połowę pytań przy założeniu losowego ich wyboru? Wynik oblicz w R, a potem wykonaj obliczenia manualnie.

#W sytuacji, w której mamy dwa możliwe wyniki i dla obu p wynosi 50% a n dąży do nieskończoności wykres zaczyna nabierać następuący kształt:

x <- seq(-5, 5, length=100)

density_values <- tibble(val = dnorm(x, mean=0, sd=1), n = 1:length(x))

ggplot(density_values, aes(x = n, y = val)) +
  geom_line() +
  labs(x = NULL, y = "Probability") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

#podstawowe statystyki opisowe

#kwantyle, percentyle i kwartyle
quantile(pstat) #percentyl jest wielkością procentowa, poniżej której padają wartości zadanego procentu próbek. Wyrazone w zapisie dziesietnym percentyle, to inaczej kwantyle (np. percentyl 25 to inaczej kwantyl 0.25). Percentyl 25, 50 i 75 to tzw. kwartyle, kolejno pierwszy, drugi (mediana) i trzeci. -> wieświetl ciag pstat.sort i znajdz liczby w szeregu wskazane przez kolejne percentyle

pstat_cum <- tibble(val = pstat)

ggplot(pstat_cum, aes(x = val)) +
  stat_ecdf() +
  stat_ecdf(geom = "point") +
  labs(title = "Wykres kumulacyjny dla pstat",
       x = "Wartość", y = "Prawdopodobieństwo kumulacyjne") +
  theme_minimal() +
  coord_equal(ratio = 20)

#Zadanie: korzystająć z wzoru oblicz manualnie wartość percentyla dla obserwacji o randze 1. Następnie oblicz, jaką rangę ma obserwacja dla 75 percentyla. 

summary(pstat)

#wkresy pudełgkowe - boxploty
#gruba kreska w srodku to mediana, granice ramki pudelka wyznaczaja kolejno pierwszy i trzeci kwartyl, a ekstrema oblicza sie wg wzoru kwartyl1 - 1.5*rozstep kwartylny (ekstremum dolne) i kwartyl3 + 1.5*rozstep kwartylny (ekstremum gorne)

ggplot(pstat_cum, aes(y = val)) +
  geom_boxplot()

ggplot(tibble(val = rnorm(1000, 5, 2)), aes(y = val)) +
  geom_boxplot()


#srednia
mean(pstat) #funkcja sluzaca do wyliczania sredniej

#odchylenie standardowe i wariancja
var(pstat)
sd(pstat)
sqrt(var(pstat))

#przykładowe dane geochemiczne z replikowanych pomiarow dla 3 pierwiastkow. Dane w postaci ramki danych:

geochem <- tibble(Cr = c(205, 255, 195, 220, 235),
                  Ni = c(130, 165, 100, 135, 145),
                  V = c(180, 215, 135, 200, 205))

#Korzystając z R oblicz "na piechotę" (tj. bez korzystania z funkcji var i sd) wariancję i odchlenie standardowe dla swojego Cr i Ni z obiektu geochem. Sprawdź wynik wykorzystując funkcje dedykowane do obliczania tych parametrów


#skośność - rozkłady symetryczne, dodatnioskośne i ujemnoskośne
library(moments)

hist(geochem$Cr)
abline(v = mean(geochem$Cr), col = "red")
abline(v = median(geochem$Cr), col = "green")

skewness(geochem$Cr)

hist(geochem$V)
abline(v = mean(geochem$V), col = "red")
abline(v = median(geochem$V), col = "green")

skewness(geochem$V)

#Zadanie oblicz "na piechotę" skośność dla Ni z obiektu geochem. Wynik sprawdź korzystając z funkcji skewness(). Jak sklasyfikujesz badany rozkład pod względem skośności (tj. jaki to typ skośności)?

#kurtoza - rozkłady mezokurtyczne, leptokurtyczne i platykurtyczne

#zadanie - oblicz "na piechotę kurtozę dla Cr z obiektu geochem. Wynik sprawdź korzystając z funkcji kurtosis(). Jak sklasyfikujesz badany rozkład pod względem kurtozy (tj. jaki to typ kurtozy)?


normal_density <- function(x) {
  dnorm(x, mean = 0, sd = 1)
}

#wykres dla rozk;adu normalnego z zaznaczonymi kolejnymi wartościami dla odchlenia standardowego
ggplot(data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = normal_density, geom = "area", fill = "lightblue", alpha = 0.5, xlim = c(-3, 3)) +
  stat_function(fun = normal_density, geom = "line", size = 1) +
  geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "red", size = 1.2) +
  geom_vline(xintercept = c(-2, 2), linetype = "dashed", color = "orange", size = 1.2) +
  geom_vline(xintercept = c(-3, 3), linetype = "dashed", color = "darkgreen", size = 1.2) +
  geom_vline(xintercept = 0) +
  labs(title = "Rozkład normalny z zaznaczonymi zakresami 1 i 2 sigma",
       x = "X-axis",
       y = "Density") +
  theme_minimal() #w przedziale 1sigma, tj. w zakresie od wartosci sredniej pomniejszona o wartosc odchylenia standardowego do wartosci sredniej powiekszonej o wartosc odchylenia standardowego znajduje sie ok. 68% obserwacji; #w przedziale 2sigma, tj. w zakresie od wartosci sredniej pomniejszona o dwukrotnosc wartosci odchylenia standardowego do wartosci sredniej powiekszonej o dwukrotnosc wartosci odchylenia standardowego znajduje sie ok. 95% obserwacji

#centralne twierdzenie graniczne. Założenia:
# próbkowanie jest losowe
# próby muszą być niezależne od siebie
# populacja musi miec skonczoną wariancję
# 
# im wieksza licza obserwacji w probach, tym lepiej. Przyjmuje sie, ze kazda proba powinna miec przynajmniej 30 obserwacji. Zdarza się, ze to wcale nie starcza

#Dzieki centralnemu twierdzeniu granicznemu możem przeprowadzać testy statystyczne dla populacji, ktore wcale nie maja rozkladu normalnego

composita <- tibble(dl = c(18.4, 16.9, 13.6, 11.4, 7.8, 6.3),
                    szer = c(15.4, 15.1, 10.9, 9.7, 7.4, 5.3))

#załóżmy, że znamy dokładnie własności populacji Composita, że średnia długość wynosi 14.2 mm a odchylenie standardowe 4.7 mm. Znalezlismy próbkę ze skamieniałościami ramienionogów, które wygladają jak Composita, ale są wyjatkowo duże - sredio maja po 20 mm. Czy moga one pochodzic ze znanej populacji Composita

#wykorzystanie statystyki z posiadajaca rozklad normalny, srednią zero i odchylenie standardowe 1. 
#hipoteza zerowa (H0) zaklada, ze porownywane srednie nie roznia się.

#ddwa rodzaje błedów:
# 1. bład I rodzaju (alfa) - odrzucenie właściwej hipotezy
# 2. Błąd II rodzaju (beta) - przyjęcie niewłaściwej hipoterzy
#prawdopodobieństwo popelnienia bledu I rodzaju w statystyce nazywa sie "poziomem istotności", ktory dla kazdego testu musi być ustalony
#W tym przykladzie odrzucamy hipoteze, jesli wartosc bedzie za duza albo za mala (two-tailed test), tj. jesli znajdziemy sie w zasiegu tzw. critical region. 

#Zadanie: korzystajac z wzoru oblicz statystyke z dla podanych powyżej danych


#wykorzystując obliczoną wartość z możemy określić czy znajduje sie ona w critical region czy nie

alpha <- .05 
z.half.alpha <- qnorm(1-alpha/2) 
c(-z.half.alpha, z.half.alpha) 

#mozemy tez obliczyc wartosc p, tj. minimalna wartosc przy ktorej hipoteza zerowa bylaby odrzucona

2*(1 - pnorm(z, mean = 0, sd = 1)) # mnozym przez dwa poniewaz pnorm() liczy nam p tylko dla jednej strony rozkladu, a my przeprowadzamy two-tailed test

#funkcja R do przeprowadzenia z-testu
install.packages("BSDA")
library(BSDA)

z.test(x = c(19, 20, 21, 18, 22, 20), alternative = "two.sided", mu = 14.2, sigma.x = 4.7)

#z-test wykonuje się, kiedy parametry populacji są znane. Często tak nie jest dlatego opracowuje się alternatywne testy. Jednak kiedy liczba obserwacji w probie wynosi powyzej 30, to mozna niekiedy przyjac, ze parametry rozkladu obliczone na podstawie takiej proby sa na tyle dobrym przyblizeniem, ze i tak stosuje sie z test, a nie alternatywy.  

#w takich przypadkach obliczamy parametry populacji w oparciu o probe statstyczna, co zawsze prowadzi do podawania nie dokladnych wartosci, a przedzialow

#zadanie: korzystajac z wzoru oblicz srednia dlugosc populacji Composita w przedziale ufnosci 90% w oparciu o dane z proby statystycznej. Dla obliczenia wartosci z skorzystaj z poniższego wzoru:
qnorm(x, mean = 0, sd = 1) #pod x podstaw odpowiednie wartosci prawdopodobienstwa

#wynik sprawdx w oparciu o wykorzystanie funkcji z.test z poniższymi parametrami
test <- z.test(composita$dl, alternative = "two.sided", sigma.x = 4.7,  conf.level = 0.9)
ci <- tibble(lower = test$conf.int[[1]], upper = test$conf.int[[2]])

#rozklad t - podobny do rozkladu normalnego, ale jego dokladny ksztalt zalezy od liczby obserwacji w probie. 
#uwaga na stopnie swobody (oznaczanie grecką literą nu) - testy bazujace na probach statystycznych musza uwzglednic to, ze parametry statystyczne populacji musza byc oszacowane w oparciu o dane z prob statystycznych. Ma to swoje konsekwencje. Np.:
#obliczamy średnia w oparciu o 5 obserwacji:
x1 <- 3
x2 <- 2
x3 <- 8
x4 <- 9
x5 <- 4

X <- mean(c(x1, x2, x3, x4, x5))

#Jesli pozniej chcemy wykorzystac obliczona wartosc sredniej to musimy miec swiadomosc ograniczonej swobody. Np.

5*X - (x1 + x2 + x3 + x4) #musi się równać 4, czyli do obliczenia np. odchylenia standardowego będziemy mieli jedynie 4 niezależne obserwacje (n-1). Jesli jakis test wymaga obliczenia wiekszej liczby paramterow, to kazdy bedzie "kosztowal" jeden stopien swobody.

#Zadanie: wykorzystujac test t zweryfikuj hipoteze, ze rdzenie, dla ktorych zostala pomierzona porowatosc (dane ponizej) pochodzi z populacji, w ktorej średnia porowatosc wynosi więcej niż 18%. Obliczenia przeprowadz "na piechotę". Wykorzystaj kod z testu z podstawiając pod funkcję qnorm() funkcję qt(). W atrybutach funkcji określ odopowiednia wartość dla stopni swobody (df). Sprawdz uzyskany wynik wykorzystujac funkcje t.test(). (podpowiedż: tym razem mamy do czynienia z one-tailed testem)

porowatosc <- c(13, 17, 15, 23, 27, 29, 18, 27, 20, 24)

#Zadanie: wykorzystujac wzor dla rozkladu t oblicz srednią w 95% przedziale ufoności dla obiektu porowatosc. Wynik sprawdz z wykorzystaniem poniższego kodu:

test_t <- t.test(porowatosc, alternative = "two.sided", conf.level = 0.95, mu = 18)
ci <- tibble(lower = test_t$conf.int[[1]], upper = test_t$conf.int[[2]])

#testowanie srednich dwoch prob. Test zaklada jednakowosc wariancji, losowosc prob i rozklad normalny porownywanych zbiorow.

porowatosc2 <- c(15, 10, 15, 23, 18, 26, 24, 18, 19, 21)

#zadanie : wykorzystujac wzor na wartosc testowa t do porownywania srednich z prob statystycznych sprawdz, czy obserwacje z obiektu porowatosc i porowatosc2 pochodza z tych samych populacji. Wynik sprawdz wykorzystujac funkcje t.test()
 
#test t na korelacje zaklada rozklad normalny zmiennych i ze  obserwacje byly pozyskane losowo z populacji

#zadanie: sprawdz czy korelacja pomiędzy osią a i b w poniższym zbiorze zawierajacym dane z pomiarow najdluzszych (a) i najkrotszych (b) osi glazikow jest istotna statystycznie wykorzystujac wzor na test t do testowania korelacji. Wyszukaj przy pomocy ChataGPT/wszukiwarki odpowiednia funkcję do wykonania tego testu w R.

glaziki <- tibble(a = c(8, 16, 12, 13, 16, 14, 16, 11, 15, 13),
                  b = c(7, 8, 10, 12, 14, 9, 13, 6, 9, 10))

#Rozklad F - od nazwyska Sir Ronalda Fisher'a. Rozklad teoretyczny spodziewany przy losowaniu z populacji i obliczaniu dla wszystkich par prob statystycznych stosunku wariancji. Wsztstkie wartosci w rozkladzie sa pozytywne, srednia dla rozkladu wynosi 1.
# test F na testowanie wariancji - wieksza wartosc wariancji zawsze mamy w liczniku. Wtedy wartosc testowa F jest zawsze > 1 i mozemy przeprowadzać test, który jest one-tailed.

#Zadanie: kozystając z wzoru na test F na jednakowosc wariancji sprawdz czy pomiary porowatosci dla 2 zbiorow wczesniej wykorzystanych do testowania srednich maja jednakowa wariancje. 

#analiza wariancji (ANOVA): poprzednie metody skupialy sie na porownaniu dwoch prob albo proby z populacja. ANOVA pozwala na porownanie wiecej niz dwoch prob. Najbardziej ogolnie - metody ANOVA obejmuja rozklad calkowitej wariancji w zestawie prob na rozne skladowe. Testy na jednakowosc uwzgledniaja jednoczesnie roznice w srendich i wariancjach. Analiza ANOVA zaklada losowosc prob, normalnosc rozkladu populacji macierzystych i jednakowosc wariancji. Do testowania hipotez wykorzystujemy statystyke F.

#w jednoczynnikowej analizie wariancji (one-way ANOVA) całkowita wariancja jest rozbijana na 2 komponenty: wariancja w obrebie pojedynczych prob i wariancja pomiedzy probami. Potrzebne wzory 


#Zadanie:
oneova <- read.delim("dane/ONEOVA.txt") %>% 
  rename(caco3 = 'CaCO3..')