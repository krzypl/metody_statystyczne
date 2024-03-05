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

#kowariancja i korelacja - miary zależności liniowej między dwoma zmiennymi

#Zadanie: korzystając ze wzorów oblicz kowariancję i korelację dla zmiennych Cr i Ni z obiektu geochem

correl <- as_tibble(read.delim("dane/CORREL.txt")) # tabela pokazująca różny stopień korelacji pomiędzy sztucznie wegenerowanymi zmiennymi

#
composita <- tibble(dl = c(18.4, 16.9, 13.6, 11.4, 7.8, 6.3),
                    szer = c(15.4, 15.1, 10.9, 9.7, 7.4, 5.3))

ggplot(correl, aes(x = X1, y = X2)) +
  geom_point()

#korzystając z kodu powyżej utwórz wykres pokazujący relację pomiędzy 4 wybranymi zmiennymi. Oblicz dla wszystkich sytuacji współczynnik korelacji

#Uwaga ogolna - obliczanie korelacji z danych kompozycyjnych wymaga szczególnego podejścia

#centralne twierdzenie graniczne
