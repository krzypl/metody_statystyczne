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