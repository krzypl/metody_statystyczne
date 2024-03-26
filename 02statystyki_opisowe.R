library(tidyverse)

pstat <- sample(1:20, 20, replace = TRUE)

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