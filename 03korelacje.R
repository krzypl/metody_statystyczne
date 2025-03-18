library(tidyverse)

#kowariancja i korelacja - miary zależności liniowej między dwoma zmiennymi

#Zadanie: korzystając ze wzorów oblicz kowariancję i korelację dla zmiennych Cr i Ni z obiektu geochem

correl <- as_tibble(read.delim("dane/CORREL.txt")) # tabela pokazująca różny stopień korelacji pomiędzy sztucznie wegenerowanymi zmiennymi

#
ggplot(correl, aes(x = X1, y = X2)) +
  geom_point()

#Zadanie" korzystając z kodu powyżej utwórz wykres pokazujący relację pomiędzy 4 wybranymi zmiennymi. Oblicz dla wszystkich sytuacji współczynnik korelacji

#Uwaga ogolna - obliczanie korelacji z danych kompozycyjnych wymaga szczególnego podejścia
