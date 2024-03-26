library(tidyverse)

#kowariancja i korelacja - miary zależności liniowej między dwoma zmiennymi

#Zadanie: korzystając ze wzorów oblicz kowariancję i korelację dla zmiennych Cr i Ni z obiektu geochem

correl <- as_tibble(read.delim("dane/CORREL.txt")) # tabela pokazująca różny stopień korelacji pomiędzy sztucznie wegenerowanymi zmiennymi

#
ggplot(correl, aes(x = X1, y = X2)) +
  geom_point()

#Zadanie" korzystając z kodu powyżej utwórz wykres pokazujący relację pomiędzy 4 wybranymi zmiennymi. Oblicz dla wszystkich sytuacji współczynnik korelacji

#Uwaga ogolna - obliczanie korelacji z danych kompozycyjnych wymaga szczególnego podejścia

composita <- tibble(dl = c(18.4, 16.9, 13.6, 11.4, 7.8, 6.3),
                    szer = c(15.4, 15.1, 10.9, 9.7, 7.4, 5.3))