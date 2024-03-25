library(tidyverse)
#Regresja liniowa – w modelowaniu statystycznym, metody oparte o liniowe kombinacje zmiennych i parametrów dopasowujących model do danych. Dopasowana linia regresji reprezentuje oszacowaną wartość oczekiwaną zmiennej y przy konkretnych wartościach innej zmiennej lub zmiennych x

#Metoda najmniejszych kwadratów polega na dopasowaniu prostej do chmury punktów w taki sposób, aby suma kwadratów różnic pomiędzy rzeczywistą a oszacowaną wartością była jak najmniejsza

#reresja liniowa metodą najmniejszych kwadratów może być wykorzystywana do opisu, predykcji, estymacji i testowania hipotez

slope <- 2.15
intercept <- 4.64
x <- rnorm(n=100, mean=15, sd=3)
errors <- rnorm(n=100, sd=2)
y <- slope*x + intercept + errors

plot(x, y, pch=16, las=1) #wykres rozrzutu

reg <- lm(y ~ x)

print(reg)

summary(reg)

confint(reg) #zakres pewności dla współczynnikow modelu

par(mfrow=c(2, 2))
plot(reg) #wykresy do ewaluacji modelu

par(mfrow=c(1, 1))
plot(x, y, pch=16)
abline(reg, col="red", lwd=2, lty="dashed") #wykres rozrzutu z krzywa regresji

wartosci_do_oszacowania <- tibble(x = c(12, 16, 18))
predict(reg, wartosci_do_oszacowania)

predict(reg, wartosci_do_oszacowania, interval = "confidence")

predict(reg, wartosci_do_oszacowania, interval = "prediction")

#zadanie: przeprowadz regresje liniową najmniejszych kwadratow i sprobuj przewidziec wartosc porowatosci w populacji, z ktorej pochodzi proba porowatosc2 wtedy gdy porowatosc w populacji, z ktorej pochodzi proba porowatosc, przyjmuje wartosc 22. Jak bardzo uwierzysz uzyskanemu wynikowi? Odpowiedz uzasadnij w oparciu o ewaluację modelu.

porowatosc <- c(13, 17, 15, 23, 27, 29, 18, 27, 20, 24)
porowatosc2 <- c(15, 10, 15, 23, 18, 26, 24, 18, 19, 21)