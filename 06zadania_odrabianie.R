#Geolodzy badają skład chemiczny próbek gleby pobranych z różnych lokalizacji w regionie górskim. Zebrano dane na temat zawartości czterech pierwiastków śladowych: miedzi (Cu), cynku (Zn), ołowiu (Pb) i niklu (Ni) w mg/kg w 40 różnych punktach pomiarowych.


Cu <- c(44.3952435, 47.6982251, 65.5870831, 50.7050839, 51.2928774, 67.1506499, 54.6091621, 37.3493877, 43.1314715, 45.5433803, 62.2408180, 53.5981383, 54.0077145, 51.1068272, 44.4415887, 9.5752083, 11.8186967, 80.8202342, 16.8629946, 19.3174242, 29.7055159, 26.9608897, 23.3705797, 32.1170469, 29.9348574, 31.4130509, 0.6353549, 11.9569938, 43.3567949, 10.1323146, 5.1911563, 51.9378423, 24.5805146, 15.8136352, 12.5856016, 25.0928201, 11.7736928, 22.5858007, 8.4072961, 144.2201515)

Zn <- c(99.07132, 95.41056, 94.29293, 89.57940, 96.88124, 81.01905, 132.53434, 118.11943, 83.15337, 93.95673, 93.00017, 111.69948, 98.74946, 103.79978, 99.57180, 99.35694, 120.52903, 96.61344, 122.74706, 76.76871, 141.69227, 132.47708, 134.31883, 137.59279, 119.95353, 123.33585, 109.62849, 108.56418, 136.07057, 138.96420, 131.06008, 148.44535, 171.00169, 120.17938, 83.81662, 150.11477, 115.81598, 116.23983, 150.51143, 124.30454)

Pb <- c(234.9765085, 153.7393979, 0.4599127, 110.8765468, 29.9970318, 119.2003007, 111.4928704, 6.7375891, 48.0668721, 157.0454339, 25.9946107, 185.6922287, 46.3219621, 23.6035747, 118.2099421, 5.9671372, 40.3238441, 94.2939840, 41.6580644, 75.3218405, 41.1936587, 36.8604696, 46.8032622, 36.9987021, 50.9366650, 47.6630531, 38.8214982, 34.8678955, 36.4479672, 41.2844185, 38.7665406, 38.2622870, 35.2419072, 39.7748614, 36.0754777, 31.6602903, 38.0988674, 44.5949830, 37.1232652, 43.0398216)

Ni <- c(75.732457, 68.947321, 80.384552, 72.136489, 77.893265, 74.589332, 70.214768, 79.462015, 85.126937, 76.503829, 72.946281, 81.735602, 66.284573, 74.100389, 77.593214, 82.450618, 69.832457, 73.291548, 74.968530, 79.582301, 86.125437, 64.823904, 76.451280, 71.389216, 78.542091, 73.664835, 82.912456, 67.735804, 74.568239, 80.673912, 76.038214, 84.257301, 69.125487, 78.214659, 77.348190, 73.218453, 75.890641, 72.156304, 76.951380, 79.874528)

#Zadania:

#oblicz parametry statystyczne, ktore pozwola na stwierdzenie czy dane dla poszczegolnych pierwiastkow maja rozklad normalny. Dla podjecia wlasciwej decyzji wspomoz sie odpowiednimi wykresami.

#sprawdz korelacje pomiedzy pierwiastkami (sprawdz wszystkie pary) wykorzystujac wlasciwy wskaznik, ktory wybierzesz na podstawie wlasciwosci rozkladow. Zwizualizuj relacje miedzy nimi na odpowiednim wykresie.

# Test Z dla cynku (Zn)
#W literaturze geologicznej średnia zawartość Zn w tym regionie wynosi 92 mg/kg przy znanym odchyleniu standardowym 18 mg/kg. Przeprowadź dwustronny test Z, aby sprawdzić, czy średnia zawartość Zn w pobranych próbkach jest inna niż wartość referencyjna (92 mg/kg). Użyj poziomu istotności α = 0.05. Jaki wniosek wyciagniesz z testu?

#Test dla niklu (Ni)
#W poprzednich badaniach średnia zawartość Cu wynosiła 70 mg/kg, ale odchylenie standardowe nie jest znane. Przeprowadź test t, aby sprawdzić, czy średnia zawartość Ni w pobranych próbkach różni się istotnie od wartości 70 mg/kg. Użyj poziomu istotności α = 0.1 i testu dwustronnego. Sformułuj wniosek.

