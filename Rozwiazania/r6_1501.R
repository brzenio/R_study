# -*- coding: utf-8 -*-
"""R6_1501.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1qllHiSdxUXHnMr7AzUehQv9KMMRlWIa7

Zadanie_1:

a) Stworz wektor liczb od 1 do 1000 dodajac do niego wartosci losowe z rozkladu normalnego o sredniej 100 oraz o odchyleniu standardowym 10.

b) Podziel powyzszy szerego na 6 rownych odcinkow.

c) Stworz za pomoca petli for 6 wykresow liniowy stworzonych w jednym okienku podzielonym na 6 obszarow.

d) Kazdy wykres stworzony jest na podstawie odcinkow z pkt b).

e) Tytul wykresu powinien zmieniac sie dynamicznie od "Wykres_1" do "Wykres_6".

f) Nazwa osi pionowej to "temperatura".
"""

set.seed(123) # ustawienie ziarna losowego
wektor <- 1:1000 + rnorm(1000, mean = 100, sd = 10) # stworzenie wektora i dodanie wartości losowych

odcinek <- length(wektor) / 6

for (i in 1:6) {
  assign(paste0("odcinek_", i), wektor[(odcinek*(i-1)+1):(odcinek*i)])
}

# punkt b z zewnetrzna biblioteka
library(dplyr)
odcinki <- wektor %>% mutate(grupa = cut(wektor, breaks = 6))


library(ggplot2)
for (i in 1:6){
    p <- ggplot(data = odcinki %>% filter(grupa == levels(grupa)[i]), aes(x = wektor)) +
    geom_line() +
    ggtitle(paste0("Wykres_", i)) +
    xlab("temperatura")
  print(p)
}

# d, e i f jest już powyżej

"""Zadanie_2:

a) Stworz nowy katalog "zadanie_2".

b) Ustaw sciezke dostepowa do katologu "zadanie_2".

c) Stworz w petli 20 plikow tekstowych o nazwach "plik_1",..."plik_20". 
   Kazdy plik powinien zawierac jeden wiersz z kolejnymi datami poczawszy od dnia dzisiejszego.

d) Za pomoca petli zlacz pliki z punktu "c)" w jeden plik "plik_wszystko". 
   Petla powinna iterowac po posortowanych nazwach plikow znajdujacaych sie w katalogu "zadanie_2".
"""

dir.create("zadanie_2")

setwd("zadanie_2")

write.table(data.frame(Sys.Date()), file = paste0("plik_",i,".txt"), sep = "\t", row.names = FALSE)

list.files(path = ".", pattern = "*.txt", full.names = TRUE)

write.table(read.table(file_name), file = "plik_wszystko", sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE)

"""Zadanie_3:

a) Stworz obiekt tekstowy "czas_char" z wartoscia "2023-sty-07 20:10:10".

b) Przeksztalc obiekt "czas_char" na obiekt "czas_num" przedstawiajacy czas numerycznie.

c) Stworz sekwencje "sekwencja" kolejnych 10 czasow oddzielonych od siebie jednym rokiem.

d) Wyeksportuj obiekt "sekwencja" do pliku pod nazwa "zadanie3.txt". Plik nie powinien zawierac nazw wierszy ani kolumn.
"""

czas_char <- as.character("2023-sty-07 20:10:10")

czas_num <- as.POSIXct(czas_char, format = "%Y-%b-%d %H:%M:%S")

sekwencja <- seq(from = czas_num, by = difftime(1, units = "years"), length.out = 10)

write.table(sekwencja, file = "zadanie3.txt", row.names = FALSE, col.names = FALSE)

"""Zadanie_4:

a) Napisz wlasna funkcje "cumsum" (zwracajaca wektor, ktorego elementami sa sumy skumulowane).

b) Porownaj jej dzialanie z wbudowana funkcja "cumsum".
"""

cumsum_custom <- function(x) {
  for (i in 2:length(x)) {
    x[i] <- x[i] + x[i-1]
  }
  return(x)
}

x <- c(1, 2, 3, 4, 5)
custom_result <- cumsum_custom(x)
builtin_result <- cumsum(x)

#Porownanie wyników
identical(custom_result, builtin_result)

"""Zadanie_5:

a) Napisz funkcje rozwiazujaca rownanie liniowe "y = ax + b" dla danych parametrow wejsciowych "a,x,b".
"""

solveLinearEquation <- function(a, x, b) {
  y <- a*x + b
  return(y)
}

solveLinearEquation(2, 3, 1) # zwraca 7
solveLinearEquation(a=3, x=5, b=2) # zwraca 17

"""Zadanie_6:

a) Napisz wlasna funkcje rownowazna wbudowanej funkcji "matrix". Zobacz kod zrodlowy "matrix".
"""

moja_matrix <- function(data, nrow, ncol, byrow = FALSE) {
    if (byrow) {
        dim(data) <- c(nrow, ncol)
    } else {
        dim(data) <- c(ncol, nrow)
    }
    return(data)
}

moja_matrix(c(1:10), 5, 2)

