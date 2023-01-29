#
#
#
#
#
#
#
#
# zajecia 2 ------------------------------------------------------------------------------------------------------------
#
#
#
#
#
#
#
#
#
#


"""
Zadanie 1: W konsoli oraz w skrypcie R policz 2 + 2.
"""

2+2

"""Zadanie 2: Wyswietl pomoc dla funkcji wbudowanej "print".

"""

help(print)

"""Zadanie 3: Utwórz nowy plik programu "prog_xx.R" (xx – Twoje inicjaly) i zapisz na Pulpicie, napisz polecenie wyswietlające napis 'Hello World'.

"""

print("Hello World")

"""Zadanie 4: Dodaj do siebie liczby 10 i 10. Następnie wynik podziel przez 2. Od otrzymanego wyniku odejmij 5. Następnie pomnóż wynik przez 3. Użyj nawiasów lub klamer. W wyniku ostatecznym powinna wyjsc liczba 15."""

a <- 10
b <- 10
c <- a+b
print(c)
print((c/2-5)*3) # poprawić, żeby nie pisac dwóch printów

"""Zadanie 5: Napisz na ekranie nastepujace zdanie „Poczatek jest najwazniejsza czescia pracy” – Platon. Cytat powinien byc ujety w cudzyslow."""

print(" \"Poczatek jest najwazniejsza czescia pracy\" – Platon", quote = FALSE)
cat(' "Poczatek jest najwazniejsza czescia pracy" – Platon ')

"""Zadanie 6: Napisz na ekranie w oddzielnych linijkach napis:

Nauka programowania

wymaga

wytrwałej pracy
"""

cat("Nauka programowanie \n wymaga \n wytrwałej pracy")

"""Zadanie 7: Napisz program tworzący wektor o określonym typie i dlugosci. Utwórz wektor typow numerycznych, zlozonych, logicznych i znakowych o dlugosci 10."""

num_v <- vector("integer", 10)
com_v <- vector("complex", 10)
logic_v <- vector("logical", 10)
char_v <- vector("character", 10)

num_v
com_v
logic_v
char_v 

# Też dobrze?
numeric_vector <- numeric(10)
complex_vector <- complex(10)
logical_vector <- logical(10)
character_vector <- character(10)

numeric_vector
complex_vector
logical_vector
character_vector

"""Zadanie 8: Napisz program, który doda wartosc do danego pustego wektora.

"""

x <- NULL
x <- c(x, 2)
x

"""Zadanie 9: Napisz program wyznaczajacy sume, średnia i iloczyn wektora, zignoruj elementy takie jak NA lub NaN.

"""

x <- c(1, 1, 2, 3, 5, 8, 13, 21)
sum(x, na.rm = TRUE)
mean(x, na.rm = TRUE)
prod(x, na.rm = TRUE)

"""Zadanie 10: Napisz program sortujacy wektory w porządku rosnacym i malejacym.

"""

sort(x)
sort(x, decreasing = T)

"""Zadanie 11: Napisz program znajdujacy elementy danego wektora, ktorych nie ma w innym danym wektorze.

"""

v <- c('a','b','c','e')

'b' %in% v

match('b',v)

v <- c('a','b','c','e')
z <- c('e','f','g','h')
z %in% v

match(z,v)

"""Zadanie 12: Napisz program, ktory odwroci kolejność danego wektora."""

x <- c(1, 1, 2, 3, 5, 8, 13, 21)
rev(x)

"""Zadanie 13: Napisz program tworzacy wektor, który zawiera 10 losowych liczb całkowitych z przedzialu od -10 do +10."""

x13 <- sample(-10:10, 10)
x13

"""Zadanie 14: 

a) Wysymuluj (bez powtorzen) 20 liczb calkowitych z przedzialu od 1 do 20. Poczatkowa wartosc generatora liczb pseudolosowych powinna zostac ustalona na 444.

b) Wysymuluj (bez powtorzen) 20 malych liter (alfabet podstawowy). Nie ustawiaj poczatkowej wartosci generatora liczb pseudolosowych.

c) Wyswietl wszystkie litery (pkt. b) dla ktorych liczby z odpowiadajacych indeksow z wektora numerycznego (pkt. a) sa wieksze od 5 i jednoczesnie mniejsze od 15.
"""

set.seed(444)

x20 <- sample(1:20, replace = FALSE)
length(x20)
x20

set.seed(NULL)
x_let <- sample(letters, 20, replace = FALSE)
x_let

x_let_comp <- x_let[x20 > 5 & x20 < 15]
x_let_comp

"""Zadanie 15:

a) Stworz wektor "litery" zawierajacy wielkie litery z podstawowego alfabetu (26 liter).

b) Stworz wektor "numery" zawierajacy kolejne liczby calkowite od 1 do "n", 
gdzie "n" jest liczba elementow z wektora "litery" (nie wpisujemy tej wartosci recznie).

c) Stworz wektor "litery_numery" laczacy wektory "litery" i "numery" element po elemenice poprzez znak "_".
"""

litery <- LETTERS
litery

numery <- (1:length(litery))
numery

litery_numery <- paste(litery, "_", numery)
litery_numery

"""Zadanie 16: Napisz program wybierajacy drugi element z podanej listy zagniezdzonej."""

lista <- c(1,2,3)
lista1 <- c(11,22,33)
lista2 <- c(111,222,333)
lista_nest <- list(lista,lista1,lista2)
lista_nest[[2]]

"""Zadanie 17: Napisz program, ktory utworzy liste zawierajaca wektor i liste oraz zaktualizuje ostatni element."""

wektor <- c(1,2,3)
lista <- list(4,5,6)

new_list <- list(wektor,lista)
new_list

new_list[[2]][3] <- 7
new_list

"""Zadanie 18: Napisz program, ktory scali dwie podane listy w jedna liste."""

list1 <- list(1,2,3)
list2 <- list(4,5,6)

# lub unlist(list(list1,list2))
list_c <- c(list1,list2)
list_c

"""Zadanie 19: Napisz program konwertujacy dana liste na wektor."""

lista <- list(1,2,3)

# lub new_list <- as.vector(lista)
new_list <- unlist(lista)
new_list

"""Zadanie 20: Napisz program, ktory zliczy liczbe obiektow w liscie."""

lista <- list(1, 2, 3, 4, 5, c("a", "b"), data.frame(x = 1:3, y = 4:6), TRUE)

liczba_obiektow <- sapply(lista, function(x) length(x))
liczba_obiektow

"""Zadanie 21: Napisz program, ktory przypisze wartosc NULL do danego elementu listy."""

lista <- list(1,2,3,4,5)
lista[[1]] <- NULL
lista

"""Zadanie 22: Napisz program, ktory doda 10 do kazdego elementu pierwszego wektora na podanej liscie.

"""

lista <- list(c(1:5), c(10:15), c(20:25))
lista[[1]] <- lista[[1]] + 10

lista

"""Zadanie 23: Napisz program, aby uzyskac dlugosc pierwszych dwoch wektorow z danej listy."""

lista <- list(c(1:5), c(10:15), c(20:25))
length(lista[[1]])
length(lista[[2]])

"""Zadanie 24: Napisz program, ktory znajdzie wszystkie elementy danej listy, ktorych nie ma na innej podanej liscie."""

lista1 <- list(1,2,3,4,5)
lista2 <- list(3,4,5,6)

setdiff(lista1,lista2)

"""Zadanie 25:

a) Wysymuluj z powtorzeniami 10 liczb z wektora od 1 do 10 (seed = 666).

b) Przeksztalc wektor z punktu a) na 10-elementowa liste.

c) Dla kazdego elementu listy wysumuluj "n" liczb z rozkladu rownomiernego (seed = 666), gdzie "n" jest liczba obecnie znajdujaca sie w elemencie listy.

d) Dokonaj zlaczenia elementow listy w jeden wektor.
"""

set.seed(666)

x <- sample(1:10, 10, TRUE)
x

listx <- as.list(x)
listx

for(i in 1:length(listx)){
  listx[[i]] <- runif(listx[[i]], 0, 1)
}

listx

wektor <- unlist(listx)
wektor

"""Zadanie 26:

a) Stworz liste o dlugosci 5 o wartosciach domyslnych.

b) Stworz wektor poprzez wylosowanie kolejnych 5 liczb calkowitch (bez powtorzen) z wektora od 1 do 10. Poczatkowa wartosc generatora liczb pseudolosowych powinna zostac ustalona na 123.

c) Wstaw do kolejnych elementow listy wektor powtarzajacy konkretna liczbe z wektora z pkt b) tyle razy, jaka liczba zawarta jest pod konkretnym elementem wektora, np. dla liczby 4 wektor bedzie mial dlugosc 4 i w kazdym elemencie bedzie liczba 4.

d) Przeksztalc liste w wektor liczb calkowitych.
"""

lista <- vector(mode = "list", length = 5)

set.seed(123)
v <- sample(1:10, 5, FALSE)
v

for(i in 1:length(lista)){
  lista[[i]] <- rep(v[i],v[i])
}

lista

wektor <- unlist(lista)
wektor


#
#
#
#
#
#
#
#
# zajecia 3 ------------------------------------------------------------------------------------------------------------
#
#
#
#
#
#
#
#
#
#

**We wszystkich ponizszych zadaniach stworz wlasne obiekty, tj. nazwane wektory, niepuste wektory i listy.**
"""
Zadanie 1: Napisz funkcje, ktora oblicza liczbe a do potęgi b, niech b ma domyślną wartość 2.
"""

power <- function(x, y=2){
  return(x^y)
}

power(3)

"""Zadanie 2: Napisz funkcje, ktora zmienia wartość pi w globalnym środowisku R na dowolną wartość określoną jako argument."""

change_pi <- function(value){
  assign("pi", value, envir = .GlobalEnv)
}

rm(pi)
pi
change_pi(2)
pi

"""Zadanie 3: Napisz funkcję, ktara sprawdza, czy wartosci z pierwszego wektora znajduja się w zakresie między maksimum a minimum drugiego wektora."""

v1 <- c(1:10)
v2 <- c(5:20)

min <- min(v2)
max <- max(v2)

min
max

check <- function(v1, v2){
  v1[v1 >= min & v1 <= max]
}

check(v1,v2)

"""Zadanie 4: Napisz funkcje zliczajaca liczbe brakow danych w danym wektorze."""

v <- c(1,NaN, NaN, 2, 3)

IsNaN <- function(v){
  sum(is.na(v))
}

IsNaN(v)

"""Zadanie 5: Napisz funkcje konwertujaca wartosci w danym wektorze typu NA, NaN, INF, na jego wartosc srednia."""

v <- c(1,NaN, NaN, 2, 3)
m <- mean(v, na.rm = TRUE)
m

conv <- function(v){
  for(i in 1:length(v)){
    if(is.na(v[i]) | is.infinite(v[i] | is.nan(v[i]))){
    v[i] <- m
    }
  }
   return(v)
}

conv2 <- function(v) {
  v[is.na(v) | is.infinite(v)] <- mean(v, na.rm = TRUE)
  v
}


conv(v)
conv2(v)

"""Zadanie 6: Napisz funkcje, ktora wyswietli w konsoli informacje o liczbie elementow w danym wekotrze oraz w linijce ponizej informacje o typie wektora - prosze wykorzystac tylko raz funkcje do wyswietlania informacji."""

v <- c(1:10)

info <- function(v){
  cat("Length: ", length(v), "\n", "Type: ", typeof(v))
}

info(v)

"""Zadanie 7: Napisz funkcje wyznaczajaca wartosc srednia oraz mediane, pomijajac wartosci minimalna i maksymalna w danym wektorze. Funkcja nie powinna zwracac niczego oraz powinna wyswietlac w konsoli tresc typu "Srednia to: xxx, mediana to: xxx"
"""

v <- sample(1:100, 10, FALSE)
# v
# v <- sort(v)
# v

# mean(v)
# median(v)

# v1 <- c(v[3:length(v)-1])
# v1 

# v2 <- v
# v3 <- v2[c(-1,-length(v))]

# mean(v3)
# median(v3)


math_print <- function(v){
  v <- sort(v)
  v2 <- v[c(-1,-length(v))]
  cat("Srednia to:", mean(v2), ", mediana to:", median(v2))

}
# math_print(v)
# cat("Srednia to: ", mean, ", mediana to: ", median)



mean_median2 <- function(vec) {
  vec <- sort(vec)
  mean_val <- mean(vec[vec != min(vec) & vec != max(vec)], na.rm = TRUE)
  median_val <- median(vec[vec != min(vec) & vec != max(vec)], na.rm = TRUE)
  cat("Srednia to:", mean_val, ", mediana to:", median_val, "\n")
}

math_print(v)
cat("\n")
mean_median2(v)

"""Zadanie 8: Napisz funkcje, ktora wyznaczy dla danego wektora wejsciowego x, pewna statystyke opisowa np. srednia, min, max - owa statystyka powinna byc przekazana do fukcji jako prametr. Fukcja powinna wyswietlac w konsoli tresc "Wartosc [Nazwa przekazanej funkcji] to: xxx"."""

v <- sample(1:100, 10, FALSE)

stat <- function(v, stat_type){
  stat_calc <- stat_type(v)

  # można zamienić też na:
  # deparse(substitute(stat_type))
  # as.character(substitute(stat_type))
  cat("Wartosść", substitute(stat_type), "to:", stat_calc, "\n")
}

stat(v, mean)
stat(v, median)

"""Zadanie 9:

a) Stworz funkcje wskazujaca obserwacje odstajace z podanego wektora liczb rzeczywistych (x <-rcauchy(10)) wykorzystujaca regule kwartylow: Q1-1.5*(Q3-Q1) Q1+1.5*(Q3-Q1).

b) Funkcja powinna zwracac obiekt z podanymi atrybutami zawierajacymi informacje:
  a. jakie wartosci sa odstajace
  b. jakie wartosci sa nieodstajace
  c. indeksy wartosci odstajacych
  d. indeksy wartosci nieodstajacych

c) (wykorzystaj funkcje quantile, which, attr).
"""

v <- sample(1:100, 10, FALSE)

outliers <- function(v){
  Q1 <- quantile(v, 0.25)
  Q3 <- quantile(v, 0.75)

  roznica_q <- Q3 - Q1
  out <- v[v < (Q1 - 1.5 * roznica_q) | v > (Q3 + 1.5 * roznica_q)]
  non_out <- v[v >= (Q1 - 1.5 * roznica_q) & v <= (Q3 + 1.5 * roznica_q)]

  out_index <- which(v %in% out)
  non_out_index <- which(v %in% non_out)

  return(list(out = out, non_out = non_out, outl_index = out_index, non_out_index = non_out_index))
}


# outliers2 <- function(vec) {
#   Q1 <- quantile(vec, 0.25)
#   Q3 <- quantile(vec, 0.75)
#   IQR <- Q3 - Q1
#   outliers <- vec[vec < (Q1 - 1.5 * IQR) | vec > (Q3 + 1.5 * IQR)]
#   non_outliers <- vec[vec >= (Q1 - 1.5 * IQR) & vec <= (Q3 + 1.5 * IQR)]
#   outlier_indices <- which(vec %in% outliers)
#   non_outlier_indices <- which(vec %in% non_outliers)
#   return(list(outliers = outliers, non_outliers = non_outliers, outlier_indices = outlier_indices, non_outlier_indices = non_outlier_indices))
# }

outliers(v)
# outliers2(v)

"""Zadanie 10:

a) Stworz dwa wektory np. Nie_odstajace oraz Odstajace, ktore beda przechowywac wyniki zwracane przez funkcje stworzona w poprzednim kroku.
"""

# Wyżej jest już wynik

"""Zadanie 11:

a) Wysymuluj 30 liczb calkowitych z przedzialu od 10 do 20. Poczatkowa wartosc generatora liczb pseudolosowych powinna zostac ustalona na 444.

b) Stworz nastepnie poprzez prealokacje wektor liczb calkowitych o wartosciach domyslnych. 
   Dlugosc tego wektora ustalona jest jako dlugosc wysymulowanego wektora z punktu powyzej.

c) Za pomoca petli for wstaw nastepnie elementy wektora losowego (pkt. a) do wektora z pkt. b. 

   Petla powinna byc wykonana w dwoch wersjach: operujac na indeksach obydwu wektorow oraz na rzeczywistych wartosciach jednego z nich. W ostatnim kroku kazda petla powinna wyswietlac (nie uzywajac funckji print()) caly wektor (pkt. b) oraz informacje o jego typie (wiersz ponizej). 
"""

set.seed(444)

x <- sample(10:20, 30, replace = TRUE)
x

default <- integer(length(x))
default

for(i in 1:length(x)){
  default[i] <- x[i]
}

cat(default, "\n")
cat("Typ wektora:", typeof(default), "\n")

default2 <- integer(length(x))

for (el in x) {
  default2 <- c(default2, el)
}

cat(default2, "\n")
cat("Typ wektora:", typeof(default2), "\n")

"""Zadanie 12:

a) Stworz wektor zawierajacy liczby od 1 do 9,

b) Stworz liste o dlugosci odpowiadajacej dlugosci wektora z pkt. a.

c) Za pomoca petli for iterujacej w ramch wartosci zawartych w wektorze z pkt. a, wstaw w kolejne elementy listy podzbiór wektora z pkt a, tak aby
  pierwszy element listy zawieral pierwszy element, drugi element listy zawieral pierwszy oraz drugi element,..., 
  dziewiaty element listy zawieral wszystkie elementy. Stworzy sie swojego rodzaju piramida.
"""

v <- c(1:9)

lista <- vector(mode = "list", length(v))

for(i in v){
  lista[[i]] <- v[1:i]
}

lista

"""Zadanie 13:

a) Wykonaj powyzsze zadanie nie wykorzystujac petli.
"""

lista1 <- lapply(1:length(v), function(i) v[1:i])
lista1














#
#
#
#
#
#
#
#
# zajecia 4 ------------------------------------------------------------------------------------------------------------
#
#
#
#
#
#
#
#
#
#

"""
Zadanie 1: Napisz program tworzacy pusta macierz.
"""

mat = matrix(, nrow = 1, ncol = 1)
mat

"""Zadanie 2: Napisz program tworzacy macierz przyjmujac jako dane wejsciowe podany wektor liczb. Wyswietl macierz."""

m_vec = matrix(c(1, 2, 3, 4), 2, 2)
m_vec

"""Zadanie 3: Napisz program, ktory utworzy macierz przyjmujac jako dane wejsciowe podany wektor liczb i zdefiniuje nazwy kolumn i wierszy. Wyświetl macierz."""

colnames(m_vec) <- c("Kolumna 1", "Kolumna 2")
rownames(m_vec) <- c("Wiersz 1", "Wiersz 2")
m_vec

"""Zadanie 4: Napisz program uzyskujacy dostep do elementu w 3 kolumnie i 2 wierszu; tylko 3 wierszu i tylko 4 kolumnie danej macierzy."""

m <- matrix(1:12, nrow = 3, ncol = 4)
colnames(m) <- c("col1", "col2", "col3", "col4")
rownames(m) <- c("row1", "row2", "row3")

# Dostęp do elementu w 3 kolumnie i 2 wierszu
m[2,3]

# Dostęp tylko do 3 wierszy i 4 kolumn
m[3,4]

"""Zadanie 5: Napisz program tworzacy dwie macierze 2x3. Dodawaj, odejmij, pomnoz i podziel macierze."""

mat1 <- matrix(1:6, nrow = 2, ncol = 3)

mat2 <- matrix(7:12, nrow = 2, ncol = 3)

mat1
mat2

# Dodawanie macierzy
mat_sum <- mat1 + mat2

# Odejmowanie macierzy
mat_diff <- mat1 - mat2

# Mnozenie macierzy
mat_prod <- mat1 * mat2

# Dzielenie macierzy
mat_quot <- mat1 / mat2

# Wyswietlanie wyniku
mat_sum
mat_diff
mat_prod
mat_quot

"""Zadanie 6: Napisz program tworzacy macierz z listy podanych wektorow."""

vectors <- list(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))
matrix_from_vectors <- matrix(unlist(vectors), ncol = 3, byrow = TRUE)

matrix_from_vectors

"""Zadanie 7: Napisz program, ktory wyodrebni podmacierz, ktorej wiersze maja wartosc kolumny > 7 z danej macierzy."""

# Do poprawy, bo nie działa
m <- matrix(1:12, 4, 3)
print (m)

submatrix <- m[m[, 1] > 7, ]

print(submatrix)

"""Zadanie 8: Napisz program konwertujacy dana macierz do listy wektorow kolumnowych."""

m <- matrix(1:12, 4, 3)
column_vectors <- as.list(apply(m, 2, as.vector))

column_vectors

"""Zadanie 9: Napisz program, ktory znajdzie indeks wierszy i kolumn o wartoĹ›ci maksymalnej i minimalnej w danej macierzy."""

m <- matrix(1:12, 4, 3)
min_index <- which(m == min(m), arr.ind = TRUE)
max_index <- which(m == max(m), arr.ind = TRUE)

min_index
max_index

"""Zadanie 10: Napisz program, ktory obraca dana macierz o 90 stopni zgodnie z ruchem wskazowek zegara."""

m <- matrix(1:12, 4, 3)
rotated_matrix <- t(apply(m, 2, rev))

m
rotated_matrix

"""Zadanie 11: Napisz program konwertujacy dana macierz na tablice jednowymiarowa."""

m <- matrix(1:12, 4, 3)
arr <- as.vector(m)

arr

"""Zadanie 12: Napisz program tworzacy trojwymiarowa tablice 24 elementow za pomoca funkcji dim()."""

dim_tab <- array(1:24, dim = c(2, 3, 4))

# bez printu daje tylko ciąg znaków
print(dim_tab)

"""Zadanie 13: Napisz program tworzacy dwuwymiarowa tablice 5x3 sekwencji parzystych liczb calkowitych wiekszych niz 50."""

even_num <- seq(52, by = 2, length.out = 15)
two_dim <- matrix(even_num, 5, 3)

two_dim

"""Zadanie 14:

a) Stworz macierz o rozmiarze "30x12".

b) Poczatkowa wartosc generatora liczb pseudolosowych powinna zostac ustalona na 222.

c) Pierwsza kolumna powinna zawierac 30 unikalnych wartosci losowych z wektora "1:30".

d) Ostatnia kolumna powinna zawierac 30 nieunikalnych wartosci losowych z wektora "1:3".

e) Pozostale kolumny powinny zawierac wartosci losowe pochodzace z rozkladu rownomiernego "runif()".
"""

set.seed(222)
matrix_a <- matrix(nrow = 30, ncol = 12)

# Wypełnienie pierwszej kolumny unikalnymi wartościami z wektora 1:30
matrix_a[, 1] <- sample(1:30, 30, replace = FALSE)

# Wypełnienie ostatniej kolumny nieunikalnymi wartościami z wektora 1:3
matrix_a[, 12] <- sample(1:3, 30, replace = TRUE)

# Wypełnienie pozostałych kolumn wartościami losowymi z rozkładu rownomiernego
for (i in 2:(ncol(matrix_a)-1)) {
  matrix_a[, i] <- runif(30)
}

matrix_a











#
#
#
#
#
#
#
#
# zajecia 5 ------------------------------------------------------------------------------------------------------------
#
#
#
#
#
#
#
#
#
#


adresy <- c("Kasztanowa 14, Bielsko Biała", "Łąki Zielonej 152, Katowice", "Specerowa 8, Elbląg")
produkty <- c("TV ", "laptop", " przenośna ładowarka", "Bezprzewodowa klawiatura", " SłuchaWki " )
dluga_sentencja <- stringr::sentences[1:10]
nazwa_pola <- c( "order_number", "order_date", "customer_email", "product_ title", "amount" )
pracownik_umiejetnosci <- c("Jan Kowalski (Beginner)", "Marta Górska (Pro)", "Krzysztof Biały (Pro)", "Sarah Machoń (Medium)")

"""Zadanie 1: Normalizuj wektor adresów, zastępując wielkie litery małymi literami."""

adresy
adresy <- tolower(adresy)
adresy

"""Zadanie 2: Wybierz tylko liczbową część wektora adresów."""

adresy_liczbowe <- gsub("[^0-9]", "", adresy)
adresy_liczbowe

"""Zadanie 3: Podziel wektor adresów na dwie części: adres i miasto. Wynik powinien być macierzą."""

adresy_miasta <- strsplit(adresy, ",")
adresy_miasta <- lapply(adresy_miasta, function(x) c(x[1], x[2]))
adresy_miasta
adresy_miasta <- do.call(rbind, adresy_miasta)

adresy_miasta

"""Zadanie 4: Podziel wektor adresów na trzy części: numer domu, ulicę i miasto. Wynik powinien być macierzą. Podpowiedź: użyj asercji z wyrażeniem w tył regex."""

adresy <- c("ul. Wiejska 12", "ul. Krakowska 15", "ul. Kościuszki 20")

adresy_podzielone <- strsplit(adresy, " ")

adresy_macierz <- as.data.frame(adresy_podzielone)

colnames(adresy_macierz) <- c("Ulica", "Numer")
adresy_macierz

# Nie działa podział adresu na ulicę i numer, zapewne trzeba poprawić drugi warunek w strsplit

adresy_podzielone <- strsplit(adresy, " |, ", )
adresy_podzielone <- lapply(adresy_podzielone, function(x) c(x[2], x[1], x[3]))
adresy_podzielone <- do.call(rbind, adresy_podzielone)

adresy_podzielone

"""Zadanie 5:

W wektorze dluga_sentencja dla zdań zaczynających się na literę „T” lub kończących się na literę „s” pokaż odpowiednio pierwsze lub ostatnie słowo.

Jeśli zdanie zarówno zaczyna się na „T”, jak i kończy na „s”, pokaż zarówno pierwsze, jak i ostatnie słowo.

Pamiętaj, że faktyczny ostatni znak zdania to zwykle kropka.
"""

dluga_sentencja_zmieniona <- sapply(dluga_sentencja, function(x) {
  first_word <- strsplit(x, " ")[[1]][1]
  last_word <- strsplit(x, " ")[[1]][length(strsplit(x, " ")[[1]])]
  if (substr(x, 1, 1) == "T" & substr(last_word, nchar(last_word), nchar(last_word)) == "s") {
    paste(first_word, last_word)
  } else if (substr(x, 1, 1) == "T") {
    first_word
  } else if (substr(last_word, nchar(last_word), nchar(last_word)) == "s") {
    last_word
  } else {
    x
  }
})

dluga_sentencja_zmieniona

"""Zadanie 6: Pokaż tylko pierwszych 20 znaków wszystkich zdań w wektorze dluga_sentencja Aby wskazać, że usunąłeś niektóre znaki, użyj dwóch kolejnych kropek na końcu każdego zdania."""

dluga_sentencja_skrocone <- substr(dluga_sentencja, 1, 20)
dluga_sentencja_skrocone <- paste0(dluga_sentencja_skrocone, "..")

dluga_sentencja_skrocone

"""Zadanie 7: Znormalizuj wektor produkty, usuwając wszystkie niepotrzebne spacje (zarówno na początku, na końcu, jak i w środku) oraz używając wielkich liter."""

produkty <- gsub(" ", "", produkty)
produkty <- toupper(produkty)

# wersja rozszerzona ???
# produkty <- gsub("^\\s+|\\s+$", "", produkty)
# produkty <- gsub("\\s+", " ", produkty)


produkty

"""Zadanie 8: Przygotuj wektor nazwa_pola, zastępując wszystkie symbole podkreśleń spacjami i konwertując je na wielkie litery."""

nazwa_pola
nazwa_pola <- gsub("_", " ", nazwa_pola)
nazwa_pola <- toupper(nazwa_pola)

nazwa_pola

"""Zadanie 9: Wyrównaj wszystkie nazwa_pola, aby miały równą długość, dodając spacje na początku odpowiednich ciągów."""

max_length <- max(nchar(nazwa_pola))
max_length

# Trzeba coś poprawić w tej funkcji, gdzieś w warunku paste0 lub rep
nazwa_pola_rownej_dlugosci <- sapply(nazwa_pola, function(x) paste0(rep("-", max_length - nchar(x)), x))

print(nazwa_pola)
print(nazwa_pola_rownej_dlugosci)

"""Zadanie 10:

W wektorze pracownik_umiejetnosci poszukaj pracowników, którzy są zdefiniowani jako „Pro” lub „Medium”.

Wynikiem powinna być macierz, która zawiera imię i nazwisko pracownika w pierwszej kolumnie, a poziom umiejętności (bez nawiasów) w drugiej kolumnie.

Pracownicy, którzy nie są zakwalifikowani, powinni otrzymać brakujące wartości w obu kolumnach.
"""

# wyrażenie regularne do wyszukania poziomów "Pro" lub "Medium"
poziom_umiejetnosci <- "(Pro|Medium)"

# indeksy pracowników spełniających warunek
indeksy_pracownikow <- grep(poziom_umiejetnosci, pracownik_umiejetnosci)

# wyciągnięcie imion i nazwisk oraz poziomów umiejętności
pracownicy_wymagani <- pracownik_umiejetnosci[indeksy_pracownikow]
imiona_nazwiska <- gsub("\\s\\(.*", "", pracownicy_wymagani)
poziomy_umiejetnosci <- gsub(".*\\(|\\)", "", pracownicy_wymagani)

# utworzenie macierzy z danymi
macierz_pracownicy <- cbind(imiona_nazwiska, poziomy_umiejetnosci)

macierz_pracownicy

"""Zadanie 11:

a) Stworz wektor x zawierajacy wszystkie znaki odpowiadajace wartosciom liczbowym z zakresu 1:128.

b) Wyswietl wszystkie znaki z wektora x, ktore odpowiadaja liczbom lub literom malym lub wielkim.

c) Wyswietl wszystkie znaki z wektora x, ktore odpowiadaja liczbom lub znakom oddzielajacym tekst tj. spacja, tabulator etc.

b) Wyswietl wszystkie znaki z wektora x, ktore nie odpowiadaja liczbom.
"""

# x <- chartoRaw(as.character(1:128))
x <- as.raw(1:128)
x

# raczje źle, bo wyświetla zupełnie coś innego
x[grep("[a-zA-Z0-9]", x)]
rawToChar(x[grep("[a-zA-Z0-9]", x)])
x[grep("[0-9\\s]", x)]
rawToChar(x[grep("[0-9\\s]", x)])
x[grep("[^0-9]", x)]
rawToChar(x[grep("[^0-9]", x)])

"""Zadanie 12:

a) Stworz wektor y zawierajacy nastepujace imiona: ("Jacek", "Kasia", "Małgosia","Elżbieta","joasia").

b) Stworz wyrazenie regularne, ktore spowoduje wybranie tylko imion: "Kasia","joasia".

c) Stworz wyrazenie regularne, ktore spowoduje wybranie tylko imion: "Jacek", "Kasia".
"""

y <- c("Jacek", "Kasia", "Małgosia","Elżbieta","joasia")
y

# Można chyba pominąć "^"
y[grep("^Kasia|^joasia", y)]
y[grep("^Jacek|^Kasia", y)]

"""Zadanie 13:

a) Stwotrz obiekt "dane" na podstawie nastepujacego wektora: c("1", "1 2", "12a3", "1223", "122234", "212234","1 22 3").

b) Stworz wyrazenie regularne znajdujace elementy wektora "dane", konczace sie na "2" lub "3".

c) Stworz wyrazenie regularne znajdujace elementy wektora "dane", posiadajace od jedej do trzech "2" nie bedacych jednoczesnie poczatkiem ani koncem tekstu.

d) Stworz wyrazenie regularne znajdujace elementy wektora "dane", posiadajace male litery lub spacje.
"""

dane <- c("1", "1 2", "12a3", "1223", "122234", "212234","1 22 3")

grep("[23]$", dane, value = T)

grep("^[^2]*2[^2]*2[^2]*2?[^2]*$", dane, value = T)

grep("[a-z ]", dane, value = T)

"""Zadanie_14:

a) Stworz macierz rozmiaru "10X10" z wartosciami losowymi z rozkladu rownomiernego.

b) Stworz funkcje "Funkcja" ktora zwracac bedzie do obiektu "wynik_14" wartosc srednia dla kazdej kolumny macierzy wejsciowej.

c) Stworz "Funkcja_env" ktora bedzie "srodowiskowa" wersja powyzszej funkcji. Powinna ona nie zwracac zadnego obiektu, lecz umieszczac wynikowy obiekt w srodowisku, ktore bedzie obiektem wejsciowym dla funkcji.
"""

matrix <- matrix(runif(100), nrow = 10, ncol = 10)

Funkcja <- function(matrix) {
  wynik_14 <- apply(matrix, 2, mean)
  return(wynik_14)
}


Funkcja_env <- function(matrix) {
  assign("wynik_14", apply(matrix, 2, mean), envir = .GlobalEnv)
}

Funkcja(matrix)
wynik_14
Funkcja_env(matrix)
wynik_14
rm(wynik_14)

"""Zadanie_2:

a) Stworz srodowisko "Pierwsze".

b) Umiesc w srodowisku "Pierwsze" obiekt "x" przyjmujacy wartosc 10.

c) Stworz srodowisko "Drugie" w srodowisku "Pierwsze". (Po wyswietleniu obiektow znajdujacych sie w srodowisku "Pierwsze", srodowisko "Drugie" powinno byc tam widoczne)

d) Umiesc w srodowisku "Drugie" obiekt "y" przyjmujacy wartosc 20.

e) Stworz funkcje zamieniajaca wartosc obiektu "x" w srodowisku "Pierwsze" na warotsc z obiektu "y" ze srodowiska "Drugie". Funkcja przyjmuje jako argument srodowisko.
"""

# Nie wiem czy to w ogóle działa

adresy_miasta_ulica <- gsub("(.) (.), (.*)", "\1,\2,\3", adresy)
adresy_miasta_ulica <- strsplit(adresy_miasta_ulica, ",")
adresy_miasta_ulica <- lapply(adresy_miasta_ulica, function(x) c(x[1], x[2], x[3]))
adresy_miasta_ulica <- do.call(rbind, adresy_miasta_ulica)

adresy_miasta_ulica













#
#
#
#
#
#
#
#
# zajecia 6 ------------------------------------------------------------------------------------------------------------
#
#
#
#
#
#
#
#
#
#
"""
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
