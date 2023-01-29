"""
Zadanie_1

A)swórz wektor x zawierający znaki odpowiadające liczbą od 1 do 128

B)swtórz wektor zawierający następujące elementy :"Kasia", "MaĹ‚gosia", "ElĹĽbieta", "joasia", "acccb", "ab 100"

C) wyświetl wszystkie znaki z wektora x które odpowiadają cyfrą lub literą dużym / małym

d) jak wyżej odpowiadające znaką specjalnym

e) jak wyżej nie odpowiadające cyfrą

f) stwórz wyrażenie regularne które spowoduje wybranie tylko imion Kasia i 
joasia

g) stwórz wyrażenie regularne które spowoduje wybranie elementów "ab 100" lub "accccb"
"""

x <- as.character(1:128)

wektor <- c("Kasia", "Małgosia", "Elżbieta", "joasia", "acccb", "ab 100")

x[grep("[[:alnum:]]", x)]

x[grep("[[:punct:]]", x)]

x[grep("[^[:digit:]]", x)]

wektor[grep("Kasia|joasia", wektor)]

wektor[grep("ab 100|acccb", wektor)]

"""Zadanie 2

a)wysymuluj 50 liczb całkowitych z przediału od 1 do 20 do wektóra " wektor_2 ",
początkowa wartość generatora liczb pseudolosowych powinna zostać ustalona na 123.

b)zaimportuj plik "zadanie_2" do obiektu "nazwa" kolumny powinny być zaimportowane jako tekst

c)nadaj nazwy kolejnym elementom wektora "wektor_2" na podstawie wektora "nazwa".

d)wyznacz sumy elementów należących do każdej grupy (zakresu), nie wykorzystując pętli

e)wyznacz liczbę elementów należących do każdej grupy (zakresu), nie wykorzystując pętli
"""

set.seed(123)
wektor_2 <- sample(1:20, 50, replace = TRUE)

nazwa <- read.table("zadanie_2.txt", header = FALSE, col.names = "nazwa")

names(wektor_2) <- nazwa$nazwa

suma <- tapply(wektor_2, cut(wektor_2, breaks = c(0, 5, 10, 15, 20)), sum)

liczba <- tapply(wektor_2, cut(wektor_2, breaks = c(0, 5, 10, 15, 20)), length)

"""Zadanie 3

A) stwórz funkcje (bez pętli) przyjmującą jako argument wejściowy obiekt x

  a)nadająca nazwy kolumn lub elementom, począwszy od x_1 do x_n 

  b) wyznacz sume elementów w każdej kolumnie lub sume elementów wektora

  c) zwraca obiekt wejściowy oraz obiekt stworzony w punkcie b).

B) przetestuj działanie funkcji dla

 b) wektorem o długości 100 o nazwie wektor (ziarno = 666) wypełnionych wartościami losowymi z rozkładu rownomiernego

a)macierzy rozmiaru 20x20 o nazwie macierz (ziarno = 666)wypełnionych wartościami losowymi z rozkładu rownomiernego
"""

nazwij_sumuj <- function(x) {
  
  # Nadaj nazwy kolumnom lub elementom
  nazwy <- paste0("x_", 1:length(x))
  
  # Wyznacz sumę elementów w każdej kolumnie (dla macierzy)
  # lub sumę elementów wektora (dla wektora)
  if (is.matrix(x)) {
    suma <- colSums(x)
  } else {
    suma <- sum(x)
  }
  
  # Zwróć obiekt wejściowy oraz obiekt z sumą
  list(x=x, suma=suma)
}

set.seed(666)
wektor <- runif(100)
nazwij_sumuj(wektor)

set.seed(666)
macierz <- matrix(runif(400), nrow=20, ncol=20)
nazwij_sumuj(macierz)

"""Zadanie 4 (potrzebujÄ™ pliku Zadanie_4)

A) zaimportuj plik "zadanie_4" do obiektu "obiekt_4"

B) Dodaj do obiektu nową kolumne "nowa" poprzez przetestowanie czy liczba w kolumnie "losowe"
jest mniejsza od 0,8,jeśli tak do nowa kolumna przyjmuje wartośc "mało" w przeciwnym "duzo"

C) stwórz listyę "lista_4" poprzez podział obiektu "obiekt_4" ze względu na kolumne "nowa"

D) Wykorzystując zwektorowana funkcje przejdz do każdego elementu listy i wyznacz do wszystkich
mozliwych kolumn sume elementów należących do grupy wyznaczonej przez kolumne "przemienne"
"""

obiekt_4 <- read.table("zadanie_4.txt", header = FALSE, col.names = "nazwa")

obiekt_4$nowa <- ifelse(obiekt_4$losowe < 0.8, "mało", "dużo")

lista_4 <- split(obiekt_4, obiekt_4$nowa)

lapply(lista_4, function(x) colSums(x[, c("kolumna_1", "kolumna_2", "kolumna_3")]))

"""Zadanie 2

A) stwórz wektor koleejnych liczb od 1 do 100.

B) Stworz macierz o wymiarze 50x10 Nazwij kolumny macierzy w następujący sposób : piersza = Y a każda następna od x_1 do x_n

c) Do kolumny pierwszej wstaw podzbiór elementów wektora z pkt a z indeksów od 21 do 70 za pomocą pętli wstaw do kolejnych kolumn i wierszy opóźnienia wektora z pkt. a, tak że x_1 jest opóźnione o jeden okres 
a x_1 o 10 okresów.
"""

vector <- 1:100

matrix <- matrix(0, nrow = 50, ncol = 10)
colnames(matrix) <- c("Y", paste("x", 1:9, sep = "_"))

matrix[,1] <- vector[21:70]

for (i in 2:ncol(matrix)) {
  matrix[,i] <- vector[21:70 - (i - 1) * 10]
}

"""Zadanie 3

a)Wysymuluj 30 liczb całkowitych z przedziału od 10 do 20 (ziarno = 444)

b) stwórz następnie poprzez prealokacje wektor liczb całkowitych o wartościach domyślinych o długości
wektora wyżej

c) za pomocą pętli for wstaw następnie elementy wektora losowaego z pkt a do wektora z pkt b
"""

set.seed(444)
random_vector <- sample(10:20, 30, replace = TRUE)

default_vector <- numeric(length(random_vector))

default_vector

for(i in 1:length(random_vector)) {
  default_vector[i] <- random_vector[i]
}

random_vector
default_vector

"""Zadanie 4

a) stwórz listę od długości 5 i wartościach domyślnych

b) stworz wektor poprzez wylosowanie licz od 1 do 10 5 razy bez powtórek (ziarno= 123)

c) wykorzystując funkcje bez pętli wstaw do kolejnych elementów listy wektor powtarzający konkrętną liczbę 
z wektora tyle razy jaka liczba jest zawarta w danym indeksie.
"""

lista_4 <- list(vector(mode = "numeric", length = 5),
                 vector(mode = "numeric", length = 5),
                 vector(mode = "numeric", length = 5),
                 vector(mode = "numeric", length = 5),
                 vector(mode = "numeric", length = 5))
lista_4

set.seed(123)
wektor_b <- sample(1:10, 5, replace = FALSE)
wektor_b

lista_4 <- Map(function(x, y) rep(y, x), 1:5, wektor_b)
lista_4

#
#
#
#
#
#
#
#
#
#  DRUGI TERMIN
# __________________________________________________________________________________________________________________________________________________________
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
Zadanie 1:

0) W zadaniu prosze wykorzystac nastepujacy wektor:

    c("Data: 01/25/2022, 12:10:44, kod pocztowy: 02-787, ulica: 'Zamiany', Pracownik: Jan Kowalski",
    "Data: 01/26/2022, 14:10:44, kod pocztowy: 02-776, ulica: 'Nowoursynowska', Pracownik: Kowalewska Aneta",
    "Data: 01/27/2022, 15:10:44, kod pocztowy: 02 759, ulica: 'Arbuzowa', Pracownik: Rafal Kowalczyk")

a) Zastap caly kod pocztowy wielka litera X.

b) Zastap nazwe ulicy wyrazeniem RODO.

c) Znajd elementy wektora, w ktorych nazwisko zawiera "Kow" oraz nazwisko nie jest koncem tekstu.

d) Przetworz zawarta w wektozre date na obiekt typu czas.
"""

vector <- c("Data: 01/25/2022, 12:10:44, kod pocztowy: 02-787, ulica: 'Zamiany', Pracownik: Jan Kowalski",
"Data: 01/26/2022, 14:10:44, kod pocztowy: 02-776, ulica: 'Nowoursynowska', Pracownik: Kowalewska Aneta",
"Data: 01/27/2022, 15:10:44, kod pocztowy: 02 759, ulica: 'Arbuzowa', Pracownik: Rafal Kowalczyk")

vector <- gsub("kod pocztowy: [0-9]+", "kod pocztowy: X", vector)

vector <- gsub("ulica: '[a-zA-Z]+'", "ulica: 'RODO'", vector)

result <- vector[grep("Pracownik: [A-Za-z]+ Kow", vector) & !grepl("Pracownik: [A-Za-z]+ Kow$", vector)]

vector_time <- as.POSIXct(gsub("Data: ", "", vector), format = "%m/%d/%Y, %H:%M:%S")
vector_time

"""Zadanie 2:

a) Wczytaj dwa pliki z kolumnowe "Zadanie_2a.txt" oraz "Zadanie_2b.txt".

b) Dokonaj zlaczenia tabel z pkt a) poprzez klucze podstawowe "klucz" oraz "key".
Tabela wynikowa powinna zawierac wszystkie wiersze z obydwu plikow (suma zbiorow).

c) Posortuj tabele wynikowa z pkt b) wzgledem klucza podstawowego malejaco.

d) Nie wykorzystujac petli, zastap braki danych w tabeli z pkt c) liczba 666.
"""

file1 <- read.table("Zadanie_2a.txt", header = TRUE, sep = ",")
file2 <- read.table("Zadanie_2b.txt", header = TRUE, sep = ",")

merged_table <- merge(file1, file2, by.x = "klucz", by.y = "key", all = TRUE)

sorted_table <- merged_table[order(merged_table$klucz, decreasing = TRUE),]

sorted_table[is.na(sorted_table)] <- 666

"""Zadanie 3:

a) Stworz wektor liczb od 1 do 1003 dodajac do niego wartosci losowe z rozkladu normalnego o sredniej 100 oraz o odchyleniu standardowym 10.

b) Podziel powyzszy szereg na 6 rownych odcinkow.

c) Stworz za pomoca petli for iterujacej w ramach wektora z pkt b) 6 wykresow liniowy stworzonych w jednym okienku podzielonym na 6 obszarow.

d) Kazdy wykres stworzony jest na podstawie odcinkow z pkt b) oraz pkt a).

e) Tytul wykresu powinien zmieniac sie dynamicznie od "Wykres_1" do "Wykres_6". Nazwa osi pionowej to "Szereg".
"""

set.seed(444)
vec <- 1:1003 + rnorm(1003, mean=100, sd=10)

splits <- split(vec, cut(seq_along(vec), 6, labels = paste0("Wykres_", 1:6)))

par(mfrow = c(2,3))
for (i in 1:6) {
  plot(splits[[i]], type = "l", main = paste0("Wykres_", i), xlab = "Index", ylab = "Szereg")
}

"""Zadania 4:

a) Stworz 5-elementowa lista. Kazdy z wektorow zawiera 10 wartosci wylosowanych z rozkladu rownomiernego (nie uzywajac petli).

b) Przeksztalc liste z pkt a) na macierz o wymiarach "10x5", poprzez polaczenie kazdego wektora kolumnowo.

c) Nie uzywajac petli, przetestuj wszystkie wartosci w tabeli z pkt. b) sprawdzajac czy sa one mniejsze lub rowne 0.5,
jezeli tak, to nowa wartosc wskazuje "mniej0.5", w przeciwnym wypadku "wiecej0.5".

d) Na podstawie wyniku z pkt c) nie wykorzystujac petli zlicz liczbe wystapien dwoch kategorii.
"""

lista <- list(runif(10), runif(10), runif(10), runif(10), runif(10))

macierz <- t(do.call(cbind, lista))

macierz[macierz <= 0.5] <- "mniej0.5"
macierz[macierz > 0.5] <- "wiecej0.5"

tablica_wystapien <- table(macierz)

tablica_wystapien