# adresy <- c("Kasztanowa 14, Bielsko Biała", "Łąki Zielonej 152, Katowice", "Specerowa 8, Elbląg")
# produkty <- c("TV ", "laptop", " przenośna ładowarka", "Bezprzewodowa klawiatura", " SłuchaWki " )
# dluga_sentencja <- stringr::sentences[1:10]
# nazwa_pola <- c( "order_number", "order_date", "customer_email", "product_ title", "amount" )
# pracownik_umiejetnosci <- c("Jan Kowalski (Beginner)", "Marta Górska (Pro)", "Krzysztof Biały (Pro)", "Sarah Machoń (Medium)")
# 
# Zadanie 1: Normalizuj wektor adresów, zastępując wielkie litery małymi literami.
# Zadanie 2: Wybierz tylko liczbową część wektora adresów.
# Zadanie 3: Podziel wektor adresów na dwie części: adres i miasto. Wynik powinien być macierzą.
# Zadanie 4: Podziel wektor adresów na trzy części: numer domu, ulicę i miasto. Wynik powinien być macierzą. 
#            Podpowiedź: użyj asercji z wyrażeniem w tył regex.
# Zadanie 5: W wektorze dluga_sentencja dla zdań zaczynających się na literę „T” lub kończących się na literę „s” pokaż odpowiednio pierwsze lub ostatnie słowo. 
#            Jeśli zdanie zarówno zaczyna się na „T”, jak i kończy na „s”, pokaż zarówno pierwsze, jak i ostatnie słowo. 
#            Pamiętaj, że faktyczny ostatni znak zdania to zwykle kropka.
# Zadanie 6: Pokaż tylko pierwszych 20 znaków wszystkich zdań w wektorze dluga_sentencja Aby wskazać, że usunąłeś niektóre znaki, użyj dwóch kolejnych kropek na końcu każdego zdania.
# Zadanie 7: Znormalizuj wektor produkty, usuwając wszystkie niepotrzebne spacje (zarówno na początku, na końcu, jak i w środku) oraz używając wielkich liter.
# Zadanie 8: Przygotuj wektor nazwa_pola, zastępując wszystkie symbole podkreśleń spacjami i konwertując je na wielkie litery.
# Zadanie 9: Wyrównaj wszystkie nazwa_pola, aby miały równą długość, dodając spacje na początku odpowiednich ciągów.
# Zadanie 10: W wektorze pracownik_umiejetnosci poszukaj pracowników, którzy są zdefiniowani jako „Pro” lub „Medium”. 
#             Wynikiem powinna być macierz, która zawiera imię i nazwisko pracownika w pierwszej kolumnie, a poziom umiejętności (bez nawiasów) w drugiej kolumnie. 
#             Pracownicy, którzy nie są zakwalifikowani, powinni otrzymać brakujące wartości w obu kolumnach.
# Zadanie 11:
# a) Stworz wektor x zawierajacy wszystkie znaki odpowiadajace wartosciom liczbowym z zakresu 1:128.
# b) Wyswietl wszystkie znaki z wektora x, ktore odpowiadaja liczbom lub literom malym lub wielkim.
# c) Wyswietl wszystkie znaki z wektora x, ktore odpowiadaja liczbom lub znakom oddzielajacym tekst tj. spacja, tabulator etc.
# b) Wyswietl wszystkie znaki z wektora x, ktore nie odpowiadaja liczbom.
# Zadanie 12:
# a) Stworz wektor y zawierajacy nastepujace imiona: ("Jacek", "Kasia", "Małgosia","Elżbieta","joasia").
# b) Stworz wyrazenie regularne, ktore spowoduje wybranie tylko imion: "Kasia","joasia".
# c) Stworz wyrazenie regularne, ktore spowoduje wybranie tylko imion: "Jacek", "Kasia".
# Zadanie 13:
# a) Stwotrz obiekt "dane" na podstawie nastepujacego wektora: c("1", "1 2", "12a3", "1223", "122234", "212234","1 22 3").
# b) Stworz wyrazenie regularne znajdujace elementy wektora "dane", konczace sie na "2" lub "3".
# c) Stworz wyrazenie regularne znajdujace elementy wektora "dane", posiadajace od jedej do trzech "2" nie bedacych jednoczesnie poczatkiem ani koncem tekstu.
# d) Stworz wyrazenie regularne znajdujace elementy wektora "dane", posiadajace male litery lub spacje.
# Zadanie_14:
# a) Stworz macierz rozmiaru "10X10" z wartosciami losowymi z rozkladu rownomiernego.
# b) Stworz funkcje "Funkcja" ktora zwracac bedzie do obiektu "wynik_14" wartosc srednia dla kazdej kolumny macierzy wejsciowej.
# c) Stworz "Funkcja_env" ktora bedzie "srodowiskowa" wersja powyzszej funkcji. 
#    Powinna ona nie zwracac zadnego obiektu, lecz umieszczac wynikowy obiekt w srodowisku, ktore bedzie obiektem wejsciowym dla funkcji. 
# Zadanie_2:
# a) Stworz srodowisko "Pierwsze".
# b) Umiesc w srodowisku "Pierwsze" obiekt "x" przyjmujacy wartosc 10.
# c) Stworz srodowisko "Drugie" w srodowisku "Pierwsze". (Po wyswietleniu obiektow znajdujacych sie w srodowisku "Pierwsze", srodowisko "Drugie" powinno byc tam widoczne)
# d) Umiesc w srodowisku "Drugie" obiekt "y" przyjmujacy wartosc 20.
# e) Stworz funkcje zamieniajaca wartosc obiektu "x" w srodowisku "Pierwsze" na warotsc z obiektu "y" ze srodowiska "Drugie". Funkcja przyjmuje jako argument srodowisko.
