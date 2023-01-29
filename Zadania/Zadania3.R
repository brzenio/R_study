# We wszystkich ponizszych zadaniach stworz wlasne obiekty, tj. nazwane wektory, niepuste wektory i listy.
# 
# Zadanie 1: Napisz funkcje, ktora oblicza liczbe a do potęgi b, niech b ma domyślną wartość 2.
# Zadanie 2: Napisz funkcje, ktora zmienia wartość pi w globalnym środowisku R na dowolną wartość określoną jako argument.
# Zadanie 3: Napisz funkcję, ktara sprawdza, czy wartosci z pierwszego wektora znajduja się w zakresie między maksimum a minimum drugiego wektora.
# Zadanie 4: Napisz funkcje zliczajaca liczbe brakow danych w danym wektorze.
# Zadanie 5: Napisz funkcje konwertujaca wartosci w danym wektorze typu NA, NaN, INF, na jego wartosc srednia.
# Zadanie 6: Napisz funkcje, ktora wyswietli w konsoli informacje o liczbie elementow w danym wekotrze oraz w linijce ponizej informacje o typie wektora - prosze wykorzystac tylko raz funkcje do wyswietlania informacji.
# Zadanie 7: Napisz funkcje wyznaczajaca wartosc srednia oraz mediane, pomijajac wartosci minimalna i maksymalna w danym wektorze. Funkcja nie powinna zwracac niczego oraz powinna wyswietlac w konsoli tresc typu "Srednia to: xxx, mediana to: xxx"
# Zadanie 8: Napisz funkcje, ktora wyznaczy dla danego wektora wejsciowego x, pewna statystyke opisowa np. srednia, min, max - owa statystyka powinna byc przekazana do fukcji jako prametr. Fukcja powinna wyswietlac w konsoli tresc "Wartosc [Nazwa przekazanej funkcji] to: xxx".
# Zadanie 9:
# a) Stworz funkcje wskazujaca obserwacje odstajace z podanego wektora liczb rzeczywistych (x <-rcauchy(10)) wykorzystujaca regule kwartylow: Q1-1.5*(Q3-Q1) Q1+1.5*(Q3-Q1).
# b) Funkcja powinna zwracac obiekt z podanymi atrybutami zawierajacymi informacje:
#   a. jakie wartosci sa odstajace
#   b. jakie wartosci sa nieodstajace
#   c. indeksy wartosci odstajacych
#   d. indeksy wartosci nieodstajacych
# c) (wykorzystaj funkcje quantile, which, attr).
# 
# Zadanie 10: 
# a) Stworz dwa wektory np. Nie_odstajace oraz Odstajace, ktore beda przechowywac wyniki zwracane przez funkcje stworzona w poprzednim kroku.

# Zadanie 11: 
# a) Wysymuluj 30 liczb calkowitych z przedzialu od 10 do 20. Poczatkowa wartosc generatora liczb pseudolosowych powinna zostac ustalona na 444.
# b) Stworz nastepnie poprzez prealokacje wektor liczb calkowitych o wartosciach domyslnych. 
#    Dlugosc tego wektora ustalona jest jako dlugosc wysymulowanego wektora z punktu powyzej.
# c) Za pomoca petli for wstaw nastepnie elementy wektora losowego (pkt. a) do wektora z pkt. b. 
#    Petla powinna byc wykonana w dwoch wersjach: operujac na indeksach obydwu wektorow oraz na rzeczywistych wartosciach jednego z nich. W ostatnim kroku kazda petla powinna wyswietlac (nie uzywajac funckji print()) caly wektor (pkt. b) oraz informacje o jego typie (wiersz ponizej). 
#
# Zadanie 12:
# a) Stworz wektor zawierajacy liczby od 1 do 9,
# b) Stworz liste o dlugosci odpowiadajacej dlugosci wektora z pkt. a.
# c) Za pomoca petli for iterujacej w ramch wartosci zawartych w wektorze z pkt. a, wstaw w kolejne elementy listy podzbiór wektora z pkt a, tak aby
#   pierwszy element listy zawieral pierwszy element, drugi element listy zawieral pierwszy oraz drugi element,..., 
#   dziewiaty element listy zawieral wszystkie elementy. Stworzy sie swojego rodzaju piramida.
#  
# Zadanie 13:
# a) Wykonaj powyzsze zadanie nie wykorzystujac petli.
