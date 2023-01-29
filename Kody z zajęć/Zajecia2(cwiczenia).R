2 + 4;

3+6; 6-8

2 + 
  9
2 / 
  9

# komentarz

2+7 #komentarz# 5+3
# /*
#   komentarz
#   */

#### Typy danych
### atomowy
## logical
## numeric
# integer
# double
## complex
## character
##  *special

### złozony
## matrix
## array
## data.frame
## time.series
## factor

### rekurencyjny
## list
## function

typeof() # zwraca rodzaj danych
class() # klasa obiektu

## logical
TRUE; FALSE
T; F

true # wielkość liter ma znaczenie

c() #combine - łączenie elementów w wektor
c( TRUE, F, T, FALSE )
typeof( c( TRUE, F, T, FALSE ) )
class( c( TRUE, F, T, FALSE ) )

length( c( TRUE, F, T, FALSE ) ) # liczba elementów

rep() # repeat - powtrzanie elementów wektora 
?rep
rep

rep( x = c(T,F), each = 2 )
rep( x = c(T,F), times = 3 )
rep( x = c(T,F), length.out = 9 )

## numeric
# integer
1
typeof(1) # domyślnie double
class(1)

typeof( 1L ) # konwersja 

1:10 # sekwencja liczb z krokiem 1
seq( from = 1, to = 10, by = 1) # sequence 
seq( from = 1, to = 10, by = 2) 

-5:1
10:-7
seq( from = 0, to = 1, length.out = 5 ) 
seq( from = 0, to = 1, length.out = 100 ) 

# dobule
typeof( 3.14 )

( 0.1 + 0.1 ) == 0.2
( 0.1 + 0.1 + 0.1 ) == 0.3

print( 0.1, digits = 22 )
print( 0.3, digits = 22 )
print( ( 0.1 + 0.1 + 0.1 ), digits = 22 )

## complex 
typeof( 4+7i )
class( 4+7i )

# character
"napis"
'napis'
typeof( "napis" )
class( "napis" )

length( "napis" ) # liczba elementów
length( c("napis",'napis') )

nchar( "napis" ) # liczba znaków

print( "To jest pierwszy wiersz \n a to jest drugi wiersz" )
# wyświetla tekst takim jaki jest

cat( "To jest pierwszy wiersz \n a to jest drugi wiersz" )
# interpretuje znaki specjalne

## special 
NA # Not Available
typeof( NA )
class( NA )

NaN # Not a Number
sqrt(-2)
typeof( NaN )
class( NaN )

Inf; -Inf
typeof( Inf )
class( Inf )

NULL # obiekt pusty
typeof( NULL )
class( NULL )

### Rzutowanie typów danych (koercja)
# logical < integer < double < complex < character

c(T)
typeof(c(T))

c(T, 1L)
typeof(c(T, 1L))

c(T, 1L, 3.14)
typeof(c(T, 1L, 3.14))

c(T, 1L, 3.14, 5+7i)
typeof(c(T, 1L, 3.14, 5+7i))

c(T, 1L, 3.14, 5+7i, "napis")
typeof(c(T, 1L, 3.14, 5+7i, "napis"))

# Wymuszone rzutowanie i testowanie wektorów
is.logical( 5L )
is.logical( T )

as.logical( c(5L,0,3.14) )
as.character( c(5L,0,3.14) )

# Alokacja wektorów
vector("double", 10) # generyczna funkcja tworząca obiekt o wartościach domyślnych
vector("character", 10)

double(5)
logical(5)

# Tworzenie obiektów
x = 1:10
x

x <- 1:10
x
1:10 -> x
x

( x <- 1:10 )# automatyczne wyświetlanie obiektu w terminalu

assign( x = "x", value = 1:10, envir = .GlobalEnv )

x <- 4; y <- 4; z <- 4

x <- y <- z <- 4

# Operacje na wektorach
# arytmetyczne: +,-,*,?,^
# logiczne: <, >, <=, >=, !, !=
# koniunkcja & (&&), alternatywa | (||)
# Powyższe operatory są zwektoryzowane
x <- 1; y <- 11
x + y

x <- 1:2; y <- 11:12
x + y

x <- 1:4; y <- 11:12 #reguła zwijania, duplikowanie elementów
x + y
c(1,2,3,4) + c(11,12,11,12)

x <- 1:4; y <- 11:13
x + y
c(1,2,3,4) + c(11,12,13,11)

# Indeksowanie wektorów
x <- 11:20
x

x[ 1 ]
x[ 2:5 ]
x[ c(1,4:6,length(x)) ]

x[ c(8, rep(2,4), 1, 5 ) ]

x[ -6 ]
x[ -c(5,8,9) ] # elementy, których nie chcemy

x[ c(T,F) ] # wartości musi być tyle ile elementów ma wektor wejściowy

x[ c(c(T,F),c(T,F),c(T,F),c(T,F),c(T,F)) ] 
x[ rep(c(T,F),5) ] 

set.seed( 666 )
wektor <- sample( x = 1:1000, size = 10, replace = F )
wektor

wektor > 500
wektor > 300 & wektor < 600
wektor > 300 && wektor < 600

wektor[ wektor > 300 & wektor < 600 ]

# Sortowanie wektorów
sort( x = wektor )# niemalejąco
sort( x = wektor, decreasing = T )# nierosnąco

wektor
order( wektor ) #permutacja elementów, wymagana kolejnosć ustawienia
wektor[ order( wektor ) ]

# Łączenie wektorów (tekst)
paste( "x", 1:10 )#reguła zwijania
paste( "x", 1:10, sep = "_" )
paste( "x", 1:10, sep = "_", collapse = " + " )

letters
LETTERS

## Listy
y <- c( double = 3.14, character = "napis" )
y
str(y)
typeof(y)

lista <- list( double = 3.14, character = "napis", funkcja = mean )
lista
str( lista )
typeof( lista )

lista1 <- list( elem1 = 1:10, 
                11:20,
                elem3 = list( el1 = 2,
                              10) )
lista1[ 1 ]
lista1[ 1 ] + 1
typeof( lista1[ 1 ] )

lista1[[ 1 ]]
lista1[[ 1 ]] + 1
typeof( lista1[[ 1 ]] )

lista1$elem1

lista1$elem3
lista1$elem3$el1
lista1[[ 3 ]][[ 2 ]]
lista1[[ 3 ]]$el1

sum( lista1[[1]] )

lista_suma <- lapply(X = lista1[1:2], FUN = sum )
lapply(X = lista1[1:2], FUN = function(x){ sum(x) } )

do.call( what = "c", args = lista_suma )
do.call( what = "rbind", args = lista_suma )
