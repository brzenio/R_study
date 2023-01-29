# Srodowiska
x <- 1:10

ls()
ls(envir = .GlobalEnv)

environment()

lokalna <- function(){
  print( environment() )
  print( ls() )
  print( ls( envir = .GlobalEnv ) )
}
lokalna()

srod1 <- new.env()
srod1
typeof( srod1 )
class( srod1 )
str( srod1 ) 

assign( "z", 666 )
z
rm( z )

assign( "z", 666, envir = srod1 )
ls()
ls( envir = srod1 )

srod1[["z"]]
srod1$z

get( x = "z" )
get( x = "z", envir = srod1 )

l1 <- list( w = 1 )
s1 <- as.environment( l1 )
l1$w
s1$w

dodaj <- function( x ){
  cat("Przed zmiana", x$w ,"\n")
  x$w <- x$w + 1 
  cat("Po zmianie", x$w ,"\n")
}

l1$w
dodaj(l1)
l1$w

s1$w
dodaj(s1)
s1$w

wektor <- c(1,1)
lokalna_globalna <- function( x ){
  x[1] <- 666
  # wektor[1] <- 666
  wektor[1] <<- 666 # super assigment
  assign( "globalny", 999, envir = .GlobalEnv )
  assign( "lokalnysrod1", 999, envir = srod1 )
}

lokalna_globalna( wektor )
wektor
ls()

globalny
srod1$lokalnysrod1

# Przetwarzanie tekstu
tekst <- "Przetwarzanie danych w R"

length( tekst )
nchar( tekst ) 

charToRaw( tekst )
as.integer( charToRaw( tekst ) )

#UTF-8
"łąka"

tolower( tekst )
toupper( tekst )

rawToChar( as.raw( 1:127 ) )

cbind( Liczby = 1:127, Znaki = rawToChar( as.raw( 1:127 ), multiple = T ) )

for( i in 1:10 ){
  
  cat( sprintf( "To jest liczba %d", i ), "\n")
  # %d - integer, %5.2f - double, %s - character
  assign(x = sprintf( "x_%d", i ), value = 100 + i )
  
}

substr( x = tekst, start = 1, stop = 13 )
strsplit( x = tekst, split = " " )

sub( pattern = "w", replacement = "_w_", x = tekst )
gsub( pattern = "w", replacement = "_w_", x = tekst )

regexpr( pattern =  " ", text = tekst )
gregexpr( pattern =  " ", text = tekst )

# Kwantyfikatory
wyrazenie <- c("a","ab","acb","accb","acccb","accccb")

grep( pattern = "ab", x = wyrazenie, value = T )

# * - co najmniej 0 znakow
grep( pattern = "ac*b", x = wyrazenie, value = T )

# + - co najmniej 1 znak
grep( pattern = "ac+b", x = wyrazenie, value = T )

# ? - co najwyzej 1 znak
grep( pattern = "ac?b", x = wyrazenie, value = T )

# {n,m} - co najmniej n znakow, i co najwyzej m znak
grep( pattern = "ac{2,3}b", x = wyrazenie, value = T )

# Pozycja w tekscie
wyrazenie <- c("abcd","cdab","cadb","c abd")

# ^ - poczatek tekstu
grep( pattern = "ab", x = wyrazenie, value = T )
grep( pattern = "^ab", x = wyrazenie, value = T )

# $ - koniec tekstu
grep( pattern = "ab$", x = wyrazenie, value = T )

# Klasy znakow specjalnych
# [::]
# [:digit:] - cyfry
# [:lower:] - litery male
# [:upper:] - litery wielkie
# [:punct:] - znaki interpunkcyjne
# [:blank:] - biale znaki

# [] - definicja znakow, ktore moga pojawic sie w danym miejscu
# [ [:digit:] [:lower:] ] 
# [^] - definicja znakow, ktore nie moga pojawic sie w danym miejscu
# | - laczenie klas znakow

wyrazenie <- c("^ab","ab","abc","abd","abe","ab 100")

grep( pattern = "ab[cde]", x = wyrazenie, value = T )
grep( pattern = "ab[c-e]", x = wyrazenie, value = T )
grep( pattern = "abc|abd", x = wyrazenie, value = T )
grep( pattern = "[[:digit:]]", x = wyrazenie, value = T )

