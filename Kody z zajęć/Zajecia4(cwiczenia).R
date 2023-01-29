# Tabele
# Matrix
x <- 1:6
x

attributes( x )
dim( x ) <- c( 3, 2 )
dim( x )
attributes( x )
x

x[1,2]
x[1:2,1]

str(x)
str(x[1:2,1])

x[1:2, 1, drop = T]
x[1:2, 1, drop = F]

colnames( x ) <- c("c1","c2")
x

rownames( x ) <- c("r1","r2","r2")
x

x[3,2]
x[3,"c2"]
x["r2","c2"] # pierwsze wystąpnie

x <- cbind( x, c3 = 10 ) #regula zwijania
x

x <- rbind( x, 20 )
x

apply(X = x, MARGIN = 1, FUN = sum)# suma po wierszach
apply(X = x, MARGIN = 2, FUN = sum)# suma po kolumnach

# Ramka danych
plec <- factor( c("K","M","K","K","M") )
wzrost <- c(200,160,180,190,165)

ramka_1 <- data.frame( klucz = 1:5, wzrost, plec )
ramka_1
typeof( ramka_1 )
str( ramka_1 )

as.matrix( ramka_1 )

ramka_1[order(ramka_1$plec),]
ramka_1[order(ramka_1$plec,-ramka_1$wzrost),]

ramka_2 <- data.frame( key = 1:5, waga = c(80,90,100,75,60), plec )
ramka_2

ramka_merge <- merge(x = ramka_1, y = ramka_2, by.x = "klucz", by.y = "key" )# iloczyn zbiorów
ramka_merge

aggregate( x = ramka_merge[,c("wzrost","waga")], by = list(ramka_merge$plec.x), FUN = mean )

