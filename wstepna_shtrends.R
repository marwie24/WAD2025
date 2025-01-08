#wstępna analiza
library(tidyverse)
library(moments)
library(Hmisc)

data <- read.csv("C:/Users/MSI/Desktop/shopping_trends.csv")
data_w <- data %>%
  filter(Gender=="Female")
data_m <- data %>%
  filter(Gender=="Male")

summary(data)
summary(data_m)
summary(data_w)
stats <- describe(data)

#Baza zawiera dane o 3900 klientach
#Wszyscy znajdują się w wieku 18-70
#Znacząco więcej mężczyzn niż kobiet, 2652 do 1248
#Średni wydatek na zakup: $60
#Ceny kupowanych produktów w zakresie $20-$100
#Oceny produktów w skali 1-5, z zebranych danych oceniane od 2.5 do 5.0
#Osób z subskrypcją: 1053


data_s <- data %>%
  filter(Subscription.Status=="Yes")
data_ns <- data %>%
  filter(Subscription.Status=="No")

#Metody płatności:
data_p <- data %>% 
  group_by(Payment.Method) %>% 
  summarise(total_count=n()) %>%
  ungroup()
data_p1 <- data %>% 
  group_by(Preferred.Payment.Method) %>% 
  summarise(total_count=n()) %>%
  ungroup()
data_p2 <- data %>% 
  group_by(Payment.Method,Preferred.Payment.Method) %>% 
  summarise(total_count=n()) %>%
  ungroup()

#Metody dostawy: 
data_d <- data %>% 
  group_by(Shipping.Type) %>% 
  summarise(total_count=n()) %>%
  ungroup()

#Częstotliwość zakupów:
data_f <- data %>% 
  group_by(Frequency.of.Purchases) %>% 
  summarise(total_count=n()) %>%
  ungroup()

#Wcześniejszych zakupów:
#Od 1 do 50
data_prev <- data %>% 
  group_by(Previous.Purchases) %>% 
  summarise(total_count=n()) %>%
  ungroup()

#Kategorie produktu:
data_c <- data %>% 
  group_by(Category,Gender) %>% 
  summarise(total_count=n()) %>%
  ungroup()


data1 <- data %>% 
  group_by(Age,Gender) %>% 
  summarise(suma_USD=sum(Purchase.Amount..USD.)) %>%
  ungroup()
#co ciekawe najwięcej produktów we wszystkich kategoriach kupują mężczyźni
data1 %>% 
  ggplot(aes(x=Age, y=suma_USD, colour=Gender))+
  geom_point()+
  geom_smooth(method="lm")

# to samo wychodzi tu: w pierwszej 50-tce klientów, którzy wydali najwięcej nie ma ani jednej kobiety
# pierwsza kobieta pojawia się dopiero na 52 miejscu

data2 <- data %>% 
  group_by(Age,Gender) %>% 
  summarise(suma_poprzednie=sum(Previous.Purchases)) %>%
  ungroup()

data2 %>% 
  ggplot(aes(x=Age, y=suma_poprzednie, colour=Gender))+
  geom_point()+
  geom_smooth(method="lm")


#Przykładowe pytania które mogą nas interesować
#1. Jak można porównać to, ile wydają klienci w stosunku do częstotliwości ich zakupów?
#2. od czego może zależeć metoda płatności? cena? częstotliwość zakupu?
#3. Kto/dlaczego decyduje się na subskrybcję?
#4. Jak wygląda rynek dla poszczególnych kategorii?
#5. Czy osoby w różnym wieku różnie dokonują zakupów? A może metod płatności?
#6 Pominęłabym w tym wszystkim Lokalizację, bo raczej średnio nam to wpłynie na cokolwiek 
#+ nie bardzo raczej znamy się na różnicach gospodarczych, ekonomicznych i społecznych w regionach Ameryki xd 
#7. Raczej też mało istotny wydaje mi się rozmiar tutaj
#8. Ciekawe może być porównanie dominujących kolorów dla danych grup, albo/i w poszczególnych porach roku
#9. Plus to w jakim okresie kto więcej wydaje? może z okazji jakichś świąt? Może na walentynki?

summary(data)

# rozkład wieku
ggplot(data, aes(x = Age)) +
  geom_histogram(bins = 53, fill = "#009999", color = "black") +
  geom_density(aes(y = ..count..), color = "#660099", size = 1) +
  labs(
    title = "Rozkład wieku klientów",
    x = "Wiek",
    y = "Liczba klientów"
  ) +
  theme_minimal()

# rozkład wydanych kwot

ggplot(data, aes(x = Purchase.Amount..USD.)) +
  geom_histogram(bins = 81, fill = "#666600", color = "black") +
  geom_density(aes(y = ..count..), color = "#990033", size = 1) +
  labs(
    title = "Rozkład kwot",
    x = "Kwota",
    y = "Liczba wystąpień"
  ) +
  theme_minimal()


# rozkład płci
ggplot(data, aes(x = Gender, fill = Gender)) +
  geom_bar(color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "black", size = 5) +
  labs(
    title = "Liczba klientów według płci",
    x = "Płeć",
    y = "Liczba klientów"
  ) +
  theme_minimal()

# kategorie
ggplot(data, aes(x = Category, fill = Category)) +
  geom_bar(color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "black", size = 5) +
  labs(
    title = "Liczba zakupów w danej kategorii",
    x = "Kategoria",
    y = "Liczba zakupów"
  ) +
  theme_minimal()


# rozkład kolorów - Ania

# rozkład zakupów w porach roku
ggplot(data, aes(x = Season, fill = Season)) +
  geom_bar(color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "black", size = 5) +
  labs(
    title = "Liczba zakupów w różnych porach roku",
    x = "Pora roku",
    y = "Liczba zakupów"
  ) +
  theme_minimal()+
  scale_fill_manual(values = c("#FF6666", "#66CC66", "#FFCC00", "#0099CC" ))

#rozkład ocen
ggplot(data, aes(x = Review.Rating)) +
  geom_histogram(bins = 26, fill = "#009919", color = "black") +
  labs(
    title = "Rozkład ocen",
    x = "Ocena",
    y = "Liczba ocen"
  ) +
  theme_minimal()

# częstośc zakupów
data$Frequency.of.Purchases <- factor(
  data$Frequency.of.Purchases,
  levels = c("Bi-Weekly","Weekly", "Fortnightly", "Monthly","Every 3 Months","Quarterly", "Annually")
)

ggplot(data, aes(x = Frequency.of.Purchases, fill = Frequency.of.Purchases)) +
  geom_bar(color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "black", size = 5) +
  labs(
    title = "Częstość zakupów",
    x = "Częstotliwość zakupów",
    y = "Liczba zakupów"
  ) +
  theme_minimal()

# age vs amount
ggplot(data, aes(x=Age, y=Purchase.Amount..USD.)) + 
  geom_point(
    fil=  "#666600",
    color = "#555500"
  )+
  labs(
    title = "Wydawana kwota a wiek",
    x = "Wiek",
    y = "Wydana kwota"
    
  )

#boxbox

data$Age.Group <- cut(
  data$Age,
  breaks = c(18, 29, 39, 49, 59, 70),
  labels = c("18-29", "30-39", "40-49", "50-59", "60-70"),
  include.lowest = TRUE
)

# Tworzenie boxplotu
library(ggplot2)

ggplot(data, aes(x = Age.Group, y = Purchase.Amount..USD., fill = Age.Group)) +
  geom_boxplot() +
  labs(
    title = "Rozkład kwoty zakupu dla grup wiekowych",
    x = "Grupy wiekowe",
    y = "Kwota zakupu"
  ) +
  theme_minimal()

# wpływ zniżki na średnią kwotę zakupów
# ile zniżek ile bez zniżek?
#Może to oznaczać, że zniżki nie są wystarczająco atrakcyjne, aby zwiększać wydatki.
#Może być sygnałem do przeanalizowania strategii rabatowej. Zniżki powinny motywować klientów do zwiększania wydatków, a nie jedynie zmniejszać marżę.
srednia_kwota <- data %>%
  group_by(Discount.Applied) %>%
    summarise(Mean_Purchase_Amount = mean(Purchase.Amount..USD., na.rm = TRUE))

ggplot(srednia_kwota, aes(x = Discount.Applied, y = Mean_Purchase_Amount, fill = Discount.Applied)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(Mean_Purchase_Amount, 2)), vjust = -0.5, size = 5) +
  labs(
    title = "Średnia kwota zakupów z rabatem i bez",
    x = "Rabat",
    y = "Średnia kwota zakupu"
  ) +
  theme_minimal()

# Czy liczba transakcji wzrasta z rabatami? Nie wiadomo, nie możemy śledzić poszczególnych klientów
# Jak rozkładają się kwoty w obu grupach? głupie pytanie bo bez zniżek było po prostu więcej zakupów
# można zostawić wykres i zapytać grupe czm to jest bez sensu? pytanie interaktywne

discount_yes <- data$Purchase.Amount[data$Discount == "Yes"]
discount_no <- data$Purchase.Amount[data$Discount == "No"]

# Ustawienie przezroczystości kolorów za pomocą rgb
color_yes <- rgb(0.2, 0.4, 0.6, 0.5) # Niebieski, 50% przezroczystości
color_no <- rgb(0.8, 0.2, 0.2, 0.5)  # Czerwony, 50% przezroczystości

# Tworzenie histogramów
hist_yes <- hist(discount_yes, breaks = 20, plot = FALSE)  # Histogram dla zniżek
hist_no <- hist(discount_no, breaks = 20, plot = FALSE)    # Histogram bez zniżek

# Rysowanie histogramów na jednym wykresie
plot(hist_yes, col = color_yes, xlim = c(0, max(data$Purchase.Amount, na.rm = TRUE)), 
     main = "Porównanie rozkładu kwot zakupów", xlab = "Kwota zakupu", ylab = "Częstość", border = "black")
plot(hist_no, col = color_no, add = TRUE, border = "black")

# Dodanie legendy
legend("bottomleft", legend = c("Z rabatem", "Bez rabatu"), 
       fill = c(color_yes, color_no), border = "black")

#wplyw kodu promocyjnego na średnią kwotę zakupów
srednia_kwota2 <- data %>%
  group_by(Promo.Code.Used) %>%
  summarise(Mean_Purchase_Amount = mean(Purchase.Amount..USD., na.rm = TRUE))

ggplot(srednia_kwota2, aes(x = Promo.Code.Used, y = Mean_Purchase_Amount, fill = Promo.Code.Used)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(Mean_Purchase_Amount, 2)), vjust = -0.5, size = 5) +
  labs(
    title = "Średnia kwota zakupów z kodem rabatowym i bez",
    x = "Kod rabatowy",
    y = "Średnia kwota zakupu"
  ) +
  theme_minimal()
 #Pora roku a ilość dokonanych zakupów
    data_season1 <- data %>% 
      group_by(Season) %>%  
      summarise(total_count = n()) %>%  
      ungroup() 
                                  #najwięcej zakupów klienci robią: wiosną

#Pora roku a ilość wydanych przez klientów pieniędzy
    data_season2 <- data %>% 
      group_by(Season) %>%  
      summarise(total_spent = sum(Purchase.Amount..USD.)) %>%  
      ungroup() 
                                  #najwięcej klienci wydają jesienią


#wykresy 'wiek klientów wg metod płatności'

    #wykres boxplot
ggplot(data, aes(x = Payment.Method, y = Age)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Wiek klientów wg metody płatności",
       x = "Metoda płatności",
       y = "Wiek") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

    #Wykres słupkowy
ggplot(data, aes(x = Payment.Method, fill = cut(Age, breaks = c(0, 20, 30, 40, 50, 60, 70)))) +
  geom_bar(width = 0.6) +
  labs(title = "Liczba klientów wg metody płatności i przedziału wiekowego",
       x = "Metoda płatności",
       y = "Liczba klientów",
       fill = "Przedział wiekowy") +
  theme_void() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))


  #wykres 'oceny zakupów a status subskrypcji'

ggplot(data, aes(x = Subscription.Status, y = Review.Rating, fill = Subscription.Status)) +
  geom_boxplot() +
  labs(title = "Ocena zakupów a statusu subskrypcji",
       x = "Status subskrypcji",
       y = "Ocena zakupów (1-5)") +
  theme_minimal() +
  scale_fill_manual(values = c("#66C2A5", "#FC8D62"))
    #średnio subskrybenci wystawiają wyższe oceny.







