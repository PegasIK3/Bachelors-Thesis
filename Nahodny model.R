#install.packages("dplyr")
library(dplyr)
library(rmarkdown)
cardsbase <- as.data.frame(read.csv2("Used Cards.csv"))
cardsbase <- cardsbase[sample(nrow(cardsbase), 39), ]       ### nacitani karet
cards <- list()


for(i in 1:nrow(cardsbase)){                                    ### vytvorim vsechny mozne karty
  if (cardsbase$Max[i] == 2){
    cards <- rbind(cards, cardsbase[i, ])
    cards <- rbind(cards, cardsbase[i, ])
  } else {
    cards <- rbind(cards, cardsbase[i, ])
  }
}        
rownames(cards) <- seq(1:nrow(cards))
cards$ID <- as.integer(rownames(cards))

seznamdeathratlu <- unique(as.vector(filter(cards, Deathrattle == 1)$Karta)) ### seznam karet s deathrattlem
seznamendturn <- c("Hogger", "Young Priestess", "Master Swordsmith", "Gruul", "Imp Master")
seznamstartturn <- unique(as.vector(filter(cards, regexpr("At the start of your turn*?", cards$Effect.Usual)>0)$Karta))
seznambcdrawcard <- c("Novice Engineer", "Gnomish Inventor", "Azure Drake")

############################

deck.generate <- function(cards){                               ### funkce generujici karti
  deck_g <- list()
  mozne_karty <- c(1:nrow(cards))  
  for (i in 1:30){
    id <- sample(mozne_karty, 1)
    deck_g <- rbind(deck_g, cards[id, ])
    mozne_karty <- mozne_karty[!mozne_karty == id]
  }
  rownames(deck_g) = deck_g$ID
  return(deck_g)
}
##################################################
pick.card.player.1 <- function(deck1.f = deck.1, player1.hand.id.f = player1.hand){   # funkce pro vzeti karet
  card.id <- deck1.f$ID[sample(1:nrow(deck1.f), 1)]
  deck1.f <- deck1.f[!(row.names(deck1.f) %in% c(card.id)), ]
  player1.hand.id.f <- append(player1.hand.id.f, card.id)
  rlist<- list("player" = player1.hand.id.f, "deck" = deck1.f)
  return(rlist)
}
pick.card.player.2 <- function(deck2.f = deck.2, player2.hand.id.f = player2.hand){
  card.id <- deck2.f$ID[sample(1:nrow(deck2.f), 1)]
  deck2.f <- deck2.f[!(row.names(deck2.f) %in% c(card.id)), ]
  player2.hand.id.f <- append(player2.hand.id.f, card.id)
  rlist<- list("player" = player2.hand.id.f, "deck" = deck2.f)
  return(rlist)
}

generate.possible.cards <- function(player.hand, mana, karty.f = karty){    #### generace moznych karet
  mozne_karty = c()
  if(length(player.hand)>0 & !(is.na(length(player.hand)))){
    for (karta in 1:length(player.hand)){
      if(karty.f[player.hand[karta], ]$Manacost <= mana){
        mozne_karty <- append(mozne_karty, player.hand[karta])
      }
    } 
  }
  return(mozne_karty)
} 
playcards <- function(hand, manaf, kartyf = karty, boardf = board, t = tah, deckf, postavyf = postavy, ff){
  mozne_karty <- generate.possible.cards(player.hand = hand, mana = manaf, karty.f = kartyf)
  while(length(mozne_karty) > 0 & sum(boardf$Played == t) < 7){        
    if (length(mozne_karty) == 1){
      karta_h = as.numeric(mozne_karty)} else {
        karta_h <- sample(c(mozne_karty), 1)    #zvolime kartu   
      }
    
    manaf <- manaf - kartyf[karta_h, 3]  # ubyla mana
    
    karta.inf <- c(karta_h+t*1000, karta_h, t, 0, 1, kartyf[karta_h, 4], kartyf[karta_h, 5], kartyf[karta_h, 6],
                   kartyf$Taunt[karta_h], kartyf$Divine.shield[karta_h], kartyf$Enrage[karta_h],
                   kartyf$Deathrattle[karta_h], kartyf$Charge[karta_h], kartyf$Stealth[karta_h],
                   kartyf$Windfurry[karta_h], kartyf$Effect.Usual[karta_h], kartyf$Effect.Deathrattle[karta_h], karty$Max.Health[karta_h], karty$Karta[karta_h])
    if(karta.inf[13] == 1){karta.inf[5] <- 0}  ###################### charge ############################
    boardf[nrow(boardf) + 1, ] <- karta.inf
    
    if(kartyf$Karta[karta_h] %in% c("Novice Engineer", "Gnomish Inventor", "Azure Drake") & length(hand) < 10){    ### Battlecry draw a card
      if(nrow(deckf) > 0){
        turn.player <- pick.card.player.1(deck1.f = deckf, player1.hand.id.f = hand)           ### hrac 1 bere kartu
        hand <- turn.player$player
        deckf <- turn.player$deck
      } else {
        postavyf$Health[1] <- as.numeric(postavy$Health[1]) - fatigue1  ### Pokud v dece nejsou zadne karty
        ff <- ff + 1                                      ### Pak ztraci zdravi kazdy tah
      } 
    }
    
    if(karty$Karta[karta_h] == "Murloc Tidehunter" & sum(boardf$Played == t) < 7){
      n <- 0
      if(nrow(filter(boardf, ID == 700))>0){
        n <- sum(as.numeric(filter(boardf, ID == 700)$UnID))
      }
      boardf[nrow(boardf) + 1, ] <- c(700+n+t*1000, 700, t, 0, 1, 1, 1, "Murloc", 0, 0, 0, 0, 0, 0, 0, "", "", 1, "Murloc Scout")
    }
    if(karty$Karta[karta_h] == "Rozorfen Hunter" & sum(boardf$Played == t) < 7){
      n <- 0
      if(nrow(filter(boardf, ID == 705))>0){
        n <- sum(as.numeric(filter(boardf, ID == 705)$UnID))
      }
      boardf[nrow(boardf) + 1, ] <- c(705+n+t*1000, 705, t, 0, 1, 1, 1, "Beast", 0, 0, 0, 0, 0, 0, 0, "", "", 1, "Boar")
    }
    if(karty$Karta[karta_h] == "Dragonling Mechanic" & sum(boardf$Played == t) < 7){
      n <- 0
      if(nrow(filter(boardf, ID == 710))>0){
        n <- sum(as.numeric(filter(boardf, ID == 710)$UnID))
      }
      boardf[nrow(boardf) + 1, ] <- c(710+n+t*1000, 710, t, 0, 1, 2, 1, "Mech", 0, 0, 0, 0, 0, 0, 0, "", "", 1, "Mechanical Dragonling")
    }
    if(karty$Karta[karta_h] == "Leeroy Jenkins" & sum(boardf$Played == t) < 7){
      h <- abs((as.numeric(t)-1)%%2-2)
      boardf[nrow(boardf) + 1, ] <- c(715+h*1000, 715, h, 0, 1, 1, 1, "Dragon", 0, 0, 0, 0, 0, 0, 0, "", "", 1, "Whelp")
      boardf[nrow(boardf) + 1, ] <- c(716+h*1000, 716, h, 0, 1, 1, 1, "Dragon", 0, 0, 0, 0, 0, 0, 0, "", "", 1, "Whelp")
    }
    if(karty$Karta[karta_h] == "Silver Hand Knight" & sum(boardf$Played == t) < 7){
      n <- 0
      if(nrow(filter(boardf, ID == 725))>0){
        n <- sum(as.numeric(filter(boardf, ID == 725)$UnID))
      }
      boardf[nrow(boardf) + 1, ] <- c(725+n+t*1000, 725, t, 0, 1, 2, 2, "none", 0, 0, 0, 0, 0, 0, 0, "", "", 2, "Squire")
    }
    if(karty$Karta[karta_h] == "Onyxia" & sum(boardf$Played == t) < 7){
      kay <- 0
      while(sum(boardf$Played == t) < 7){
        boardf[nrow(boardf) + 1, ] <- c(750+kay+t*1000, 750+kay, t, 0, 1, 1, 1, "Dragon", 0, 0, 0, 0, 0, 0, 0, "", "", 1, "Whelp")
        kay <- kay + 1
      }
    }
    if(karty$Karta[karta_h] == "Injured Blademaster"){
      boardf$Health[nrow(boardf)] <- as.numeric(boardf$Health[nrow(boardf)]) - 4
    }
    if(karty$Karta[karta_h] == "Priestess of Elune"){
      postavyf$Health[t] <- as.numeric(postavyf$Health[t]) + 4
      if(postavyf$Health[t] > 30){postavyf$Health[t] <- 30}
    }
    if(karty$Karta[karta_h] == "Twillight Drake"){
      boardf$Health[nrow(boardf)] <- as.numeric(boardf$Health[nrow(boardf)]) + length(hand)-1
    }
    if(karty$Karta[karta_h] == "Blood Knight"){
      a <- 0
      if (nrow(boardf) > 0){
        for(i in 1:nrow(boardf)){
          if(boardf$Divine.Shield[i] == 1){
            boardf$Divine.Shield[i] <- 0
            a <- a + 1
          }
        }}
      boardf$Health[nrow(boardf)] <- as.numeric(boardf$Health[nrow(boardf)]) + a*3
      boardf$Attack[nrow(boardf)] <- as.numeric(boardf$Attack[nrow(boardf)]) + a*3
    }
    if(karty$Karta[karta_h] == "Goldlight Seer"){ 
      if(nrow(boardf) > 1){
        for(i in 1:(nrow(boardf)-1)){
          if(boardf$Class[i] == "Murloc"){
            boardf$Health[i] <- as.numeric(boardf$Health[i]) + 2
          }
        }}
    }
    if(karty$Karta[karta_h] == "Frostwolf Warlord"){
      a <- sum(boardf$Played == t)-1
      boardf$Attack[nrow(boardf)] <- as.numeric(boardf$Attack[nrow(boardf)]) + a
      boardf$Health[nrow(boardf)] <- as.numeric(boardf$Health[nrow(boardf)]) + a
    }
    
    hand <- hand[!hand %in% c(karta_h)]  # uz neni v ruce
    
    mozne_karty <- generate.possible.cards(player.hand = hand, mana = manaf, karty.f = kartyf)
  }
  retlist <- list("board" = boardf, "mana" = manaf, "hand" = hand, "deck" = deckf, 
                  "postavy" = postavyf, "fatigue" = ff)
  return(retlist)
}

unsleep <- function(boardf = board, t = tah){   ### miniony kterych hrac zahral v minulosti uz utoci
  if(nrow(boardf) != 0){
    for(i in 1:nrow(boardf)){
      if(boardf$Played[i] == t){
        boardf$Sleep[i] <- 0
        boardf$Attacked[i] <- 0
      }
    }
  }
  return(boardf)
}

karty.utoku <- function(boardf = board, t = tah){                   
  mozne_karty <- data.frame(matrix(nrow = 0, ncol = ncol(boardf))) ### karty kterymi muze hrac utocit
  colnames(mozne_karty) <- colnames(boardf)
  if(nrow(boardf) > 0){
    for(i in 1:nrow(boardf)){
      if((boardf$Played[i] == t & boardf$Sleep[i] == 0 & boardf$Attack[i] != 0 & boardf$Attacked[i] == 0 & boardf$Name[i] != "Ancient Watcher") | (boardf$Played[i] == t & boardf$Sleep[i] == 0 & boardf$Attack[i] != 0 & boardf$Attacked[i] == 1 & boardf$Windfurry[i] == 1)){
        mozne_karty[nrow(mozne_karty) + 1, ] <- board[i, ]    #### funkce if overuje i windfurry
      }
    }
  } 
  return(mozne_karty)
}

cile <- function(boardf = board, t = tah, postavyf = postavy){
  mozne_cile <- data.frame(matrix(nrow = 0, ncol = ncol(boardf))) ### karty ktere muzeme urazit popr. postava soupere
  colnames(mozne_cile) <- colnames(boardf)
  if(nrow(boardf) > 0){
    if(t == 1){                                                  ### pokud je na tahu hrac 1 pat utoci minionu 2. hrace
      for(i in 1:nrow(boardf)){
        if(boardf$Played[i] == 2 & boardf$Taunt[i] == 1 & boardf$Stealth[i] != 1){            ### Overovani ma-li oponent taunt
          mozne_cile[nrow(mozne_cile)+1, ] <- boardf[i, ]           ### Pokud ano pak utocime pouze taunty
        }}
      
      if(nrow(mozne_cile) == 0){                                   ### Pokud neni zadny taunt
        for(i in 1:nrow(boardf)){
          if(boardf$Played[i] == 2 & boardf$Stealth[i] != 1){       ### Karty se stealth nelze utocit
            mozne_cile[nrow(mozne_cile)+1, ] <- boardf[i, ]         ### pak muzeme utocit libovolneho minionu
          }
        }
        mozne_cile[nrow(mozne_cile)+1, ] <- postavyf[2, ]           ### i postavu 2. hrace
      }
      
    } else {                                                         ### pokud na tahu je 2. hrac
      
      for(i in 1:nrow(boardf)){
        if(boardf$Played[i] == 1 & boardf$Taunt[i] == 1 & boardf$Stealth[i] != 1){            ### Overovani ma-li oponent taunt
          mozne_cile[nrow(mozne_cile)+1, ] <- boardf[i, ]           ### Pokud ano pak utocime pouze taunty
        }}
      
      if(nrow(mozne_cile) == 0){                                   ### Pokud neni zadny taunt
        for(i in 1:nrow(boardf)){
          if(boardf$Played[i] == 1 & boardf$Stealth[i] != 1){
            mozne_cile[nrow(mozne_cile)+1, ] <- boardf[i, ]         ### pak muzeme utocit libovolneho minionu
          }
        }
        mozne_cile[nrow(mozne_cile)+1, ] <- postavyf[1, ]           ### i postavu 1. hrace
      }
    }
  } else {
    if(t == 1){
      mozne_cile[nrow(mozne_cile)+1, ] <- postavyf[2, ] 
    } else {
      mozne_cile[nrow(mozne_cile)+1, ] <- postavyf[1, ]
    }
  }
  return(mozne_cile)
}
trade <- function(board.f = board, t.f = tah, postavy.f = postavy){
  mozne_karty <- karty.utoku(boardf = board.f, t = t.f)
  mozne_cile <- cile(boardf = board.f, t = t.f, postavyf = postavy.f)
  
  if(nrow(mozne_karty) != 0){ ### pokud neni mozna zadna karta pak trade nedelame
    
    intrade <- rbind(mozne_karty[sample(nrow(mozne_karty),1), ], mozne_cile[sample(nrow(mozne_cile), 1), ])
    rownames(intrade) <- intrade$UnID
    intrade[1, 14] <- 0 #### pri utoku karta ztrati stealh
    
    if(intrade$Divine.Shield[1] == 0 & intrade$Divine.Shield[2] == 0){                ### efekt divine shield
      intrade$Health[1] <- as.numeric(intrade$Health[1])-as.numeric(intrade$Attack[2])
      intrade$Health[2] <- as.numeric(intrade$Health[2])-as.numeric(intrade$Attack[1])
    } else if (intrade$Divine.Shield[1] == 1 & intrade$Divine.Shield[2] != 1){
      intrade$Divine.Shield[1] <- 0
      intrade$Health[2] <- as.numeric(intrade$Health[2])-as.numeric(intrade$Attack[1])
    } else if (intrade$Divine.Shield[1] != 0 & intrade$Divine.Shield[2] == 1){
      intrade$Health[1] <- as.numeric(intrade$Health[1])-as.numeric(intrade$Attack[2])
      intrade$Divine.Shield[2] <- 0
    } else if (intrade$Divine.Shield[1] == 0 & intrade$Divine.Shield[2] == 1){
      intrade$Divine.Shield[2] <- 0
      intrade$Divine.Shield[1] <- 0
    }
    intrade$Attacked[1] <- as.numeric(intrade$Attacked[1]) + 1           ### kolikrat karta utocila behem tahu
    
    for(i in 1:nrow(board.f)){                               ### "vracime" miniony na board uz po trade
      if(board.f$UnID[i] == intrade$UnID[1]){
        board.f[i, ] <- intrade[1, ]
      } else if (board.f$UnID[i] == intrade$UnID[2]){
        board.f[i, ] <- intrade[2, ]
      }
    }
    
    
    for(i in 1:2){                                         ### prepocet zdravi postav
      if(postavy.f$UnID[i] == intrade$UnID[1]){
        postavy.f[i, ] <- intrade[1, ]
      } else if (postavy.f$UnID[i] == intrade$UnID[2]){
        postavy.f[i, ] <- intrade[2, ]
      }
    }
  } ###
  
  reslist <- list("board" = board.f, "postavy" = postavy.f)
  return(reslist)
}
death <- function(boardf = board){
  delete <- c()
  if(nrow(boardf) > 0){
    for (i in 1:nrow(boardf)){
      if(boardf$Health[i] < 1){
        delete <- append(delete, boardf$UnID[i])
      }
    }
    boardf <- boardf[!boardf$UnID %in% delete, ]
  }
  return(boardf)
}

#############################
### Deathrattle functions ###
#############################

LeperGnome <- function(t = played, postavyf = postavy.f){            #### deathrattle Leper Gnome
  if (t == 1) {
    postavyf$Health[2] <- as.numeric(postavyf$Health[2]) - 2
  } else {
    postavyf$Health[1] <- as.numeric(postavyf$Health[1]) - 2
  }
  return(postavyf)
} 

BloodmageThalnos <- function(t = played, deckf1 = deck.f1, deckf2 = deck.f2, p1.hand = p1.handf,
                             p2.hand = p2.handf){
  if(t == 1){
    turn.player1f <- pick.card.player.1(deck1.f = deckf1, player1.hand.id.f = p1.hand)           ### hrac 1 bere kartu
    p1.hand <- turn.player1f$player
    deckf1 <- turn.player1f$deck
  } else {
    turn.player2f <- pick.card.player.2(deck2.f = deckf2, player2.hand.id.f = p2.hand)           ### hrac 1 bere kartu
    p2.hand <- turn.player2f$player
    deckf2 <- turn.player2f$deck
  }
  reslist <- list("deck1" = deckf1, "deck2" = deckf2, "player1hand" = p1.hand, "player2hand" = p2.hand)
  return(reslist)
}

LootHoarder <- function(t = played, deckf1 = deck.f1, deckf2 = deck.f2, p1.hand = p1.handf,
                        p2.hand = p2.handf){
  if(t == 1){
    turn.player1f <- pick.card.player.1(deck1.f = deckf1, player1.hand.id.f = p1.hand)           ### hrac 1 bere kartu
    p1.hand <- turn.player1f$player
    deckf1 <- turn.player1f$deck
  } else {
    turn.player2f <- pick.card.player.2(deck2.f = deckf2, player2.hand.id.f = p2.hand)           ### hrac 1 bere kartu
    p2.hand <- turn.player2f$player
    deckf2 <- turn.player2f$deck
  }
  reslist <- list("deck1" = deckf1, "deck2" = deckf2, "player1hand" = p1.hand, "player2hand" = p2.hand)
  return(reslist)
}

HarvestGolem <- function(boardf = board.f, t = played){
  n <- nrow(filter(boardf, Name == "Damaged Golem"))
  boardf[nrow(boardf)+1, ] <- c(t*1000+990+n, 990+n, t, 0, 1, 2, 1, "Mech", 0, 0, 0, 0, 0, 0, 0, "", "", 1, "Damaged Golem")
  return(boardf)
}

Abomination <- function(boardf = board.f, postavyf = postavy.f, seznamdeathratluf = seznamdeathratlu,
                        minionycrit = miniony){
  for(i in 1:nrow(boardf)){
    if(boardf$Divine.Shield[i] == 0){boardf$Health[i] <- as.numeric(boardf$Health[i]) - 2}
    else {boardf$Divine.Shield[i] <- 0}   # ztrata divine shield
  }
  postavyf$Health[1] <- as.numeric(postavy$Health[1]) - 2
  postavyf$Health[2] <- as.numeric(postavy$Health[2]) - 2
  
  if(nrow(filter(boardf, Health %in% c(-1,0) & Name %in% seznamdeathratluf))>0){
    minionycrit <- rbind(minionycrit, filter(boardf, Health < 1 & Name %in% seznamdeathratluf))
  }
  
  reslist <- list("board" = boardf, "postavy" = postavyf, "miniony" = minionycrit)
  return(reslist)
  ##### deathrattle if dies
}

CairneBloodhoof <- function(boardf = board.f, t = played){
  boardf[nrow(boardf)+1, ] <- c(t*1000+997, 997, t, 0, 1, 4, 5, "none", 0, 0, 0, 0, 0, 0, 0, "", "", 5, "Same BloodHoof")
  return(boardf)
}

SylvanasWindrunner <- function(boardf = board.f, t = played){
  minion <- filter(boardf, Played != t)[ , 1]
  minion <- minion[sample(length(minion), 1)]
  for(i in 1:nrow(boardf)){
    if (boardf$UnID == minion$UnID){
      boardf$Played[i] <- t
      boardf$UnId[i] <- boardf$Played[i]*1000+boardf$ID
    }
  }
  return(boardf)
}

TheBeast <- function(boardf = board.f, t = played){
  if(t == 1){
    boardf[nrow(boardf)+1, ] <- c(2*1000+996, 996, 2, 0, 1, 3, 3, "none", 0, 0, 0, 0, 0, 0, 0, "", "", 3, "Pip Quickwit")
  } else {
    boardf[nrow(boardf)+1, ] <- c(1*1000+996, 996, 1, 0, 1, 3, 3, "none", 0, 0, 0, 0, 0, 0, 0, "", "", 3, "Pip Quickwit")
  }
}




####
#### miniony <- filter(board, Name %in% seznamdeathratlu & Health < 1)
deathrattle <- function(board.f = board, postavy.f = postavy, deck.f1 = deck.1, deck.f2 = deck.2,
                        p1.handf = player1.hand, p2.handf = player2.hand, minionyf = minionyl){
  played <- as.numeric(minionyf$Played)
  reslist <- list()
  if(minionyf$Name == "Leper Gnome"){
    reslist <- LeperGnome(t = played, postavyf = postavy.f)               ### Leper Gnome
  } else if(minionyf$Name == "Bloodmage Thalnos") {
    reslist <- BloodmageThalnos(t = played, deckf1 = deck.f1, deckf2 = deck.f2, p1.hand = p1.handf,
                                p2.hand = p2.handf)
  } else if(minionyf$Name == "Loot Hoarder") {
    reslist <- LootHoarder(t = played, deckf1 = deck.f1, deckf2 = deck.f2, p1.hand = p1.handf,
                           p2.hand = p2.handf)
  } else if(minionyf$Name == "Harvest Golem") {
    reslist <- HarvestGolem(boardf = board.f, t = played)
  } else if(minionyf$Name == "Abomination") {
    reslist <- Abomination(boardf = board.f, postavyf = postavy.f)
  } else if(minionyf$Name == "Cairne Bloodhoof") {
    reslist <- CairneBloodhoof(boardf = board.f, t = played)
  } else if(minionyf$Name == "Sylvanas WIndrunner") {
    reslist <- SylvanasWindrunner(boardf = board.f, t = played)
  } else if(minionyf$Name == "The Beast") {
    reslist <- TheBeast(boardf = board.f, t = played)
  }
  return(reslist)
}


###########################
##### End Deathrattle #####
###########################


############################
##### End turn effects #####
############################

YoungPriestess <- function(boardf = board.f, tf = t.f){
  myminions <- filter(boardf, Played == tf, Name != "Young Priestess") #### vsechny miniony kterym muze dodat zdravi
  if (nrow(myminions) > 0){                                            #### pokud nejsou zadne tak nic
    c <- sample(nrow(myminions), 1)
    unid <- myminions$UnID[c]
    boardf[row.names(boardf) %in% unid, 7] <- as.numeric(boardf[row.names(boardf) %in% unid, 7])+1
  }
  return(boardf)
}

MasterSwordsmith <- function(boardf = board.f, tf = t.f){
  myminions <- filter(boardf, Played == tf, Name != "Master Swordsmith") #### vsechny miniony kterym muze dodat utok
  if (nrow(myminions) > 0){                                              #### pokud nejsou zadne tak nic
    c <- sample(nrow(myminions), 1)
    unid <- myminions$UnID[c]
    boardf[row.names(boardf) %in% unid, 6] <- as.numeric(boardf[row.names(boardf) %in% unid, 7])+1
  }
  return(boardf)
}

ImpMaster <- function(boardf = board.f, tf = t.f){
  if(tf == abs(length(as.numeric(filter(boardf, Name == "Imp Master", Played == tf)$Played)))){
    boardf[nrow(boardf)+1, ] <- c(tf*1000+894, 894, tf, 0, 1, 1, 1, "Demon", 0, 0, 0, 0, 0, 0, 0, "", "", 1, "Imp")
    for (i in 1:nrow(boardf)){
      if(boardf$Name[i] == "Imp Master" & boardf$Played[i] == tf){boardf$Health[i] <- as.numeric(boardf$Health[i]) - 1} 
    }
    for(i in 1:nrow(boardf)){ 
      if(boardf$Name[i] == "Imp"){
        boardf$UnID[i] = tf*1000+894 - i
        boardf$ID[i] = 894-i
      }
    }
  }
  boardf<-death(boardf = boardf)
  return(boardf)
}

Hogger <- function(boardf = board.f, tf = t.f, minionf = minion){ 
  played <- as.numeric(minionf$Played)
  if(tf == played){
    boardf[nrow(boardf)+1, ] <- c(tf*1000+995, 995, tf, 0, 1, 2, 2, "none", 1, 0, 0, 0, 0, 0, 0, "", "", 2, "Ghoul")
  }
  for(i in 1:nrow(boardf)){ 
    if(boardf$Name[i] == "Ghoul"){
      boardf$UnID[i] = tf*1000+995 - i
      boardf$ID[i] = 995 - i}
  }
  return(boardf)
}

Gruul <- function(boardf = board.f){
  row <- filter(boardf, Name == "Gruul")$UnID
  boardf[row.names(boardf) %in% row, 6] <- as.numeric(boardf[row.names(boardf) %in% row, 6]) + 1
  boardf[row.names(boardf) %in% row, 7] <- as.numeric(boardf[row.names(boardf) %in% row, 7]) + 1
  return(boardf)
}


Endturn <- function(board.f = board, t.f = tah, minionyf = miniony){
  minion <- minionyf[1, ]
  if(minion$Name == "Young Priestess"){
    board.f <- YoungPriestess(boardf = board.f, tf = t.f)
  } else if (minion$Name == "Master Swordsmith"){
    board.f <- MasterSwordsmith(boardf = board.f, tf = t.f)
  } else if (minion$Name == "Imp Master"){
    board.f <- ImpMaster(boardf = board.f, tf = t.f)
  } else if (minion$Name == "Hogger"){
    board.f <- Hogger(boardf = board.f, tf = t.f, minionf = minion)
  } else if (minion$Name == "Gruul"){
    board.f <- Gruul(boardf = board.f)
  }
  return(board.f)
}

########################################
#####             hra              #####
########################################
 
nwin1 = 0
nwin2 = 0
for (p in 1:10000){
deck1 <- deck.generate(cards)
deck2 <- deck.generate(cards)

player1.start.offer <- deck1[sample(1:30, 3, replace = FALSE), ]   ### nabidka na zacatku
player2.start.offer <- deck2[sample(1:30, 4, replace = FALSE), ]


deck1 <- deck1[!(row.names(deck1) %in% c(player1.start.offer$ID)), ]  ### Karty ktere jsou v ruce uz nejsou v dece
deck2 <- deck2[!(row.names(deck2) %in% c(player2.start.offer$ID)), ]

player1.starthand <- player1.start.offer                           ### Generovani pocatecnich karet
player2.starthand <- player2.start.offer


player1.health <- 30
player2.health <- 30

player1.hand.id <- player1.starthand$ID
player2.hand.id <- player2.starthand$ID

player1.hand = player1.hand.id
player2.hand = player2.hand.id
deck.1 = deck1
deck.2 = deck2
karty = cards

tah = 2                                           ### na zacatku je tah dva aby pri zmene tahu na zacatku byl tah 1. hrace
mana1 = 0                                         ### man na zacatku je 3 (na zacatku je 2 pak se zvysi)
mana2 = 0
## vytvorime board
columns = c("UnID", "ID", "Played", "Attacked", "Sleep", "Attack", "Health", "Class","Taunt", "Divine.Shield", "Enrage", "Deathrattle", "Charge", "Stealth", "Windfurry", "Effect", "Deathrattle effect", "Max.Health", "Name")
board <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(board) = columns

konec <- 0

postava1 <- c(1, 1, 1, 0, 0, 0, 30, "none", 0, 0, 0, 0, 0, 0, 0, "Postava1", "", 30, "Postava1") ### postavy
postava2 <- c(2, 2, 2, 0, 0, 0, 30, "none", 0, 0, 0, 0, 0, 0, 0, "Postava2", "", 30, "Postava2")
postavy <- data.frame(t(data.frame(postava1, postava2)))
colnames(postavy) <- columns

fatigue1 <- 1
fatigue2 <- 1

while(konec == 0){                                 ### tahy se opakuji dokud neskonci hra
  if (tah == 1) {tah <- 2} else {tah <- 1}         ### urcime ci tah je ted
  
  if (tah == 1) {                                  ### pokud je tah 1. hrace
    
    board <- unsleep()
    
    mana1 = mana1 + 1                              ### mana 1. hrace se zvysi o 1
    if (mana1>10) {mana1 = 10}                     ### max mana = 10
    
    if(nrow(deck.1) > 0){
      turn.player1 <- pick.card.player.1()           ### hrac 1 bere kartu
      player1.hand <- turn.player1$player
      deck.1 <- turn.player1$deck
    } else {
      postavy$Health[1] <- as.numeric(postavy$Health[1]) - fatigue1  ### Pokud v dece nejsou zadne karty
      fatigue1 <- fatigue1 + 1                                      ### Pak ztraci zdravi kazdy tah
    } 
    
    res <- playcards(hand = player1.hand, manaf = mana1, deckf = deck.1, ff = fatigue1)  ### hrac hraje vsechny mozne karty dokud to dovoli mana
    board <- res$board
    player1.hand <- res$hand
    deck.1 <- res$deck
    fatigue1 <- res$fatigue
    postavy <- res$postavy
    
    if((sum(board$Played == 1 & board$Sleep == 0)) > 0){
      for(i in 1:(sum(board$Played == 1 & board$Sleep == 0))){ 
        restrade<-trade()
        board <- restrade$board
        postavy <- restrade$postavy
        
        #### Deathrattles ####
        miniony <- as.data.frame(filter(board, Name %in% seznamdeathratlu & Health < 1))   #### Deathrattle
        if(nrow(miniony) > 0){
          while(nrow(miniony) > 0){ 
            minionyl <- miniony[1, ]
            deathrattlelist <- deathrattle(board.f = board, postavy.f = postavy, deck.f1 = deck.1, deck.f2 = deck.2,
                                           p1.handf = player1.hand, p2.handf = player2.hand, minionyf = minionyl)
            vysledky <- rownames(deathrattlelist)
            if (is.null(vysledky)){vysledky <- names(deathrattlelist)}
            
            if ("postava1" %in% vysledky){postavy[1, ] <- deathrattlelist[row.names(deathrattlelist) %in% "postava1", ]}
            if ("postava1" %in% vysledky){postavy[2, ] <- deathrattlelist[row.names(deathrattlelist) %in% "postava2", ]}
            if ("deck1" %in% vysledky){deck.1 <- deathrattlelist$deck1}
            if ("deck2" %in% vysledky){deck.2 <- deathrattlelist$deck2}
            if ("board" %in% vysledky){board <- deathrattlelist$board}
            if ("player1hand" %in% vysledky){player1.hand <- deathrattlelist$player1hand}
            if ("player2hand" %in% vysledky){player2.hand <- deathrattlelist$player2hand}
            if ("miniony" %in% vysledky){miniony <- deathrattlelist$miniony}
            miniony <- miniony[-1, ]
          }}
        
        
        
        rownames(board) <- board$UnID
        board <- death()
      }}
    miniony <- filter(board, Name %in% seznamendturn)
    if(nrow(miniony) > 0){
      for(i in 1:nrow(miniony)){
        board <- Endturn(board.f = board, t.f = tah, minionyf = miniony)
        miniony <- miniony[-1, ]
      }}
    
  } else {
    
    board <- unsleep()
    
    mana2 = mana2 + 1
    if(mana2 > 10) {mana2 = 10}               ### max mana = 10
    
    if(nrow(deck.2) > 0){
      turn.player2 <- pick.card.player.2()
      player2.hand <- turn.player2$player
      deck.2 <- turn.player2$deck
    } else {
      postavy$Health[2] <- as.numeric(postavy$Health[2]) - fatigue2
      fatigue2 <- fatigue2 + 1
    }
    
    res <- playcards(hand = player2.hand, manaf = mana2, deckf = deck.2, ff = fatigue2)  ### hrac hraje vsechny mozne karty dokud to dovoli mana
    board <- res$board
    player2.hand <- res$hand
    deck.2 <- res$deck
    fatigue2 <- res$fatigue
    postavy <- res$postavy
    
    if(sum(board$Played == 2 & board$Sleep == 0) > 0){            ### overovani moznosti trade
      for(i in 1:(sum(board$Played == 2 & board$Sleep == 0))){
        restrade<-trade()
        board <- restrade$board
        postavy <- restrade$postavy
        
        a <- rbind(deck.2, deck2)
        #### Deathrattles ####
        miniony <- as.data.frame(filter(board, Name %in% seznamdeathratlu & Health < 1))   #### Deathrattle
        if(nrow(miniony) > 0){
          while(nrow(miniony) > 0){ 
            minionyl <- miniony[1, ]
            deathrattlelist <- deathrattle(board.f = board, postavy.f = postavy, deck.f1 = deck.1, deck.f2 = deck.2,
                                           p1.handf = player1.hand, p2.handf = player2.hand, minionyf = minionyl)
            vysledky <- rownames(deathrattlelist)
            if (is.null(vysledky)){vysledky <- names(deathrattlelist)}
            
            if ("postava1" %in% vysledky){postavy[1, ] <- deathrattlelist[row.names(deathrattlelist) %in% "postava1", ]}
            if ("postava1" %in% vysledky){postavy[2, ] <- deathrattlelist[row.names(deathrattlelist) %in% "postava2", ]}
            if ("deck1" %in% vysledky){deck.1 <- deathrattlelist$deck1}
            if ("deck2" %in% vysledky){deck.2 <- deathrattlelist$deck2}
            if ("board" %in% vysledky){board <- deathrattlelist$board}
            if ("player1hand" %in% vysledky){player1.hand <- deathrattlelist$player1hand}
            if ("player2hand" %in% vysledky){player2.hand <- deathrattlelist$player2hand}
            if ("miniony" %in% vysledky){miniony <- deathrattlelist$miniony}
            miniony <- miniony[-1, ]
          }}
        
        
        
        rownames(board) <- board$UnID
        board <- death()
      }}
    
    miniony <- filter(board, Name %in% seznamendturn)
    if(nrow(miniony) > 0){
      for(i in 1:nrow(miniony)){
        board <- Endturn(board.f = board, t.f = tah, minionyf = miniony)
        miniony <- miniony[-1, ]
      }}
  }
  
  if(postavy$Health[1] < 1 | postavy$Health[2] < 1){
    konec <- 1
    if(postavy$Health[1] < 1){
      resultofgame <- c(0,1)
    } else {
      resultofgame <- c(1,0)
    }
  }
}

 nwin1 <- nwin1 + resultofgame[1]
 nwin2 <- nwin2 + resultofgame[2]
 print(p)
 print(paste(nwin1, nwin2))
}
print(paste(nwin1, nwin2))

