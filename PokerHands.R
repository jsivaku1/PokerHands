poker_hands_test <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/poker/poker-hand-testing.data"),header=FALSE)
poker_hands_train <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/poker/poker-hand-training-true.data"),header=FALSE)

colnames(poker_hands_test) <- c("S1","C1","S2","C2","S3","C3","S4","C4","S5","C5","CLASS")
colnames(poker_hands_train) <- c("S1","C1","S2","C2","S3","C3","S4","C4","S5","C5","CLASS")

library(ggplot2)

library(VGAM)

library("MASS")

library(dplyr)

##testset
dim(poker_hands_test)

##Count of Poker Hands in Test Set
ggplot(data = poker_hands_test, aes(poker_hands_test$CLASS), fill = factor(poker_hands_test$CLASS)) + geom_bar(aes(fill = factor(poker_hands_test$CLASS))) + xlab("Class") + ylab("Count") + theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0, 9, by = 1))

##Computing Porbabilities of each Hand
l9 = length(which(poker_hands_test$CLASS == 9))
l9
l8 = length(which(poker_hands_test$CLASS == 8))
l8
l7 = length(which(poker_hands_test$CLASS == 7))
l7
l6 = length(which(poker_hands_test$CLASS == 6))
l6
l5 = length(which(poker_hands_test$CLASS == 5))
l5
l4 = length(which(poker_hands_test$CLASS == 4))
l4
l3 = length(which(poker_hands_test$CLASS == 3))
l3
l2 = length(which(poker_hands_test$CLASS == 2))
l2
l1 = length(which(poker_hands_test$CLASS == 1))
l1
l0 = length(which(poker_hands_test$CLASS == 0))
l0
s = sum(l0+l1+l2+l3+l4+l5+l6+l7+l8+l9)
pl9 = l9/s
pl9
pl8 = l8/s
pl8
pl7 = l7/s
pl7
pl6 = l6/s
pl6
pl5 = l5/s
pl5
pl4 = l4/s
pl4
pl3 = l3/s
pl3
pl2 = l2/s
pl2
pl1 = l1/s
pl1
pl0 = l0/s
pl0

p = sum(pl9+pl8+pl7+pl6+pl5+pl4+pl3+pl2+pl1+pl0)
p

x = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

y = c(pl0, pl1, pl2, pl3, pl4, pl5, pl6, pl7, pl8, pl9)

##Probability for each Poker Hand in Test Set
qplot(x, y, xlab = "Class", ylab = "Probability") + ggtitle("Probability of Poker Hands (Test Set)") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(colour = I("magenta")) + geom_line(colour = I("green")) + scale_x_continuous(breaks = seq(0, 9, by = 1))

table(poker_hands_test$CLASS)

(1-y)/y

##trainingset
dim(poker_hands_train)

##Count of Poker Hands in Training Set
ggplot(data = poker_hands_train, aes(poker_hands_train$CLASS), fill = factor(poker_hands_train$CLASS)) + geom_bar(aes(fill = factor(poker_hands_train$CLASS))) + xlab("Class") + ylab("Count") + theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0, 9, by = 1))

##Computing Porbabilities of each Hand
ll9 = length(which(poker_hands_train$CLASS == 9))
ll9
ll8 = length(which(poker_hands_train$CLASS == 8))
ll8
ll7 = length(which(poker_hands_train$CLASS == 7))
ll7
ll6 = length(which(poker_hands_train$CLASS == 6))
ll6
ll5 = length(which(poker_hands_train$CLASS == 5))
ll5
ll4 = length(which(poker_hands_train$CLASS == 4))
ll4
ll3 = length(which(poker_hands_train$CLASS == 3))
ll3
ll2 = length(which(poker_hands_train$CLASS == 2))
ll2
ll1 = length(which(poker_hands_train$CLASS == 1))
ll1
ll0 = length(which(poker_hands_train$CLASS == 0))
ll0

ss = sum(ll0+ll1+ll2+ll3+ll4+ll5+ll6+ll7+ll8+ll9)

pll9 = ll9/ss
pll9
pll8 = ll8/ss
pll8
pll7 = ll7/ss
pll7
pll6 = ll6/ss
pll6
pll5 = ll5/ss
pll5
pll4 = ll4/ss
pll4
pll3 = ll3/ss
pll3
pll2 = ll2/ss
pll2
pll1 = ll1/ss
pll1
pll0 = ll0/ss
pll0

pp = sum(pll9+pll8+pll7+pll6+pll5+pll4+pll3+pll2+pll1+pll0)
pp

y2 = c(pll0, pll1, pll2, pll3, pll4, pll5, pll6, pll7, pll8, pll9)

##Probability for each Poker Hand in Training Set
qplot(x, y2, xlab = "CLASS", ylab = "Probability") + ggtitle("Probability of Poker Hands (Training Set)") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(colour = I("magenta")) + geom_line(colour = I("green")) + scale_x_continuous(breaks = seq(0, 9, by = 1))

table(poker_hands_train$CLASS)

(1-y2)/y2

## fit vglm on train
m1 <- vglm(CLASS~S1 + C1 + S2 + C2 + S3 + C3 + S4 + C4 + S5 + C5, family = propodds(reverse = FALSE), data = poker_hands_train)

summary(m1)

AICvlm(m1)

exp(coef(m1))

## probability comparison
y3 = c(0.501177, 0.422569, 0.047539, 0.0211285, 0.00392465, 0.0019654, 0.00144058, 0.000240096, 0.0000138517, 0.00000153908)

ggplot() + ggtitle("Probability Comparison") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(aes(x, y, color="red", size = 3)) + geom_point(aes(x, y2, color="blue", size = 2)) + geom_point(aes(x, y3, color = "green", size = 1)) + scale_x_continuous(breaks = seq(0, 9, by = 1)) + scale_colour_discrete(breaks=c("blue", "green", "red"),labels=c("Train", "Known", "Test")) + xlab("Class") + ylab("Probability")  

##odds comparison
o1 = (1-y)/y
o1

o2 = (1-y2)/y2
o2

o3 = (1-y3)/y3
o3

ggplot() + ggtitle("Odds Comparison") + theme(plot.title = element_text(hjust = 0.5)) + geom_point(aes(x, o1, color="red", size = 3)) + geom_point(aes(x, o2, color="blue", size = 2)) + geom_point(aes(x, o3, color = "green", size = 1)) + scale_x_continuous(breaks = seq(0, 9, by = 1)) + scale_colour_discrete(breaks=c("blue", "green", "red"),labels=c("Train", "Known", "Test")) + xlab("Class") + ylab("Odds")

##predictions
p = predictvglm(m1,data.frame(S1 = 1, C1 = 4, S2 = 3, C2 = 4, S3 = 2, C3 = 4, S4 = 4, C4 = 4, S5 = 1, C5 = 3))
p
exp(p)/(1+exp(p))

## frequency histograms
poker_hands_train$CLASS = factor(poker_hands_train$CLASS,levels = 0:9,labels=c("Nothing","OP","TP","TK","Straight","Flush","Full House","FK","SF","RF"))

poker_hands_test$CLASS = factor(poker_hands_test$CLASS,levels = 0:9,labels=c("Nothing","OP","TP","TK","Straight","Flush","Full House","FK","SF","RF"))

ggplot(data = poker_hands_train, aes(poker_hands_train$CLASS), fill = factor(poker_hands_train$CLASS)) + geom_bar(aes(fill = factor(poker_hands_train$CLASS))) + xlab("Poker Hand") + ylab("Count") + ggtitle("Histogram for Poker Hands (Training Set)") + theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + scale_x_discrete(labels = c("Nothing","OP","TP","TK","Straight","Flush","Full House","FK","SF","RF")) + coord_flip()

ggplot(data = poker_hands_test, aes(poker_hands_test$CLASS), fill = factor(poker_hands_test$CLASS)) + geom_bar(aes(fill = factor(poker_hands_test$CLASS))) + xlab("Poker Hand") + ylab("Count") + ggtitle("Histogram for Poker Hands (Test Set)") + theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + scale_x_discrete(labels = c("Nothing","OP","TP","TK","Straight","Flush","Full House","FK","SF","RF")) + coord_flip()


##predictions
train = poker_hands_train
test = poker_hands_test

test$S1 = factor(test$S1,levels = c(1,2,3,4),labels=c("H","S","D","C"),ordered = TRUE)
test$C1 = factor(test$C1,levels = 1:13,labels=1:13,ordered = TRUE)
test$S2 = factor(test$S2,levels = c(1,2,3,4),labels=c("H","S","D","C"),ordered = TRUE)
test$C2 = factor(test$C2,levels = 1:13,labels=1:13,ordered = TRUE)
test$S3 = factor(test$S3,levels = c(1,2,3,4),labels=c("H","S","D","C"),ordered = TRUE)
test$C3 = factor(test$C3,levels = 1:13,labels=1:13,ordered = TRUE)
test$S4 = factor(test$S4,levels = c(1,2,3,4),labels=c("H","S","D","C"),ordered = TRUE)
test$C4 = factor(test$C4,levels = 1:13,labels=1:13,ordered = TRUE)
test$S5 = factor(test$S5,levels = c(1,2,3,4),labels=c("H","S","D","C"),ordered = TRUE)
test$C5 = factor(test$C5,levels = 1:13,labels=1:13,ordered = TRUE)
test$CLASS = factor(test$CLASS,levels = 0:9,labels=c("Nothing","OP","TP","TK","Straight","Flush","Full House","FK","SF","RF"),ordered = TRUE)

test_X = test[,-11]
test_Y = test[,11]

train$S1 = factor(train$S1,levels = c(1,2,3,4),labels=c("H","S","D","C"),ordered = TRUE)
train$C1 = factor(train$C1,levels = 1:13,labels=1:13,ordered = TRUE)
train$S2 = factor(train$S2,levels = c(1,2,3,4),labels=c("H","S","D","C"),ordered = TRUE)
train$C2 = factor(train$C2,levels = 1:13,labels=1:13,ordered = TRUE)
train$S3 = factor(train$S3,levels = c(1,2,3,4),labels=c("H","S","D","C"),ordered = TRUE)
train$C3 = factor(train$C3,levels = 1:13,labels=1:13,ordered = TRUE)
train$S4 = factor(train$S4,levels = c(1,2,3,4),labels=c("H","S","D","C"),ordered = TRUE)
train$C4 = factor(train$C4,levels = 1:13,labels=1:13,ordered = TRUE)
train$S5 = factor(train$S5,levels = c(1,2,3,4),labels=c("H","S","D","C"),ordered = TRUE)
train$C5 = factor(train$C5,levels = 1:13,labels=1:13,ordered = TRUE)
train$CLASS = factor(train$CLASS,levels = 0:9,labels=c("Nothing","OP","TP","TK","Straight","Flush","Full House","FK","SF","RF"),ordered = TRUE)

options(contrasts=c("contr.treatment", "contr.poly"))
polrMod <- polr(CLASS ~S1+C1+S2+C2+S3+C3+S4+C4+S5+C5, data=train)
predictedClass <- predict(polrMod, test_X,type ="class")  # predict the classes directly
head(predictedClass)
freq_table <- data.frame(table(predictedClass))
colnames(freq_table) <- c("Hands","Count")
freq_table <- freq_table[order(freq_table$Count),]

#frequency table for predicted test set
freq_table

#Confusion Matrix for testset and Predicted set
table(predictedClass,test_Y)



