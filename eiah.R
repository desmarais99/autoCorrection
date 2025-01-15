library(purrr)
library(stringr)
library(dplyr)
library(tibble)
library(tidyr)
library(smoother)

source('util.R')

#############################################################################
## Mohler data
#############################################################################
## Sentence similarity <2025-01-07 Tue>
#############################################################################

library(ollamar)
list_models()

test_connection()

m0 <- tibble(read.csv('Data/Mohler/data/mohler_dataset_edited.csv'))
m0$question <- factor(m0$question)
m0$desired_answer <- factor(m0$desired_answer)
m0$student_answer <- factor(m0$student_answer)

## compute embeddings
# da.emb <- embed('bge-m3:latest', levels(m0$desired_answer)) # cor = 0.517
# sa.emb <- embed('bge-m3:latest', levels(m0$student_answer))
# da.emb <- embed('all-minilm', levels(m0$desired_answer)) # cor = 0.492
# sa.emb <- embed('all-minilm', levels(m0$student_answer))
da.emb <- embed('mxbai-embed-large', levels(m0$desired_answer)) # cor = 0.523
sa.emb <- embed('mxbai-embed-large', levels(m0$student_answer))

dim(da.emb)                             # 1024 80
dim(sa.emb)                             # 1024 2172

m.student_answer <- sa.emb[,m0$student_answer]
m.desired_answer <- da.emb[,m0$desired_answer]
all(m.desired_answer[,1] == m.desired_answer[,2])
res0 <- sapply(1:ncol(m.desired_answer), function(i) m.desired_answer[,i] %*% m.student_answer[,i])
cor(m0$score_avg, res0)                 # 0.523 with mxbai-embed-large

## figure 1
pdf(file='Papers/Images/score-similarite.pdf', width=6, height=4)
plot(res0+rnorm(nrow(m0),0,.01), m0$score_avg+rnorm(nrow(m0),0,.1), pch='.', cex=2, main='Distribution du score par\n la similarité avec la réponse attendue', xlab='Similarité', ylab='Score')
dev.off()
cor(m0$score_me, res0)
cor(m0$score_other, res0)
cor(m0$score_other, m0$score_me)
cor(m0$score_other, m0$score_avg)
cor(m0$score_me, m0$score_avg)
i.set <- unique(m0$id)

## figure 2
pdf(file='Papers/Images/score-similarite-q.pdf') #, width=6, height=6)
par(mfrow=c(3,2), mar = c(2, 4, 4, 2) + 0.1)
sapply(i.set[c(2:4,78:80)], function(id) {i <- m0$id==id; plot(res0[i]+rnorm(sum(i),0,.0001), m0[i,]$score_avg+rnorm(sum(i),0,.001), main=paste('Question', id), xlab='Similarité', ylab='Score', xlim=c(0,1), ylim=c(0,5))})
dev.off()

0

#############################################################################
##  Gaussian smoothing for scoring function
#############################################################################
n <- 50
i <- sample(nrow(m0), n)
plot(res0[i], m0$score_avg[i], ylim=c(1,5), xlim=c(0,1))
seeds <- rbind(res0[i], m0$score_avg[i])
seeds.k <- dnorm(outer(seq(.3,1,.1), seeds[1,], '-'), 0, .1)
pred <- ((seeds.k) / rowSums(seeds.k)) %*% seeds[2,]
score <- function(x, pred) approx(c(0,seq(min(x),1,length.out=length(pred)+1)), c(0,pred,5), x)

#############################################################################
## By question : grade half, automate grading for other half
#############################################################################

m0
m1 <- m0 %>% select(!desired_answer) %>% add_column(res0, .before='question')
m1

q <- unique(m1$id)

par(mfrow=c(3,2))
window=.4

## visualization of sample of questions
res <- sapply(70:75, function(q.i) {
    df <- (m1 %>% filter(id==q[q.i]) %>% arrange(res0) %>% select(res0,score_avg))
    i <- ((1:nrow(df))%%2)>0            # observed
    res0.smth <- smth(df$score_avg[i], tails=T, window=window) # observed scores smoothed
    plot(df[!i,], main=q.i, col='blue', type='b', ylim=c(0,5));                  # raw scores (all)
    lines(df$res0[i], res0.smth, col='red', type='b')          # observed smoothed scores
    pred <- approx(df$res0[i], res0.smth, df$res0[!i])         # predicted scores smoothed
    lines(pred, type='b')                                      # predicted scores smoothed
    cor(pred$y, df$score_avg[!i], use='pairwise.complete.obs')
})
(res)

## individual predictions
par(mfrow=c(3,2))
res <- sapply(q, function(quest) {
    df <- (m1 %>% filter(id==quest) %>% arrange(res0) %>% select(res0,score_avg))
    i <- ((1:nrow(df))%%2)>0            # observed
    res0.smth <- smth(df$score_avg[i], tails=T, window=window) # observed scores smoothed
    pred <- approx(df$res0[i], res0.smth, df$res0[!i])         # predicted scores smoothed
    c(pred=(pred$y), observed=c(df$score_avg[!i]), q=rep(quest,sum(!i)), i=which(!i))
})
res.pred <- unlist(lapply(res, function(i) i[grepl('pred', names(i))]))
res.observed <- unlist(lapply(res, function(i) i[grepl('observed', names(i))]))

## correlation
cor(res.pred,res.observed, use='pairwise.complete.obs') # correlation for all questions with !i half (.623)
0

## RMSE
res3 <- sapply(seq(.1,2,.1), function(window) {
    res <- sapply(1:length(q), function(q.i) {
        df <- (m1 %>% filter(id==q[q.i]) %>% arrange(res0) %>% select(res0,score_avg))
        i <- ((1:nrow(df))%%2)>0            # observed
        res0.smth <- smth(df$score_avg[i], tails=T, window=window) # observed scores smoothed
        pred <- approx(df$res0[i], res0.smth, df$res0[!i])         # predicted scores smoothed
        sqrt(mean((pred$y - df$score_avg[!i])^2, na.rm=T))
    })
    res
})
res3

colMeans(res3)                          # RMSE lowest = .755
rbind(seq(.1,2,.1),colMeans(res3))      # RMSE lowest at window=0.9
0

