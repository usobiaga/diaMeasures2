
require(testthat)

## Linguistic measures test

## data representation

##    location                     q1              q2                 q3
## 1:   Itziar          deitu,deitatu            diru freskatu,urreztatu
## 2:    Maule                  deitu             sos freskatu,urreztatu
## 3:  Senpere                  deitu          dihura             urritu
## 4:   Urketa deitatu,erran,atxikitu diru,sos,dihura   ihintzatu,urritu
## 5: Uztartze                               diru,sos                   
##             q4
## 1: herots,hots
## 2:      herots
## 3:            
## 4:            
## 5:   harrabots

set <- structure(list(gender = structure(c(1L, 2L, 2L, 1L, 2L, 1L, 1L, 
                          1L, 2L, 1L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 
                          1L, 1L, 2L), .Label = c("Female", "Male"), class = "factor"), 
                      location = c("Itziar", "Itziar", "Maule", "Urketa", "Urketa", 
                          "Urketa", "Senpere", "Itziar", "Maule", "Uztartze", "Uztartze", 
                          "Urketa", "Urketa", "Urketa", "Senpere", "Itziar", "Itziar", 
                          "Maule", "Maule", "Urketa", "Urketa", "Senpere", "Itziar", 
                          "Itziar", "Maule", "Uztartze"),
                      question = c("q1", "q1", "q1", "q1", "q1", "q1", "q1", "q2", "q2", "q2",
                          "q2", "q2", "q2", "q2", "q2", "q3", "q3", "q3", "q3", "q3", "q3", "q3", 
                          "q4", "q4", "q4", "q4"),
                      answer = c("deitu", "deitatu", "deitu", 
                          "deitatu", "erran", "atxikitu", "deitu", "diru", "sos", "diru", 
                          "sos", "diru", "sos", "dihura", "dihura", "freskatu", "urreztatu", 
                          "freskatu", "urreztatu", "ihintzatu", "urritu", "urritu", 
                          "herots", "hots", "herots", "harrabots")),
                 .Names = c("gender", "location", "question", "answer"),
                 row.names = c(NA, -26L), class = "data.frame")

context('Measure correctness')

test_that('IRD correctness DICE', {
    m <- diaMeasures2::diaMeasure(set, location ~ question, value.var = 'answer', measure = 'rdi', binary.index = 'dice')
    m <- as.matrix(m)
    expect_that(round(m[1, 2], 4), equals(0.4167)) # Itziar Maule (simple example)
    expect_that(round(m[4, 5], 4), equals(0.2000))# Urketa Uztartze (NA observations)
    expect_that(round(m[4, 3], 4), equals(0.6111)) # Sempere Urketa (more sample)
})

test_that('IRD correctness JACCARD', {
    m <-  diaMeasures2::diaMeasure(set, location ~ question, value.var = 'answer', measure = 'rdi', binary.index = 'jac')
    m <- as.matrix(m)
    expect_that(round(m[1, 2], 4), equals(1 - 0.5)) # Itziar Maule (simple example)
    expect_that(round(m[4, 5], 4), equals(0.3333))# Urketa Uztartze (NA observations)
    expect_that(round(m[4, 3], 4), equals(0.7222)) # Sempere Urketa (more sample)
})

## ipi example from "Goebl-1987b-Points chauds de l'analyse dialectométrique-pondération et
## visualisation"

ipiSet <- structure(list(
    location = c("1", "2", "3", "4", "5", "6", "1", 
                 "2", "3", "5", "6", "1", "2", "3", "4", "5", "6", "1", "2", "4", 
                 "5", "6", "1", "2", "3", "4", "5", "6"),
    question = c("5", "5", 
                 "5", "5", "5", "5", "4", "4", "4", "4", "4", "3", "3", "3", "3", 
                 "3", "3", "2", "2", "2", "2", "2", "1", "1", "1", "1", "1", "1"),
    answer = c("j", "j", "j", "j", "j", "j", "h", "i", "h", "i", 
               "h", "e", "f", "e", "e", "f", "g", "c", "c", "c", "d", "d", "a", 
               "a", "b", "a", "a", "b")),
    .Names = c("location", "question", "answer"),
    row.names = c(NA, -28L), class = "data.frame")

test_that('IPD correctness', {
    m <- diaMeasure(ipiSet, location ~ question, 'answer', 'ipd')
    m <- as.matrix(m)
    expect_that(round(m[2, 4], 4), equals(0.5588))
    expect_that(round(m[1, 6], 4), equals(0.2035))
    expect_that(round(m[1, 5], 4), equals(0.1818))
    expect_that(round(m[4, 5], 4), equals(0.2500))
    expect_that(round(m[6, 5], 4), equals(0.2437))
})

coverSet <- structure(list(
    location = c("1", "2", "2", "3", "3", "3", "4", 
                 "4", "5", "5", "6", "6"),
    question = c("1", "1", "1", "1", "1", 
                 "1", "1", "1", "1", "1", "1", "1"),
    answer = c("a", "a", "b",  "aaaaa", "aaaa", "bbbbbb", "aaaaaa",
               "cccccc", "a", "b", "a", "c")),
    .Names = c("location", "question", "answer"),
    row.names = c(NA, -12L), class = "data.frame")

test_that('Cover Set correctness', {
    m <- diaMeasure(coverSet, location ~ question, 'answer', 'lv', 'cover', weight = c(1, 1, 1))
    m <- as.matrix(m)
    expect_that(round(m[1, 2], 4), equals(0.5000))
    expect_that(round(m[3, 4], 4), equals(3.0000))
    expect_that(round(m[5, 6], 4), equals(0.5000))
    expect_that(round(m[1, 3], 4), equals(4.3333))
})

context('stringist distances')




##distances between variables

distancesBetweenForms <- function(set){

    nq <- length(unique(set$question))
    uniqueQuestions <- unique(set$question)
    resultForms <- matrix(NA, nq, nq, dimnames = list(uniqueQuestions, uniqueQuestions))

    for (i in seq_along(uniqueQuestions)){
        print(i)
        for (j in 1:i){
            
            if (i == j) next
            
            townsA <- set[set$question == uniqueQuestions[i], 'location']
            townsB <- set[set$question == uniqueQuestions[j], 'location']
            towns <- intersect(townsA, townsB)
            
            subsetA <- set[set$question == uniqueQuestions[i] & set$location %in% towns, ]
            measureA <- diaMeasure(
                subsetA,
                formula = location ~ question,
                value.var = 'answer',
                measure = 'rdi',
                binary.index = 'dice')
            
            subsetB <- set[set$question == uniqueQuestions[j] & set$location %in% towns, ]
            measureB <- diaMeasure(
                subsetB,
                formula = location ~ question,
                value.var = 'answer',
                measure = 'rdi',
                binary.index = 'dice')
            
            resMat <- abs(as.matrix(measureA) - as.matrix(measureB))
            upperResMat<- resMat[upper.tri(resMat, diag = FALSE)]
            ##res <- sum(upperResMat) / length(upperResMat)
            res <- sum(upperResMat) / length(upperResMat)
            resultForms[uniqueQuestions[i], uniqueQuestions[j]] <- res
            resultForms[uniqueQuestions[j], uniqueQuestions[i]] <- res
            
        }
    }
    
    diag(resultForms) <- 0

    return (resultForms)

}

mTest <- distancesBetweenForms(set)

##set2 <- set[set$question %in% c('q1', 'q2') & set$location %in% c('Itziar', 'Maule'), ]
##set2 <- set[ set$location %in% c('Itziar', 'Maule'), ]

test_that('distances between forms correctness', {
    m <- diaMeasure(set, location ~ question, 'answer', 'rdi', 'dice', variable.dist = TRUE)
    m <- as.matrix(m)
    expect_that(round(m[1, 2], 4), equals(0.5722))
    expect_that(round(m[1, 2], 4), equals(round(mTest[1, 2], 4))) 
    expect_that(round(m[3, 4], 4), equals(round(mTest[3, 4], 4)))
    expect_that(round(m[1, 3], 4), equals(round(mTest[1, 3], 4)))
})



## 1, 2 -> 1/2
## 3, 4 -> (1 + 2 + 6) / 3
## 5, 6 -> (0 + 1) / 2
##m <- diaMeasure(set, location ~ question, 'answer', 'rdi', variable.dist = TRUE)
