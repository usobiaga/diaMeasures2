## the nexts tests are based on the ones in stringdist package and adapted to diaMeasures2
## M-% (M-S-5).

test_set <- function(a, b, method = 'lv', weight){
    f <- function(a, b){
        data.frame(
        location = c('l1', 'l2'),
        question = 'q1',
        answer = c(a, b))
    }
    dfs <- mapply(f, a, b, SIMPLIFY = FALSE)
    r <- sapply(dfs, diaMeasures2::diaMeasure, location ~ question, 'answer', method, weight = weight)
    as.numeric(r)
}



### -------------------------------------------------------------
context("Optimal String Alignment")
test_that("Edge cases in OSA method",{
    expect_equal(test_set( "", "",method='osa'),0)
    expect_equal(test_set( "","a",method='osa'),1)
    expect_equal(test_set("a", "",method='osa'),1)
    expect_equal(test_set("a","a",method='osa'),0)
                                        # Thanks to Frank Binder for poining out this bug
    expect_equal(test_set("ab","aba",method='osa'),1)
                                        # Thanks to Lauri Koobas for pointing out this bug.
    expect_equal(sum(is.na(test_set(c("a", NA, "b", "c"), c("aa", "bb", "cc", "dd")))),1)
})


test_that("transpositions are found",{
  expect_equal(test_set("ab","ba",method='osa'),1)
})

test_that("Shortest argument is recycled",{
   expect_equal(test_set(c('a','b'),'a',method='osa'),c(0,1))
   expect_equal(test_set('a',c('a','b'),method='osa'),c(0,1))
})

test_that("weights are handled correctly",{
   # deletion
   expect_equal(test_set("a","ab", method='osa',weight=c(0.5,1,1,1)),0.5)
   # insertion
   expect_equal(test_set("ab","a" ,method='osa',weight=c(1,0.5,1,1)),0.5)
   # substitution
   expect_equal(test_set("b","a" , method='osa',weight=c(1,1,0.5,1)),0.5)
   # transposition
   expect_equal(test_set("ca","ac",method='osa',weight=c(1,1,1,0.5)),0.5)
   # symmetry property in simple case
   expect_equal(
      test_set("abc","ac",method='osa',weight=c(0.5,1,1,1)),
      test_set("ac","abc",method='osa',weight=c(1,0.5,1,1))
   )
   
  expect_equal( # two deletions from source (b) to target (a)
    test_set("","aa",weight=c(0.5,1,1,1),method="osa"),1
  )
  expect_equal( # two deletions from source (b) to target (a)
    test_set("","aa",weight=c(0.5,1,1,1),method="lv"),1
  )
  expect_equal( # two deletions from source (b) to target (a)
    test_set("","aa",weight=c(0.5,1,1,1),method="dl"),1
  )
  
  # Thanks to Zach Price for reporting this bug.
  expect_equal(
    test_set("ABC", "BC", method = "lv", weight = c(i=.1, d=.1, s=.1)),.1
  )
  expect_equal(
    test_set("ABC", "BC", method = "lv", weight = c(i=.1, d=.1, s=1)),.1
  )
  
  expect_equal(
    test_set("ABC", "BC", method = "osa", weight = c(i=.1, d=.1, s=.1,t=.1)),.1
  )
  expect_equal(
    test_set("ABC", "BC", method = "osa", weight = c(i=.1, d=.1, s=1,t=.1)),.1
  )
  expect_equal(
    test_set("ABC", "BC", method = "dl", weight = c(i=.1, d=.1, s=.1,t=.1)),.1
  )
  expect_equal(
    test_set("ABC", "BC", method = "dl", weight = c(i=.1, d=.1, s=1,t=.1)),.1
  )
  # examples from the paper; Tanks to Nathalia Potocka for reporting.
  expect_equal(test_set("leia","leela",method="lv",weight=c(i=.1,d=1,s=1)),1.1)
  expect_equal(test_set("leia","leela",method="lv",weight=c(i=1,d=.1,s=1)),2)
  expect_equal(test_set("a","b",method="lv",weight=c(i=.1,d=1,s=.3)),.3)
  expect_equal(test_set("a","b",method="osa",weight=c(i=.1,d=1,s=.3,1)),.3)
  expect_equal(test_set("a","b",method="dl",weight=c(i=.1,d=1,s=.3,t=1)),.3)
  expect_equal(test_set("leia","leela",method="dl",weight=c(i=1,d=.1,s=1,t=1)),2)

})
