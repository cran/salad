require(salad)
set.seed(1)
x <- dual( runif(5) )

A <- cbind(x,Reduce("cbind", replicate(4, sample(x))))
# matrix( rep(x, 5) + runif(5*5), 5, 5)

B <- A %*% runif(5)
C <- runif(5) %*% A
D <- (A %*% A) [2,]

D0 <- new("dual", x = c(x = 0.726211992519978, init = 0.813528940780877, 
0.778596428262308, 0.70481117327454, 0.863980224686331), d = structure(list(
    x1 = c(x = 0.37212389963679, init = 0.572853363351896, 0.201681931037456, 
    0.572853363351896, 0.37212389963679), x2 = c(x = 0.838362026493996, 
    init = 0.403363862074912, 1.14570672670379, 0.74424779927358, 
    0.838362026493996), x3 = c(x = 0.775487761711702, init = 1.41121538984589, 
    0.945929730311036, 0.668872525217012, 1.51783062634058), 
    x4 = c(x = 0.201681931037456, init = 0.201681931037456, 0.201681931037456, 
    0.201681931037456, 0.201681931037456), x5 = c(x = 2.05391451669857, 
    init = 1.65245558926836, 1.74656981648877, 2.05391451669857, 
    1.31157165206969)), class = "differential"))

B0 <- new("dual", x = structure(c(0.738267808193373, 0.598844717432399, 
1.19337752056918, 1.51025534723466, 0.581285632679134), dim = c(5L, 
1L)), d = structure(list(x1 = structure(c(0.726463089231402, 
0, 0.382387957070023, 0, 0.883081178879365), dim = c(5L, 1L)), 
    x2 = structure(c(0.86969084572047, 0.386114092543721, 0, 
    0.0133903331588954, 0.722736953757703), dim = c(5L, 1L)), 
    x3 = structure(c(0.382387957070023, 0.353739329846576, 0.386114092543721, 
    0.86969084572047, 0), dim = c(5L, 1L)), x4 = structure(c(0, 
    0, 0.883081178879365, 1.10885104630142, 0), dim = c(5L, 1L
    )), x5 = structure(c(0.0133903331588954, 1.25207880279049, 
    0.34034899668768, 0, 0.386114092543721), dim = c(5L, 1L))), class = "differential"))

C0 <- new("dual", x = structure(c(0.969826542549454, 1.17789901175544, 
1.00513196365952, 1.07490334873465, 1.04800777249521), dim = c(1L, 
5L), dimnames = list(NULL, c("x", "init", "", "", ""))), d = structure(list(
    x1 = structure(c(0.482080115471035, 0.827373318606988, 0.493541307048872, 
    0.827373318606988, 0.482080115471035), dim = c(1L, 5L), dimnames = list(
        NULL, c("x", "init", "", "", ""))), x2 = structure(c(0.599565825425088, 
    0.186217601411045, 0.827373318606988, 0.482080115471035, 
    0.827373318606988), dim = c(1L, 5L), dimnames = list(NULL, 
        c("x", "init", "", "", ""))), x3 = structure(c(0.493541307048872, 
    0.599565825425088, 0.482080115471035, 0.186217601411045, 
    0.599565825425088), dim = c(1L, 5L), dimnames = list(NULL, 
        c("x", "init", "", "", ""))), x4 = structure(c(0.186217601411045, 
    0.493541307048872, 0.186217601411045, 0.493541307048872, 
    0.186217601411045), dim = c(1L, 5L), dimnames = list(NULL, 
        c("x", "init", "", "", ""))), x5 = structure(c(0.827373318606988, 
    0.482080115471035, 0.599565825425088, 0.599565825425088, 
    0.493541307048872), dim = c(1L, 5L), dimnames = list(NULL, 
        c("x", "init", "", "", "")))), class = "differential"))

stopifnot( salad:::test_near_equality(B, B0) )
stopifnot( salad:::test_near_equality(C, C0) )
stopifnot( salad:::test_near_equality(D, D0) )

# mb <- microbenchmark::microbenchmark( A %*% runif(5), runif(5) %*% A, A %*% A )
# print(mb)
