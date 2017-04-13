# ucitavanje podataka
A.data = read.table("zad51r.dat", header = TRUE, sep = " ")
A.data = data.frame(A.data)

# dijagram raspršenja
plot(A.data)
