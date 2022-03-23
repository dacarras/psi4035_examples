data_ptsd <- psi2301::ptsd_data

data_ex02 <- psi2301::dem_16

# with()

data_ptsd$age

data_ex02[,1]

hist(data_ptsd$age)

with(data_ptsd, hist(age))


data_table_3_2 <- read.table(
text="
person   y    x    x_q   xy   z
1        2    8     64   16   1
2        3    9     81   27   2
3        3    9     81   27   1
4        4   10    100   40   2
5        7    6     36   42   1
6        5    7     49   35   2
7        5    4     16   20   1
8        7    5     25   35   2
9        8    3      9   24   1
10       9    1      1    9   2
11       9    2      4   18   1
12      10    2      4   20   2

",
header=TRUE, stringsAsFactors = FALSE)

