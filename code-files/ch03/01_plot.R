v = read.table("Billianaires.TXT", sep = ",", header = T, na.strings = "-")


# Histogram / 直方图

par(mfrow = c(1, 2))

hist(v$Age, main = "", xlab = "Age")

hist(v$Net.Worth, main = "", xlab = "Net Worth")


# Box / 盒型图

w = v[v[, 6] == "United States" |
    v[, 6] == "China" |
    v[, 6] == "Japan",]

w[, 6] = as.character(w[, 6])

boxplot(Age ~ Country.of.Citizenship, w)


# Stem / 茎叶图

stem(v[v[, 6] == "China", 4])



#########################

# 散点图

v = read.table("g100.txt", sep = ",", header = T)

plot(v$Assets, v$Sales, pch = 1, col = 1, xlab = "Asserts(Billion $)",
ylab = "Sales(Billion $)", ylim = c(0, 600), xlim = c(- 100, 3000), cex = log(v$Profits))

title("Global 100 Companys' Assets, Sales and log Profits(size of points)")

identify(v$Assets, v$Sales, labels = v$Company)



#########################

# Pie / 饼图

w = read.table("global2000.TXT", sep = ",", header = T)

ws = sort(table(w$Country), de = T)

pie(ws[1 : 10])


# 条形图

barplot(ws[1 : 100], cex.names = .8, main = "Number of Companies Among top 10")



#######################

v = read.table("g100.txt", sep = ",", header = T)

# Need install package `TeachingDemos` first
#   install.packages("TeachingDemos")
library(TeachingDemos)

# 面孔图

q = v[1 : 10, 4 : 7]
row.names(q) = v[1 : 10, 2]

faces(q, nrow = 2, ncol = 5)

# 星图

stars(q, nrow = 2, ncol = 5)



#############################

v = read.table("Billianaires.TXT", sep = ",", header = T, na.strings = "-")

# Need install package `ineq` first
#   install.packages("ineq")
library(ineq)

# Lorenz曲线图

plot(Lc(v[, 3]), col = 'red')

Gini(v[, 3])
