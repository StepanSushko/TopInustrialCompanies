if (!require("XML")) { install.packages("XML"); require("XML") }
if (!require("ggplot2")) { install.packages("ggplot2"); require("ggplot2") }
#if (!require("RCurl")) { install.packages("RCurl"); require("RCurl") }
if (!require("qdap")) { install.packages("qdap"); require("qdap") }

#if (!require("gridExtra")) { install.packages("gridExtra"); require("gridExtra") }
#if (!require("xlsx")) { install.packages("xlsx"); require("xlsx") }
#if (!require("rJava")) { install.packages("rJava"); require("rJava") }

#if (!require("mclust")) { install.packages("mclust"); require("mclust") }


# Parsing -----

# u = "http://en.wikipedia.org/wiki/World_population"
u = "http://expert.ru/ratings/300-krupnejshih-proizvodstvennyih-kompanij-yuga-rossii-po-itogam-2016-goda/"



doc = htmlParse(u, encoding = "UTF8", asText = T)
tableNodes = getNodeSet(doc, "//table")

tryAsInteger = function(val) {
    #val = xmlValue(node)
    ans = as.integer(gsub(",", ".", val))
    if (is.na(ans))
        val
    else
        ans
    }

del_ref = function(val) { genX(val, "[", "]") }


tb = readHTMLTable(tableNodes[[1]])
colnames(tb) = as.character( unlist( tb[1,] ) )
tb = tb[-c(1),]

#tb = apply(tb, MARGIN = 2, del_ref)
tb = as.data.frame(apply(tb, MARGIN = 2, tryAsInteger))

tb[, c(1, 2, 7, 8, 9, 10, 11)] = apply(tb[, c(1, 2, 7, 8, 9, 10, 11)], MARGIN = 2, as.integer)

#head( tb[,c(1,2,7,8,9,10,11)] )

tb$`Регион` = as.character(tb$`Регион`)
tb[tb$`Регион` == "Крым",4] = "Республика   Крым"
tb[tb$`Регион` == "Чечня", 4] = "Чеченская   Республика"

head(tb, n = 10)

#write.csv2( tb, file = "Выручка.csv", sep = ",", col.names = NA)

#tb = tb[tb$`Отрасль` != "АПК   и пищевая промышленность",]



# Analysis -----

#Отрасли
Показатель = tb$`Выручка в 2016   году, млн руб.`
Показатель = tb$`Чистая прибыль в   2016 году, млн руб.`
Показатель = tb$`Прирост выручки   в 2016 году, %`

df = aggregate( Показатель ~ `Отрасль`, tb, sum)
df = df[order( df$Показатель, decreasing = T),]



ggplot(df) +
    geom_bar(aes(x = 1:dim(df)[1], y = Показатель), stat = "identity") + xlab("") +
    theme(axis.text.x = element_text(angle = 270, vjust = 0.25, hjust = 0, size = 7),
    axis.text.y = element_text(size = 7)) +
    scale_x_reverse( #name = "",
                      breaks = 1:dim(df)[1],
                      labels = df$`Отрасль`) +
                      coord_flip() +
                      scale_y_continuous(breaks = seq(500000, 3000000, 500000),
    labels = as.character(seq(500000, 3000000, 500000)))


# Регионы

Показатель = tb$`Выручка в 2016   году, млн руб.`
Показатель = tb$`Чистая прибыль в   2016 году, млн руб.`
Показатель = tb$`Прирост выручки   в 2016 году, %`

df = aggregate(Показатель ~ `Регион`, tb, sum)
df = df[order(df$Показатель, decreasing = T),]

ggplot(df) +
    geom_bar(aes(x = 1:dim(df)[1], y = Показатель), stat = "identity") + xlab("") +
    theme(axis.text.x = element_text(angle = 270, vjust = 0.25, hjust = 0)) +
    scale_x_continuous(#name = "",
                      breaks = 1:dim(df)[1],
                      labels = df$`Регион`)


# Структура по региону

Region = "Ростовская   область"
Region = "Волгоградская   область"
Region = "Краснодарский   край"

Показатель = (tb[tb$`Регион` == Region,])$`Выручка в 2016   году, млн руб.`
Показатель = (tb[tb$`Регион` == Region,])$`Чистая прибыль в   2016 году, млн руб.`

df = aggregate(Показатель ~ `Отрасль`, tb[tb$`Регион` == Region,], sum)
df = df[order(df$Показатель, decreasing = T),]

ggplot(df) +
    geom_bar(aes(x = 1:dim(df)[1], y = Показатель), stat = "identity") + xlab("") +
    theme(axis.text.x = element_text(angle = 270, vjust = 0.25, hjust = 0, size = 7),
    axis.text.y = element_text(size = 7)) +
    scale_x_reverse(#name = "",
                      breaks = 1:dim(df)[1],
                      labels = df$`Отрасль`) +
                      coord_flip() +
                      scale_y_continuous(breaks = seq(500000, 3000000, 500000),
    labels = as.character(seq(500000, 3000000, 500000))) + title(Region)




# Частота
unique(tb$`Регион`)

df = data.frame(table(as.character(tb$`Регион`)))
df = df[order(df$Freq, decreasing = T),]

ggplot(data = df) +
    geom_bar(aes(x = 1:dim(df)[1], y = Freq), stat = "identity") + xlab("") +
    theme(axis.text.x = element_text(angle = 270, vjust = 0.25, hjust = 0)) +
    scale_x_continuous(#name = "",
                      breaks = 1:dim(df)[1],
                      labels = df$Var1) + ylab("Число компаний")
