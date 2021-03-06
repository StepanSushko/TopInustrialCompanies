if (!require("XML")) { install.packages("XML"); require("XML") }
if (!require("ggplot2")) { install.packages("ggplot2"); require("ggplot2") }
if (!require("RCurl")) { install.packages("RCurl"); require("RCurl") }
if (!require("qdap")) { install.packages("qdap"); require("qdap") }

#if (!require("gridExtra")) { install.packages("gridExtra"); require("gridExtra") }
#if (!require("xlsx")) { install.packages("xlsx"); require("xlsx") }
#if (!require("rJava")) { install.packages("rJava"); require("rJava") }

#if (!require("mclust")) { install.packages("mclust"); require("mclust") }

plotDir = "C:/Users/stepa/OneDrive/DataScience/Expert_Parcer/Expert_Parcer/Plots"

# Parsing -----

# u = "http://en.wikipedia.org/wiki/World_population"
u = "http://expert.ru/ratings/300-krupnejshih-proizvodstvennyih-kompanij-yuga-rossii-po-itogam-2016-goda/"

u <- getURL(u)

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

tb$`������` = as.character(tb$`������`)
tb[tb$`������` == "����",4] = "����������   ����"
tb[tb$`������` == "�����", 4] = "���������   ����������"

head(tb, n = 10)

#write.csv2( tb, file = "�������.csv", sep = ",", col.names = NA)

#tb = tb[tb$`�������` != "���   � ������� ��������������",]



# Analysis -----

#�������
���������� = tb$`������� � 2016   ����, ��� ���.`
���������� = tb$`������ ������� �   2016 ����, ��� ���.`
���������� = tb$`������� �������   �2016 ����, %`

df = aggregate( ���������� ~ `�������`, tb, sum)
df = df[order( df$����������, decreasing = T),]



ggplot(df) +
    geom_bar(aes(x = 1:dim(df)[1], y = ����������), stat = "identity") + xlab("") +
    theme(axis.text.x = element_text(angle = 270, vjust = 0.25, hjust = 0, size = 7),
    axis.text.y = element_text(size = 7)) +
    scale_x_reverse( #name = "",
                      breaks = 1:dim(df)[1],
                      labels = df$`�������`) +
                      coord_flip() +
                      scale_y_continuous(breaks = seq(0, 3000000, 100000),
    labels = as.character( as.character(seq(00, 3000000, 100000))))


# �������

���������� = tb$`������� � 2016   ����, ��� ���.`
���������� = tb$`������ ������� �   2016 ����, ��� ���.`
���������� = tb$`������� �������   �2016 ����, %`

df = aggregate(���������� ~ `������`, tb, sum)
df = df[order(df$����������, decreasing = T),]


png(filename = file.path(plotDir, "Regions_ranking.png"), width = 600, height = 400, units = "px", pointsize = 24, bg = "white", res = 100, family = "", restoreConsole = TRUE) #, type = c("cairo-png"))

ggplot(df) +
    geom_bar(aes(x = 1:dim(df)[1], y = ����������), stat = "identity") + xlab("") +
    theme(axis.text.x = element_text(angle = 270, vjust = 0.25, hjust = 0)) +
    scale_x_continuous(#name = "",
                      breaks = 1:dim(df)[1],
    labels = df$`������`) + ggtitle("���������� ������� � 2016 ����, ��� ���.") + ylab("") + annotate(geom = "text", x = 7, y = 250000, xend = Inf, yend = Inf, label = '������ �����', color = 'white', angle = 45, fontface = 'bold', size = 6, alpha = 0.5, family = 'Verdana')

dev.off()


# ��������� �� �������

Region = "����������   �������"
Region = "�������������   �������"
Region = "�������������   ����"

���������� = (tb[tb$`������` == Region,])$`������� � 2016   ����, ��� ���.`
#���������� = (tb[tb$`������` == Region,])$`������ ������� �   2016 ����, ��� ���.`

df = aggregate(���������� ~ `�������`, tb[tb$`������` == Region,], sum)
df = df[order(df$����������, decreasing = T),]


png(filename = file.path(plotDir, "Region_vs_revenue_VO.png"), width = 600, height = 400, units = "px", pointsize = 24, bg = "white", res = 100, family = "", restoreConsole = TRUE) #, type = c("cairo-png"))

ggplot(df) +
    geom_bar(aes(x = 1:dim(df)[1], y = ����������), stat = "identity") + xlab("") +
    theme(axis.text.x = element_text(angle = 270, vjust = 0.25, hjust = 0, size = 7),
    axis.text.y = element_text(size = 7)) +
    scale_x_reverse(#name = "",
                      breaks = 1:dim(df)[1],
                      labels = df$`�������`) +
                      coord_flip() +
                      #scale_y_continuous(
                      #    breaks = seq(500000, 3000000, 500000),
                      #    labels = as.character(seq(500000, 3000000, 500000))) +
    ggtitle(Region) + ylab("������� � 2016   ����, ��� ���.") +
    annotate(geom = "text", x = 4, y = 55000, xend = Inf, yend = Inf, label = '������ �����', color = 'white', angle = 45, fontface = 'bold', size = 6, alpha = 0.5, family = 'Verdana')

dev.off()


# �������
unique(tb$`������`)

df = data.frame(table(as.character(tb$`������`)))
df = df[order(df$Freq, decreasing = T),]

ggplot(data = df) +
    geom_bar(aes(x = 1:dim(df)[1], y = Freq), stat = "identity") + xlab("") +
    theme(axis.text.x = element_text(angle = 270, vjust = 0.25, hjust = 0)) +
    scale_x_continuous(#name = "",
                      breaks = 1:dim(df)[1],
                      labels = df$Var1) + ylab("����� ��������")
