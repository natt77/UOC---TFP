
# Para la función Corpus()
library(tm)
# Para la función rbind.fill
library(plyr)
library(SnowballC)

#directorio de trabajo
nombreruta <-getwd()


#leemos todos los ficheros con datos de Facebook
fdatos <- ""
file.names <- dir(nombreruta, pattern = ".csv")
for(i in 1:length(file.names)){
  file <- read.csv(file.names[i], header = TRUE, stringsAsFactors = FALSE,encoding = "UTF-8")
  fdatos <- rbind(fdatos, file)
}




#fdatos <- read.csv("ArchivoAsociaciones120816.csv", header=TRUE, sep=",", encoding = "UTF-8")
str(fdatos)
names(fdatos)
head(fdatos,2)

#Extraemos solo la variable que contiene el post_mensaje
linea <- fdatos$post_message

#lo convertimos para eliminar tildes 
linea = iconv(linea, to="ASCII//TRANSLIT")



#creamos corpus
doc.corpus <- Corpus(VectorSource(linea))

# Vamos a ir eliminando/modificando el corpus para quedarnos sólo con las palabras necesarias 
# Transformamos a minúsculas
doc.corpus <- tm_map(doc.corpus, content_transformer(tolower)) 
# Quitamos la puntuación
doc.corpus <- tm_map(doc.corpus, removePunctuation) 
# Quitamos números
doc.corpus <- tm_map(doc.corpus, removeNumbers)
# Quitamos espacios en blanco
doc.corpus <- tm_map(doc.corpus, stripWhitespace)
# Quitamos palabras sin valor analitico, en ingles y español
doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("spanish")) 
doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))  
# Palabras especificas
doc.corpus <- tm_map(doc.corpus, removeWords, c("vomito", "enfermedad","sindrome","asociacion","marfan","paramo","gracias"))   
# sustituimos palabras derivadas -->OJO, abrazo pone abraz
doc.corpus <- tm_map(doc.corpus, stemDocument, language="spanish")


# Indicamos que nuestro corpus es un texto
doc.corpus <- tm_map(doc.corpus, PlainTextDocument) 


# Creamos una matriz de terminos - documentos
TDM <- TermDocumentMatrix(doc.corpus)

# Creamos una matriz de documentos - terminos, traspuesta de la anterior
Doctm <- DocumentTermMatrix(doc.corpus) 
inspect(TDM[38,1])

# Para evitar tener palabras que 
#(2,inf) nos indica la longitud minima de las palabras, por defecto es 3
TDM <- TermDocumentMatrix(doc.corpus, 
       control = list(wordLengths = c(3, Inf))) 
result <- list(tdm = TDM)
result
dim(TDM)

inspect(TDM[1:30,1:10])


#nos quedamos con las que aparecen al menos X veces
palabras.frec <- findFreqTerms(TDM, 100) 


# This makes a matrix that is 10% 0.1 empty space, maximum
#cuanto mayor ponemos el coeficiente más palabras tenemos

dtms <- removeSparseTerms(TDM, 0.99)
dtms
inspect(dtms[1:5,1:5])

library(ggplot2)   
#Vamos a representar gráficamente las palabras y su frecuencia

#Creamos un data frame, con palabras y frequencias
freq1 <- colSums(as.matrix(Doctm))
wf1 <- data.frame(word=names(freq1), freq=freq1) 

#Seleccionamos sólo las que aparecen mas de 200 veces
filtrado <- data.frame(subset(wf1, freq>200))
p <- ggplot(filtrado, aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p 



#*********nube***********

#muestra matriz de terminos
prueba<-as.matrix(TDM)
prueba_freq <- sort(rowSums(prueba), decreasing=TRUE)
prueba_df <- data.frame(word=names(prueba_freq), freq=prueba_freq)

library(wordcloud)
wordcloud(prueba_df$word, prueba_df$freq, min.freq=200, random.color=TRUE, colors=rainbow(7))


##################

### Hierarchal Clustering   

library(cluster)   


#le ponemos a las palaras su propio nombre, para que luego al sacar el dendograma salgan con etiqueta

#nofunciona
#names(dtms$Terms)<-dtms[[6]][1]
# First calculate distance between words

dtms1 <- removeSparseTerms(Doctm, 0.95)
d <- dist(t(dtms1), method="euclidian")   

fit <- hclust(d, method="average")   
plot.new()
plot(fit, hang=-1)

# "k=" defines the number of clusters you are using   
groups <- cutree(fit, k=3) 

# draw dendogram with red borders around the 5 clusters   

rect.hclust(fit, k=3, border="red") 
