library('readxl')
# library("writexl")

name ="Novos_Dados.txt"
name = "Dados_modificados.xlsx" 
diretorio ="D:"
setwd(diretorio)

df2 = read_excel(name);

df3= data.frame(df2)
df =data.frame(df2[,c(2,3,4)])
cols = ncol(df);
rows = nrow(df)
num = ncol(df)*nrow(df)
a = matrix(0, num, 2)
k = 1
for (i in 1:rows)
{
 for (j in 1:cols)
{
   a[k,2]= df[i,j]
   k =k +1
}
}

casos = c('a','a','a',
          'b','b','b',
          'c','c','c',
          'd','d','d',
	    'e','e','e',
          'f','f','f',
          'g','g','g',
          'h','h','h',
          'i','i','i',
	    'j','j','j',
          'k','k','k',
          'l','l','l',
          'm','m','m',
	    'n','n','n',
          'o','o','o',
	    'p','p','p')
df = data.frame(a)
k =1
for (j in 1:cols){
  for (i in 1:rows){
     df[k,1] = casos[k]
     k = k+ 1
  }
 }

print(df)

treatment = df[,1]
value= df[,2]

data=data.frame(treatment,value)


# What is the effect of the treatment on the value ?
model=lm( data$value ~ data$treatment )
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY = TukeyHSD(x=ANOVA, 'data$treatment', conf.level=0.95)


# Tuckey test representation :
plot(TUKEY , las=1 , col="brown")
   
#library("writexl")
#write_xlsx("dados_tensao_prontos.xlsx")

