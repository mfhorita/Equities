# Big Data na Pratica - Buscando Dados para Analise Financeira

# http://www.quantmod.com
# http://www.bmfbovespa.com.br/pt_br/produtos/indices/indices-amplos/indice-ibovespa-ibovespa-composicao-da-carteira.htm

#Sys.setenv(TZ="America/Sao_Paulo")
#Sys.setenv(TZ="America/New_York")
Sys.setenv(TZ="GMT")
Sys.getenv("TZ")

# Instalar e carregar os pacotes
install.packages("quantmod")
install.packages("xts")
install.packages("moments")
library(quantmod)
library(xts)
library(moments)

# xlsx
install.packages("rJava", type = "source")
install.packages("xlsx")
library(rJava)
library(xlsx)

#dir()
#setwd('C:/GitHub/Equities')
getwd()

#help(getSymbols)
#getSymbols("^BVSP", src = "yahoo", from = startDate, to = endDate)

# Selecao do periodo de analise
startDate = as.Date(paste(format(Sys.Date(), format = "%b-%d-"), 
  as.integer(format.Date(Sys.Date(), "%Y")) - 10, sep=""), format = "%b-%d-%Y")
endDate = Sys.Date()

# Oculta mensagens de alerta
options("getSymbols.yahoo.warning"=FALSE)
options("getSymbols.warning4.0"=FALSE)

#getOption("getSymbols.env")
#getOption("getSymbols.auto.assign")
#help("getOption")

pdf("Equities.pdf")

  p <- c(0, 0.35, 0.5, 0.75, 1)
  LISTA <- c('ABEV3','BBAS3','BBDC4','BBSE3','BRAP4','BRFS3','BRKM5','BRML3'
             ,'BVMF3','CCRO3','CIEL3','CMIG4','CPFE3','CPLE6','CSAN3','CSNA3'
             ,'CYRE3','ECOR3','EGIE3','EMBR3','ENBR3','EQTL3','ESTC3','FIBR3'
             ,'GGBR4','GOAU4','HYPE3','JBSS3','KROT3','LAME4','LREN3','MRFG3'
             ,'MRVE3','MULT3','NATU3','PETR4','QUAL3','RADL3','RENT3','SBSP3'
             ,'SUZB5','TIMP3','UGPA3','USIM5','VALE5','VIVT4','WEGE3')
  
  getSymbols('ITSA4.SA', src = "yahoo", from = startDate, to = endDate)
  #help("getSymbols")

  SAIDA <- 'ITSA4.SA'
  #ITSA4.SA[,'ITSA4.SA.Adjusted']
  SAIDA[2:6] <- as.numeric(quantile(na.omit(ITSA4.SA[,'ITSA4.SA.Adjusted']), p, type = 1))
  SAIDA[7] <- as.numeric(tail(ITSA4.SA[,'ITSA4.SA.Adjusted'], n=1L))

  # Preco Atual > 3º Quartil
  if (as.numeric(SAIDA[7]) > as.numeric(SAIDA[5])) {
    SAIDA[8] <- 'VENDER'
  
  # Preco Atual < 3º Quartil
  } else if (as.numeric(SAIDA[7]) < as.numeric(SAIDA[3])) {
    SAIDA[8] <- 'COMPRAR'

  } else {
    SAIDA[8] <- 'NEUTRO'
  }

  class(SAIDA)
  frame <- as.data.frame(SAIDA)

  plot(na.omit(ITSA4.SA[,'ITSA4.SA.Adjusted']), main = 'ITSA4.SA',
       col = "red", xlab = "Data", ylab = "Pregao", major.ticks = 'months',
       minor.ticks = FALSE)
  
  write.xlsx(ITSA4.SA, 'ITSA4.SA.xlsx', sheetName = 'ITSA4.SA')
  class(ITSA4.SA)
  
  iItem <- 1
  for (ACAO in LISTA) {
    
    ACAO <- paste(ACAO, '.SA', sep='')
  
    getSymbols(ACAO, src = "yahoo", from = startDate, to = endDate)
    SERIE = get(ACAO)
  
    SAIDA <- ACAO
    SAIDA[2:6] <- as.numeric(quantile(na.omit(SERIE[,paste(ACAO,'.Adjusted',sep='')]), p, type = 1))
    SAIDA[7] <- as.numeric(tail(SERIE[,paste(ACAO,'.Adjusted',sep='')], n=1L))

    # Preco Atual > 3º Quartil
    if (as.numeric(SAIDA[7]) > as.numeric(SAIDA[5])) {
      SAIDA[8] <- 'VENDER'
      
      # Preco Atual < 3º Quartil
    } else if (as.numeric(SAIDA[7]) < as.numeric(SAIDA[3])) {
      SAIDA[8] <- 'COMPRAR'
      
    } else {
      SAIDA[8] <- 'NEUTRO'
    }
    
    #NameAcao = SERIE[,paste(ACAO,'.Adjusted',sep='')]
    plot(SERIE[,paste(ACAO,'.Adjusted',sep='')], main = ACAO,
         col = "red", xlab = "Data", ylab = "Pregao", major.ticks = 'months',
         minor.ticks = FALSE)
  
    #write.xlsx(SERIE, paste(ACAO, '.xlsx', sep=''), sheetName = ACAO)
    frame[iItem <- iItem + 1] <- as.data.frame(SAIDA)
  }
  
  write.xlsx(frame, "Equities.xlsx", sheetName = "Equity")

dev.off()

# ---------------------------------------------------------------------