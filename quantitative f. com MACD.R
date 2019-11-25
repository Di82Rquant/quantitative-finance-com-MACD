# implementando indicadores
#pacote especial para ativos
require(quantmod)
#pacote para indicadores
require(TTR)
#começo e fim do estudo
startDate <- as.Date("2018-09-11")
endDate   <- as.Date("2019-02-05")
#ativos buscados na internet
tickers <- c("^BVSP","PETR4.SA")

#fonte dos dados
getSymbols(tickers,src = "yahoo",from = startDate, to = endDate)
#grafico de candles
chartSeries(PETR4.SA,TA = NULL)
#indicador
addMACD()
#criando estratégia
macd <- MACD(PETR4.SA$PETR4.SA.Close,nFast = 12,nSlow =
               26,nSig = 9,maType = SMA,percent = F)
View(macd)
#iniciando modelo
tradesRulles <- lag(ifelse(macd$macd < macd$signal,-1,1))
# -1 entrou vendido, ou 1 para compra
#retorno com info do tradesRulles
retornos <- ROC(PETR4.SA$PETR4.SA.Close)*tradesRulles
#ROC = calcula os retornos
#view retornos, tabela com valores de retorno
View(retornos)
#plotando resultados no grafico (data de plote)
retornos <- retornos["2018-09-11/2019-02-05"]
#carteira recebe de forma exp. os retornos
#-1 para valores absolutos
carteira <- exp(cumsum(retornos$PETR4.SA.Close))-1

# o plot não funcionou
plot(carteira,colors(T),col = "blue")
