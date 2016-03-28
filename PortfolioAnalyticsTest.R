# Some simple tests of PortfolioAnalytics package showing errors/bugs(?) even...
# 
###############################################################################

library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(parallel)
library(doParallel)


Sys.setenv(TZ="UTC")

symbols <- c("VTI", "VGK", "EWJ", "EEM", "IEF", "TLT", "ICF", "RWX", "DBC", "GLD")

transformTo <- "monthly"#NULL#"monthly"#"weekly"
startDateData <- "1995-01-01"
startDateAnalysis <- "1995-01-01"
subset <- "1995-01-01"

data <- new.env()

getSymbols(symbols, from=startDateData, auto.assign=TRUE, env=data)
for (symbol in symbols) {
	assign(symbol, adjustOHLC(get(symbol, envir=data), symbol.name=symbol), envir=data)
}

startDateData <- max(do.call("c", eapply(data, function(x) index(first(x)))))

prices <- do.call(cbind, lapply(symbols, function(symbol) {
														tmpData <- get(symbol, envir=data)
														
														if (!is.null(transformTo)) {
															if (transformTo == "monthly") {
																tmpData <- to.monthly(tmpData, drop.time=FALSE)
															} else if (transformTo == "weekly") {
																tmpData <- to.weekly(tmpData, drop.time=FALSE)
															}
														}
														
														return(Cl(tmpData))
														}))
										
colnames(prices) <- paste(symbols, "close", sep=".")
returns.all <- CalculateReturns(prices)
colnames(returns.all) <- paste(symbols, "return", sep=".")

subsetData <- paste0(startDateData, "/")

returns <- returns.all[subsetData]

init.portfolio <- portfolio.spec(assets=symbols)
init.portfolio <- add.constraint(portfolio=init.portfolio, type = "long_only")
init.portfolio <- add.constraint(init.portfolio, type="box", min=0.0, max=2.0)
init.portfolio <- add.constraint(init.portfolio, type="weight_sum", min_sum=0.99, max_sum=2.01)#, max_sum=1.01)
init.portfolio <- add.objective(init.portfolio, type="return", name="mean", multiplier=0)

volatilityTarget <- 0.15
naiveRP.portfolio <- init.portfolio
naiveRP.portfolio <- add.objective(naiveRP.portfolio, type="risk_budget", name="var", min_concentration=TRUE)
# TODO: If I enable the following objevtive (portfolio volatility target) the subsequent code (from line 74) does not run and results are not as expected
naiveRP.portfolio <- add.objective(naiveRP.portfolio, type="risk", name="StdDev", target=volatilityTarget, enabled=TRUE)

numberOfCores <- detectCores(logical=FALSE)
cl <- makeCluster(numberOfCores)
registerDoParallel(cl)

naiveRP <- optimize.portfolio(returns, naiveRP.portfolio, optimize_method="DEoptim", search_size=5000, trace=TRUE, traceDE=0)

stopCluster(cl)

# TODO: This does not work if volatility target objective is enabled:
#Error in pct_contrib[[idx[ii]]] : attempt to select less than one element
chart.RiskBudget(naiveRP, 
				 match.col="var",
				 main="Equal var Component Contribution", 
				 risk.type="percentage",#"absolute" 
				 neighbors=NULL)

# TODO: Does not work at all:
#Error in applyFUN(R = R, weights = wts, FUN = risk.col) : 
#		argument "arguments" is missing, with no default
chart.Concentration(naiveRP, 
					risk.col='var',#'ES'
					conc.type="pct_contrib", #"weights",#c("weights", "pct_contrib")
					chart.assets=TRUE)

charts.PerformanceSummary(Return.portfolio(returns, extractWeights(naiveRP)))

chart.Weights(naiveRP, main="Naive Risk Parity Weights")

# Rebalancing parameters
# Set rebalancing frequency
rebalanceFrequency <- "months"
# Training Period
trainingPeriod <- 24
# ROlling window
rollingWindow <- 24

numberOfCores <- detectCores(logical=FALSE)
cl <- makeCluster(numberOfCores)
registerDoParallel(cl)

naiveRP.rebalanced <- optimize.portfolio.rebalancing(returns, 
													 naiveRP.portfolio, 
													 rebalance_on=rebalanceFrequency, 
													 training_period=trainingPeriod, 
													 rolling_window=rollingWindow, 
													 optimize_method="DEoptim", 
													 search_size=5000, 
													 traceDE=0,
													 message=TRUE)

stopCluster(cl)

# TODO: There is an issue with extractWeights function
#> extractWeights(naiveRP.rebalanced)
#Error in as.POSIXlt.character(x, tz, ...) : 
#		character string is not in a standard unambiguous format
# TODO: How to get returns actually?
charts.PerformanceSummary(Return.portfolio(returns, extractWeights(naiveRP.rebalanced), rebalance_on=rebalanceFrequency))

# TODO: Fails
#Error in as.POSIXlt.character(x, tz, ...) : 
#		character string is not in a standard unambiguous format
chart.Weights(naiveRP.rebalanced, main="Naive Risk Parity Weights", col=bluemono)

# TODO: Fails
#Error in as.POSIXlt.character(x, tz, ...) : 
#		character string is not in a standard unambiguous format
chart.RiskBudget(naiveRP.rebalanced, risk.type="pct_contrib")

sessionInfo()
