

# R packages --------------------------------------------------------------

if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")

pacman::p_load(
  ellmer,
  here,
  data.table
  )


# Setting working directory using here R package --------------------------

# Load objects ------------------------------------------------------------

B         <- readRDS(here("CongresoR26_agent","B.rds"))
scenarios <- readRDS(here("CongresoR26_agent","scenarios.rds"))
X         <- readRDS(here("CongresoR26_agent","X.rds"))
GVA       <- readRDS(here("CongresoR26_agent","GVA.rds"))

# Create list of test shocks  ---------------------------------------------

testShocks <- scenarios[,1:4]
shocksNames <- names(testShocks)[2:4]

# Define function -----------------------------------------------------------

shockEffect <- function(inverse, variable, output, responseVariable){
  
  #Use the inverse of the technical coefficient matrix, an exogenous shock vector
  #and a response variable in terms of output, to evaluate the effect of the
  #shock on the response variable
  #
  #Written by: Kamal Romero (kamal.romero-sookoo@ext.ec.europa.eu)
  #Written on: 06th Feb 2025
  # Args:
  #   inverse: Inverse of the technical coefficient matrix
  #   variable: Shock variable
  #   output: Output data.table
  #   responseVariable: Response variable over which you will evaluate the effect
  #   of the shock
  # Returns:
  #   A list of two data.table objects with the change in output and the
  #   response variable
  
  #Convert the data.table in matrix
  variable2 <- variable |> 
    as.matrix(rownames = TRUE)
  
  #Ensure row order matches between inverse and variable2
  variable2 <- variable2[match(rownames(inverse), rownames(variable2)), , drop = FALSE]
  
  #Compute the new output
  newOutput <- inverse%*%variable2
  
  #Merge the old and new output to compute the difference
  changeOutput <- merge.data.table(x = output,
                                   y = newOutput|>
                                     as.data.table(keep.rownames="REGION"),
                                   by = "REGION")
  
  colnames(changeOutput) <- c("REGION",  "OUTPUT", "NEW_OUTPUT")
  
  changeOutput[, DIFFERENCE := OUTPUT - NEW_OUTPUT]
  
  
  #Merge the response variable with the output and compute the variable as a fraction
  #of output, take into account that output are not supposed to be zero and that
  #the fraction cannot be negative
  Vartemp <- merge(x = responseVariable,
                   y = output,
                   by = "REGION",
                   all = TRUE)
  
  Vartemp[, coeff := OBS_VALUE.x/OBS_VALUE.y]
  Vartemp[, coeff := ifelse(OBS_VALUE.y == 0, 0, coeff)]
  Vartemp[, coeff := ifelse(coeff < 0, 0, coeff)]
  
  #Vartemp2 <- Vartemp[, .(REGION, coeff)]
  
  #Create a named vector to be diagonalized
  diagVarTemp <- Vartemp$coeff
  names(diagVarTemp) <- Vartemp$REGION
  
  #Diagonalize vector
  diagVar <- diag(diagVarTemp)
  dimnames(diagVar) <- list(names(diagVarTemp), names(diagVarTemp))
  
  #Compute the effect of the demand shock on the variable multiplying the diagonal
  #matrix times the new output
  #newVar <- diagVar%*%newOutput
  newOutput <- newOutput[match(names(diagVarTemp), rownames(newOutput)), , drop = FALSE]
  newVar <- diagVarTemp * as.vector(newOutput)
  names(newVar) <- names(diagVarTemp)
  
  diffRespVar <- merge.data.table(
    x = responseVariable,
    y = data.table(REGION = names(newVar), AFTER = as.vector(newVar)),
    by = "REGION"
  )
  
  setnames(diffRespVar, c("REGION", "BEFORE", "AFTER"))
  
  diffRespVar[, DIFFERENCE := BEFORE - AFTER]
  
  return(list(VarOutput = changeOutput,
              VarResponse = diffRespVar)
  )
  
}

# Test function -----------------------------------------------------------
# test to compare the results obtained by the LLM tool

# Create an empty named list to collect results: 

testEmptyList <- vector(mode = "list", length = 3)
names(testEmptyList) <- shocksNames

#Loop

for (shock in shocksNames) {
  print(shock)
  testEmptyList[[shock]] <- shockEffect(
    inverse = B,
    variable = testShocks[, c("ICIOP_ROW", ..shock)],
    output = X,
    responseVariable = GVA
  )$VarResponse[, c("REGION","DIFFERENCE")] |>
    setnames(old = "DIFFERENCE", shock)
}

# Test results:

GVA_AT_1 <- sum(testEmptyList$AT_1$AT_1)
print(GVA_AT_1)

GVA_BE_1 <- sum(testEmptyList$BE_1$BE_1)
print(GVA_BE_1)

GVA_BG_1 <- sum(testEmptyList$BG_1$BG_1)
print(GVA_BG_1)


# Build shock -------------------------------------------------------------

buildShock <- function(country, product, amount) {
  
  shock <- data.table(ICIOP_ROW = rownames(B), SHOCK = 0)#Where is B?
  shock[ICIOP_ROW == paste(country, product, sep = "_"), SHOCK := amount]
  shock
}

evalShock <- function(country, product, amount) {

  shock <- buildShock(country, product, amount)

  res <- shockEffect(B, shock, X, GVA)$VarResponse   
  res[, c("ctry", "prod") := .(sub("_CPA_.*", "", REGION),
                               sub("^.*_(CPA_)", "\\1", REGION))] #What is done here? Extracting region and product whit grep?

  #total    <- sum(res$DIFFERENCE) #Explain this?
  topCountries <- head(res[, .(impact = sum(DIFFERENCE)), by = .(country = ctry)][order(-impact)], 10)
  topProducts  <- head(res[, .(impact = sum(DIFFERENCE)), by = .(product = prod)][order(-impact)], 10)
  topCells     <- head(res[order(-DIFFERENCE), .(cell = REGION, impact = DIFFERENCE)], 10)
  
  out <- list(
    shock = list(country = country, product = product, amount = amount), 
    #totalImpact = total,
    topCountries = topCountries, 
    topProducts = topProducts, 
    topCells = topCells
  )
  
  
# Tools must return text: convert the list to JSON
  jsonlite::toJSON(
    out,
    auto_unbox = TRUE,
    digits = 3
  )
}

# Define tool -------------------------------------------------------------

toolCompare <- tool(
  evalShock,
  name = "evalShock",
  description = "Impact on value added (million euros) of an increase in final demand for ONE product of ONE country. Call it once per shock. Returns the total impact and the top 5 countries and products.",
  arguments = list(
    country = type_string("Country code, e.g. 'ES'"),
    product = type_string("CPA product code, e.g. 'CPA_C29'"),
    amount  = type_number("Increase in final demand")
  )
)

# To see tool information: 

toolCompare@name
toolCompare@description
toolCompare@arguments


# System prompt -----------------------------------------------------------

prompt <-  glue::trim(
"
You are an economic analyst with expertise in input-output analysis. Specifically, in evaluating the effects of demand shocks on the value added using multiplier analisys.
Call evalShock once per shock. In case more than one shock is provided finish with a two-sentence conclusion: which scenario is the most favourable and why. 
If only one shock is provided, just describe the which countries are more benefit by this shock and sectors globally
"
)

# Connect to LLM ----------------------------------------------------------

model <- chat_openai_compatible(
  base_url = "https://api-gpt.jrc.ec.europa.eu/v1",
  model = "glm-5.3-flash",
  #credentials = function() Sys.getenv("GPT_API_KEY"),
  credentials = function() Sys.getenv("OPENAI_API_KEY"),
  system_prompt = prompt
)

model <- chat_openai(system_prompt = prompt, model = "gpt-6.2-sol")
?set_system_prompt
model$chat("Hello, works?")

# Register tool -----------------------------------------------------------

model$register_tool(toolCompare)

# Chat with tool ----------------------------------------------------------

# Live chat 
#live_console(model)
live_browser(model)

# Ask by script
chat01 <- model$chat

chat01("Compare: Austria increases its demand for CPA_C29 by 674 and
            Belgica increases its demand for CPA_C29 by 172.38.")

chat01("Diga cual es impacto de un incremento de la demanda del producto  CPA_C29 en Austria por 674 unidades en el resto de las economias de la zona euro y el impacto global por sectores")


# Tests -------------------------------------------------------------------

scenarios_nonzero <- melt.data.table(
  scenarios,
  id.vars = "ICIOP_ROW",
  variable.name = "scenario",
  value.name = "value"
)[value != 0]
nrow(scenarios_nonzero)

scenarios_nonzero


