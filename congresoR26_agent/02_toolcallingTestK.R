

# R packages --------------------------------------------------------------

if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")

pacman::p_load(ellmer, 
               #here,
               data.table,
               ggplot2)


# Load objects ------------------------------------------------------------

B         <- readRDS("rrfAutomotive/congresoR/B.rds")
scenarios <- readRDS("rrfAutomotive/congresoR/scenarios.rds")
X         <- readRDS("rrfAutomotive/congresoR/X.rds")
GVA       <- readRDS("rrfAutomotive/congresoR/GVA.rds")


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

# Define tool -------------------------------------------------------------

toolCompare <- tool(
  function(shock) {
    res <- shockEffect(
    inverse = B,
    variable = testShocks[, c("ICIOP_ROW", ..shock)],
    output = X,
    responseVariable = GVA
  )$VarResponse
  #sum(res$DIFFERENCE, na.rm= TRUE)
  },
  name = "eval_shock",
  description = "Evaluate the effect of a demand shock on the economy. Return the impact on value added.",
  arguments = list(
    shock = type_enum(values = shocksNames, description = "Shock to evaluate")
  )
)

# To see tool information: 

toolCompare@name
toolCompare@description
toolCompare@arguments


# System prompt -----------------------------------------------------------

prompt = "You are an economic analyst with expertise in input-output analysis."
  
# Connect to LLM ----------------------------------------------------------

modelIo <- chat_openai_compatible(
  base_url = "https://api-gpt.jrc.ec.europa.eu/v1",
  model = "glm-5.3-flash",
  credentials = function() Sys.getenv("OPENAI_API_KEY"), 
  system_prompt = prompt
)

modelIo$chat("Hello, works?")

# Register tool -----------------------------------------------------------

modelIo$register_tool(toolCompare)

# Chat with tool ----------------------------------------------------------

# Live chat in console
live_console(model)

# Ask by script
modelIo$chat("Which of the 3 shocks is the most favorable for the french (FR) economy?")

model$chat("Yes please, what sectors are more favoured?")
