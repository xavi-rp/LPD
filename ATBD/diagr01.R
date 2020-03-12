

library(DiagrammeR)
library(DiagrammeRsvg)  # to save the graph
library(rsvg)


graph01 <- grViz("
      
      digraph boxes_and_circles {

#graph[rankdir = LR]

node [shape = box
      fontsize = 15
      fontname = 'times-bold']
'Land Productivity Dynamics'
'EO Imagery'

node [shape = box
      fontsize = 12
      fontname = '']
'Phenological\nVariables'

edge[minlen = 3]

{rank = same 
'EO Imagery' -> 'Phenological\nVariables' 
[label = 'Phenolo, etc.']

'Phenological\nVariables' -> 'Land Productivity Dynamics'}

edge[minlen = '']

node [shape = oval
      fontsize = 12
      fontname = '' 
      fixedsize = true
      width = 1.6]
'Combined\nAssessment of:'

edge[color = black] 
'Land Productivity Dynamics' ->
'Combined\nAssessment of:'


node [shape = box
      fixedsize = false
      style = filled
      color = lightblue]
'Long-Term Change Map of Land-Productivity'

edge[color = black] 
      
'Combined\nAssessment of:'->
'Long-Term Change Map of Land-Productivity'
    
  
node [shape = box
      style = filled
      color = orange]
'Current Status Map of Land-Productivity'

edge[color = black] 

'Combined\nAssessment of:' ->
'Current Status Map of Land-Productivity'
      

node [shape = box
      style = ''
      color = lightblue]

'Steadiness Index (Standing Biomass)'
'Baseline Levels of Standing Biomass'
'Standing Biomass State Change'
'Trend Tendency (Slope)'
'Net Change (MTID)'


edge[color = lightblue] 

'Long-Term Change Map of Land-Productivity' -> 
{'Steadiness Index (Standing Biomass)' 
'Baseline Levels of Standing Biomass'
'Standing Biomass State Change'}


'Steadiness Index (Standing Biomass)' ->
{'Trend Tendency (Slope)'
'Net Change (MTID)'}



node [shape = box
      style = ''
      color = orange]
'Ecosystem Functional Types (EFTs)'
'Local Net Scaling'

edge[color = orange] 
      
'Current Status Map of Land-Productivity' ->
'Ecosystem Functional Types (EFTs)' -> 
'Local Net Scaling'
      }

      ")

graph01

export_svg(graph01) %>%
  charToRaw() %>%
  rsvg() %>%
  png::writePNG("/Users/xavi_rp/Documents/D6_LPD/temp_results/graph01.png")

