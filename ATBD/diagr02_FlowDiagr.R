

library(DiagrammeR)
library(DiagrammeRsvg)  # to save the graph
library(rsvg)


graph02 <- grViz("
      
      digraph flowchart {

#graph[rankdir = LR]

node [shape = box
      fontsize = 15
      fontname = 'times-bold'
      fixedsize = ''
      width = ''] 
'EO Imagery'


node [shape = box
      fontsize = 12
      fontname = ''
      fixedsize = ''
      width = '']
'Phenological\nProductivity\nVariables'

edge[minlen = 3]

{rank = same 
'EO Imagery' -> 'Phenological\nProductivity\nVariables' 
[label = '']}


#edge [arrowhead = vee]

node [shape = box
      style = ''
      color = black]

'Standing Biomass'
'Season Integral'
'Season Beginning Day'
'Season End Day'
'Season Length'

node [shape = box
      style = ''
      color = DeepSkyBlue]

'Steadiness Index'
'Baseline Levels'
'State Change'
'Trend Tendency\n(Slope)'
'Net Change\n(MTID)'


edge[color = black] 

'Phenological\nProductivity\nVariables' -> 'Standing Biomass'
'Phenological\nProductivity\nVariables' -> 'Season Integral'
'Phenological\nProductivity\nVariables' -> 'Season Beginning Day'
'Phenological\nProductivity\nVariables' -> 'Season End Day'
'Phenological\nProductivity\nVariables' -> 'Season Length'


edge[minlen = ''
     color = DeepSkyBlue]


'Standing Biomass' -> 'Trend Tendency\n(Slope)'
'Standing Biomass' -> 'Net Change\n(MTID)'

'Trend Tendency\n(Slope)' -> 'Steadiness Index'
'Net Change\n(MTID)'      -> 'Steadiness Index'

{rank = same 
'Steadiness Index' 
'Baseline Levels' 
'State Change'}

'Standing Biomass' -> 'Baseline Levels'
'Standing Biomass' -> 'State Change'


node [shape = box
      fixedsize = false
      style = filled
      color = DeepSkyBlue]
'Long-Term Change Map of Land-Productivity'

node [shape = box
      fixedsize = false
      style = filled
      color = orange]
'Current Status Map of Land-Productivity'


'Steadiness Index' -> 'Long-Term Change Map of Land-Productivity'
'Baseline Levels'  -> 'Long-Term Change Map of Land-Productivity'
'State Change'     -> 'Long-Term Change Map of Land-Productivity'


node [shape = box
      style = ''
      color = orange]

'Ecosystem Functional Types (EFTs)'
'Local Net Scaling'


edge[minlen = ''
     color = orange]

'Standing Biomass'     -> 'Ecosystem Functional Types (EFTs)'
'Season Integral'      -> 'Ecosystem Functional Types (EFTs)'
'Season Beginning Day' -> 'Ecosystem Functional Types (EFTs)'
'Season End Day'       -> 'Ecosystem Functional Types (EFTs)'
'Season Length'        -> 'Ecosystem Functional Types (EFTs)'


{rank = same 
'Long-Term Change Map of Land-Productivity'
'Current Status Map of Land-Productivity'}


'Ecosystem Functional Types (EFTs)' -> 
'Local Net Scaling' -> 
'Current Status Map of Land-Productivity'


node [shape = oval
      fontsize = 12
      fontname = '' 
      fixedsize = true
      width = 1.6
      color = black]
'Combined\nAssessment'

edge[color = black
     arrowhead = normal]

'Current Status Map of Land-Productivity'   -> 'Combined\nAssessment'
'Long-Term Change Map of Land-Productivity' -> 'Combined\nAssessment'


node [shape = box
      fontsize = 15
      fontname = 'times-bold'
      fixedsize = true
      width = 3] 
'Land Productivity Dynamics\n indicator'

'Combined\nAssessment' -> 'Land Productivity Dynamics\n indicator'


{rank = same 
'Net Change\n(MTID)'
'Ecosystem Functional Types (EFTs)'}

edge[color = white
     minlen = 4]
'Net Change\n(MTID)' ->
'Ecosystem Functional Types (EFTs)'


edge[color = white
     minlen = 2]
'Long-Term Change Map of Land-Productivity' ->
'Current Status Map of Land-Productivity'


      }

      ")

graph02

export_svg(graph02) %>%
  charToRaw() %>%
  rsvg() %>%
  png::writePNG("/Users/xavi_rp/Documents/D6_LPD/LPD/ATBD/graph02.png", dpi = 96)

