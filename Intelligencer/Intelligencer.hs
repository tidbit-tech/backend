module Intelligencer where 

import Data.List (foldl')
import RankReduce (importantNodes)
import SectionTree (htmlToSectionTrees, sectionTreeToMarkdown)


intelligencer :: String -> String
intelligencer htmlContent = 
  foldl'
    (++)
    ""
    (map 
      sectionTreeToMarkdown 
      (importantNodes (htmlToSectionTrees htmlContent)))
