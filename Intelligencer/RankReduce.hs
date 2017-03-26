module RankReduce where

import Data.List (foldl')
import NLP.Similarity.VectorSim 
  (TermVector (TermVector)
  , sum
  , addVectors)
import Data.Graph.Inductive.Graph 
  (neighbors
  , context
  , subgraph)
import SectionTree 
  (SectionTree
  , SectionTreeNode (Root, ContainerNode)
  , SectionGraph)


importantNodes :: SectionTree -> [SectionTree]
importantNodes (rootNode, graph) = 
  getShallowTextNodes rootNode graph
  where 
    getShallowTextNodes :: Int 
                          -> SectionGraph 
                          -> [SectionTree]
    getShallowTextNodes rootNode sectionGraph = 
      foldl'
        (++)
        []
        (map nodeProcessor (neighbors sectionGraph rootNode))
      where 
        nodeProcessor :: Int -> [SectionTree]
        nodeProcessor node = case (context sectionGraph node) of 
          (_, 
            _, 
            Root,
            adjacents) -> 
              foldl' 
                (++) 
                [] 
                (map nodeProcessor (adjacentsTransform adjacents))
          (_, 
            _, 
            ContainerNode {},
            adjacents) -> 
              foldl' 
                (++) 
                [] 
                (map nodeProcessor (adjacentsTransform adjacents))
          ((_, parNode):_, 
            node, 
            textNode, 
            adjacents) -> [(parNode, (subgraph [parNode] sectionGraph))]
          where
            adjacentsTransform :: [(String, Int)] -> [Int]
            adjacentsTransform adjacents = 
              map (\(_, node) -> node) adjacents
  