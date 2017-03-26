module SectionTree where

import Prelude hiding (concat)
import Data.List (foldl')
import Data.Graph.Inductive.PatriciaTree
  (Gr)
import Data.Graph.Inductive.Graph 
  (LNode
  , LEdge, nodes
  , mkGraph
  , neighbors
  , lab)
import Text.HTML.TagSoup 
  (Tag (TagText))
import Text.HTML.TagSoup.Tree 
  (TagTree (TagBranch, TagLeaf)
  , parseTree
  , renderTree
  )
import Glider.NLP.Language.English.Porter (stem)
import Glider.NLP.Tokenizer
  (getWords
  , tokenize)
import Data.Text 
  (pack
  , unpack
  , concat)
import Data.Maybe
  (mapMaybe)


type SectionTree = Gr SectionTreeNode String
data SectionTreeNode = 
  Root 
  | ContainerNode {
      children :: String
    }
  | Node {
      rawContent :: String, 
      normalizedContent :: String
    } 
  deriving (Eq, Show)
  

htmlToSectionTrees :: String -> SectionTree
htmlToSectionTrees htmlBody = 
  uncurry mkGraph $ buildNormalizedTree 0 1 ([(0, Root)], []) parsed
  where 
    
    parsed :: TagTree String
    parsed = head (parseTree htmlBody)
    
    normalizeContent :: String -> String
    normalizeContent stringContent = 
      (unpack . concat . getWords . tokenize . stem . pack) stringContent
      
    createNewChildNode :: Int 
                          -> Int
                          -> ([LNode SectionTreeNode], [LEdge String])
                          -> String
                          -> String
                          -> String
                          -> ([LNode SectionTreeNode], [LEdge String])
    createNewChildNode parNode curNode (nodes, edges) "" rel children = 
      (
        (curNode, ContainerNode {
          children = children
        }):nodes, 
        (parNode, curNode, rel):edges
      )
    createNewChildNode parNode curNode (nodes, edges) content rel _ = 
      (
        (curNode, Node {
          rawContent = content,
          normalizedContent = normalizeContent content
        }):nodes, 
        (parNode, curNode, rel):edges
      )

    buildNormalizedTree :: Int 
                          -> Int 
                          -> ([LNode SectionTreeNode], [LEdge String])
                          -> TagTree String
                          -> ([LNode SectionTreeNode], [LEdge String])
    buildNormalizedTree parNode curNode x (TagLeaf (TagText content)) = 
      createNewChildNode parNode curNode x content "text" ""
    buildNormalizedTree _ _ x (TagLeaf _) = 
      x
    buildNormalizedTree parNode curNode x (TagBranch rel _ tagTrees) = 
      foldl' 
        reducer
        initSectionTreeTuple 
        (zip tagTrees [firstChildNode..lastChildNode])
      where
        renderedChildren = renderTree tagTrees
        initSectionTreeTuple@((newParNode,_):_, _) = 
          createNewChildNode parNode curNode x "" rel renderedChildren
        firstChildNode = newParNode + 1
        lastChildNode = firstChildNode + (length tagTrees) - 1
        reducer sectionTreeTuple (tagTree, curNode) = 
          buildNormalizedTree newParNode curNode sectionTreeTuple tagTree


sectionTreeToMarkdown :: SectionTree -> String
sectionTreeToMarkdown sectionTree =
  let nodes = (neighbors sectionTree 0)
      lnodes = map (\x -> lab sectionTree x) nodes
  in
  map nodeToText lnodes

nodeToText Nothing = ""
nodeToText (Just Root) = "r"
nodeToText (Just (ContainerNode children)) = children
nodeToText (Just (Node raw _)) = raw

main :: IO ()
main = do
  print $ sectionTreeToMarkdown $ htmlToSectionTrees "<body><span>Test</span><span>Test2<span/></body><end></end>"
      
