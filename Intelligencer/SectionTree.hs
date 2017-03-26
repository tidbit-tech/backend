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
  (Text
  , pack)
import Text.Pandoc


type SectionGraph = Gr SectionTreeNode String
type SectionTree = (Int, SectionGraph)
data SectionTreeNode = 
  Root 
  | ContainerNode {
      children :: String
    }
  | Node {
      rawContent :: String, 
      normalizedContent :: [Text]
    } 
  deriving (Eq, Show)
  

htmlToSectionTrees :: String -> SectionTree
htmlToSectionTrees htmlBody = 
  (0, uncurry mkGraph $ buildNormalizedTree 0 1 ([(0, Root)], []) parsed)
  where 
    
    parsed :: TagTree String
    parsed = head (parseTree htmlBody)
    
    normalizeContent :: String -> [Text]
    normalizeContent stringContent = 
      (getWords . tokenize . stem . pack) stringContent
      
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
      folded
      where
        folded = foldl' 
          reducer
          initSectionTreeTuple 
          (zip tagTrees [firstChildNode..lastChildNode])
        initSectionTreeTuple@((newParNode,_):_, _) = 
          createNewChildNode parNode curNode x "" rel renderedChildren
        renderedChildren = renderTree tagTrees
        firstChildNode = newParNode + 1
        lastChildNode = firstChildNode + (length tagTrees) - 1
        reducer sectionTreeTuple (tagTree, curNode) = 
          buildNormalizedTree newParNode curNode sectionTreeTuple tagTree

sectionTreeToMarkdown :: SectionTree -> [String]
sectionTreeToMarkdown sectionTree =
  let lnodes = (toLnodes $ getG sectionTree)
  in
  map nodeToText lnodes

getG :: SectionTree -> SectionGraph
getG t = snd t 

nodeToText :: Maybe SectionTreeNode -> String
nodeToText Nothing = "nothin"
nodeToText (Just Root) = "root"
nodeToText (Just (ContainerNode children)) = children
nodeToText (Just (Node raw _)) = raw

toLnodes t = map (\x -> lab t x) (neighbors t 1)

htmlToMD string = writeMarkdown def (readHTML def string)

main = do
  print $ sectionTreeToMarkdown $ htmlToSectionTrees "<body></body>" 
  

