{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintLI where

-- pretty-printer generated by the BNF converter

import AbsLI
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])


instance Print Program where
  prt i e = case e of
    Prog declarations -> prPrec i 0 (concatD [prt 0 declarations])

instance Print Declaration where
  prt i e = case e of
    DecF function -> prPrec i 0 (concatD [prt 0 function])
    DecGV assigment -> prPrec i 0 (concatD [prt 0 assigment])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print Function where
  prt i e = case e of
    Fun id ids stms -> prPrec i 0 (concatD [prt 0 id, doc (showString "("), prt 0 ids, doc (showString ")"), doc (showString "{"), prt 0 stms, doc (showString "}")])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print Assigment where
  prt i e = case e of
    SAsss id exp -> prPrec i 0 (concatD [prt 0 id, doc (showString "="), prt 0 exp, doc (showString ";")])

instance Print Stm where
  prt i e = case e of
    SAss assigment -> prPrec i 0 (concatD [prt 0 assigment])
    SBlock stms -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stms, doc (showString "}")])
    SWhile exp stm -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 exp, doc (showString ")"), prt 0 stm])
    SReturn exp -> prPrec i 0 (concatD [doc (showString "return"), prt 0 exp, doc (showString ";")])
    SIf exp stm1 stm2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 exp, doc (showString ")"), doc (showString "then"), prt 0 stm1, doc (showString "else"), prt 0 stm2])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print Exp where
  prt i e = case e of
    EOr exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, doc (showString "||"), prt 2 exp2])
    EAnd exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "&&"), prt 3 exp2])
    ENot exp -> prPrec i 3 (concatD [doc (showString "!"), prt 3 exp])
    ECon exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "++"), prt 5 exp2])
    EAdd exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "+"), prt 5 exp2])
    ESub exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "-"), prt 5 exp2])
    EMul exp1 exp2 -> prPrec i 5 (concatD [prt 5 exp1, doc (showString "*"), prt 6 exp2])
    EDiv exp1 exp2 -> prPrec i 5 (concatD [prt 5 exp1, doc (showString "/"), prt 6 exp2])
    Call id exps -> prPrec i 6 (concatD [prt 0 id, doc (showString "("), prt 0 exps, doc (showString ")")])
    EInt n -> prPrec i 7 (concatD [prt 0 n])
    EVar id -> prPrec i 7 (concatD [prt 0 id])
    EStr str -> prPrec i 7 (concatD [prt 0 str])
    ETrue -> prPrec i 7 (concatD [doc (showString "true")])
    EFalse -> prPrec i 7 (concatD [doc (showString "false")])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])

