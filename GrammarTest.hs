module Main where

import Data.Map as M
import Data.Maybe
import Data.List as L

data Symbol = 
    Literal String
  | RuleRef String (Maybe Int) -- If it's an expanded rule

data Rule = Rule String (Maybe Int) [[Symbol]]

type Input  = (String {- consumed -}, String {- remaining -})
type Inputs = [Input]

pp_sym :: Symbol -> String
pp_sym (Literal l) = "'" ++ l ++ "'"
pp_sym (RuleRef r i) = r ++ maybe "" suff i
    where suff i = "_" ++ show i

pp_seq :: [Symbol] -> String
pp_seq (x:[]) = pp_sym x
pp_seq (x:xs) = pp_sym x ++ " " ++ pp_seq xs

pp_alts :: [[Symbol]] -> String
pp_alts (x:[]) = pp_seq x
pp_alts (x:xs) = pp_seq x ++ " | " ++ pp_alts xs

pp :: Rule -> String
pp (Rule name n alts) = name ++ (maybe "" suff n) ++ " = " ++ pp_alts alts
    where suff i = "_" ++ show i

match_lit :: String -> Inputs -> Inputs
match_lit lit [] = []
match_lit lit ((consumed, remaining):is) | lit `isPrefixOf` remaining =
    (consumed ++ "::" ++ lit, Prelude.drop n remaining):match_lit lit is
    where n = length lit
match_lit lit ((consumed, remaining):is) | otherwise =
    match_lit lit is

match_sym :: Symbol -> Inputs -> Inputs
match_sym (Literal l)   inputs = match_lit  l inputs
match_sym (RuleRef r i) inputs = match_rule r i inputs

match_seq :: [Symbol] -> Inputs -> Inputs
match_seq _          []     = []
match_seq (sym:[])   inputs = match_sym sym inputs
match_seq (sym:syms) inputs = match_seq syms (match_sym sym inputs)

match_alts :: [[Symbol]] -> Inputs -> Inputs
match_alts (alt:[]) inputs = 
    match_seq alt inputs
match_alts (alt:alts) inputs = 
    match_seq alt inputs ++ match_alts alts inputs

match_rule :: String -> Maybe Int -> Inputs -> Inputs
match_rule rule_name i inputs = match_alts alts inputs
    where
        Rule _ _ alts = fromJust $ M.lookup (rule_name, i) rules

isLeftRec :: Rule -> Bool
isLeftRec r@(Rule name i alts) = isLeftRecN name i r

isLeftRecN :: String -> Maybe Int -> Rule -> Bool
isLeftRecN _ _     (Rule _ _ [])   = False
isLeftRecN name si (Rule _ _ alts) = L.foldr (||) False (L.map (isLeftRecSeq name si) alts)

isLeftRecSeq :: String -> Maybe Int -> [Symbol] -> Bool
isLeftRecSeq rule_name si ((Literal _) :seq) = False
isLeftRecSeq rule_name si ((RuleRef rr i):seq) | rr == rule_name && si == i = True
isLeftRecSeq rule_name si ((RuleRef rr i):seq) = isLeftRecN rule_name si refRule
    where refRule = fromJust $ M.lookup (rr, i) rules

para f z (x:xs) = f x xs (para f z xs)
para f z []     = z

mapButLast f = para g [] where
  g x [] r = [x]
  g x _  r = f x : r

-- Replace a rule with multiple rules to remove left recursion.
removeLeftRecursion :: Rule -> [Rule]
removeLeftRecursion (Rule name n alts) = leftDerivs
    where
{- 1 -} createRules = L.map (makeSubRule name) alts -- Make a rule for each alternative
{- 2 -} sortRules   = sortOn (not . isLeftRec) createRules -- Put left recursive rules first
{- 3 -} labelRules  = zipWith applyLabel sortRules [1..] -- Add a subscript for each of the new rules
{- 4 -} incLabels   = L.map (incLeftmostRef name) labelRules -- If a leftmost reference exists to the original rule, increment its index
{- 5 -} leftDerivs  = mapButLast addAltToNext incLabels -- Add an alternative to the next rule

        makeSubRule :: String -> [Symbol] -> Rule
        makeSubRule name seq = Rule name Nothing [seq]

        subSymb :: String -> Maybe Int -> Symbol -> Symbol
        subSymb what i (RuleRef r n) | r == what = RuleRef what i
        subSymb _ _ r = r

        applyLabel :: Rule -> Int -> Rule
        applyLabel (Rule name _ alts) i = 
            Rule name (Just i) (L.map (L.map (subSymb name (Just i))) alts)

        incLeftmostRef :: String -> Rule -> Rule
        incLeftmostRef search (Rule name i (((RuleRef r (Just n)):seq):[])) | search == name = 
            Rule name i [(RuleRef r (Just $ n + 1)):seq]
        incLeftmostRef _ r = r

        addAltToNext :: Rule -> Rule
        addAltToNext (Rule name (Just i) alts) = 
            Rule name (Just i) ([RuleRef name (Just (i + 1))]:alts)

no = Nothing

-- This rule creates infinite recursion (because of leftmost self reference).
rE  = Rule "E"  no [[Literal "a"], [Literal "b"], [RuleRef "E" no, Literal "+", RuleRef "E" no]]
rE' = Rule "E'" no [[RuleRef "E'" no, Literal "+", RuleRef "E'" no], [Literal "a"]]

-- rI  = Rule "Digit" no [[Literal "1"], ]
rF  = Rule "F"  no [[RuleRef "E" no, Literal "+", RuleRef "E" no]]

rI  = Rule "I"  no [[RuleRef "J" no, Literal "+", RuleRef "J" no], [Literal "a"]]
rJ  = Rule "J"  no [[RuleRef "I" no], [Literal "b"]]

-- But these rule produce the same language without left-recursion...
rE1 = Rule "E" (Just 1) [[Literal "a"]]
rE2 = Rule "E" (Just 2) [[RuleRef "E" (Just 1)], [RuleRef "E" (Just 1), Literal "+", RuleRef "E" (Just 2)]]

testRW = L.map pp $ removeLeftRecursion rE

testRWP :: IO ()
testRWP = do
    mapM putStrLn testRW
    return ()

rules :: M.Map (String, Maybe Int) Rule
rules = M.fromList([
        (("E",  no), rE), 
        (("E'", no), rE'), 
        (("E",  Just 1), rE1), 
        (("E",  Just 2), rE2),
        (("I",  no), rI),
        (("J",  no), rJ)
    ])

main :: IO ()
main = do
    putStrLn "Production rules"
    mapM putStrLn (L.map (pp . snd) (M.toList rules))
    putStrLn $ show $ match_rule "E" (Just 2) [("", "a+a+a")]
