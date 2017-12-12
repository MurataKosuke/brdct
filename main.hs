{--
  型無しラムダ計算におけるベータ簡約のシミュレータ
-}

import Data.Char
import Data.List
-- import Data.Monad
import System.Environment
import System.Process
-- import System.cmd

-- 型無しラムダ式を代数的データ型として定義
data Term =
    Var String         -- 変数
  | Abs String Term    -- 関数抽象 (λv.M)
  | App Term Term      -- 関数適用 (MN)
  deriving (Eq)

instance Show Term where
  show = printTerm

{-
instance Read Term where
  readsPrec = readTerm $ preReadTerm
-- readsPrec :: Int -> ReadS a
-- readsPrec :: Int -> String -> [(a, String)]
-- ReadS a = String -> [(a, String)]
-}

data Tag = IsVar | IsAbs | PTerm | TTerm deriving (Eq, Show)

partialLexer :: String -> [(String, Tag)]
partialLexerAbs :: String -> [(String, Tag)]
partialLexer [] = []
partialLexer (h:t)
  | h == '('    = if (length f == 1) then [(f, IsVar)] ++ partialLexer s else [(f,  PTerm)] ++ partialLexer s
  | h == '&'    = [(fz, TTerm)] ++ partialLexer sz
  | h == '\\'   = partialLexerAbs t
  | h == ' '    = partialLexer t
  | otherwise   = [([h], IsVar)] ++ partialLexer t
  where
    y = parenCut (h:t) 0
    f = fst y
    s = snd y
    z = break (\x -> x == ' ') (h:t)
    fz = fst z
    sz = snd z
partialLexerAbs [] = []
partialLexerAbs (h:t)
  | h == '.'    = partialLexer t
  | h == ' '    = partialLexerAbs t
  | otherwise   = [([h], IsAbs)] ++ partialLexerAbs t

parser :: String -> Term
parser = parserIn . partialLexer

parserIn :: [(String, Tag)] -> Term
parserIn x = h
  where
    hs = fst . head $ x
    ht = snd . head $ x
    t  = tail x
    h | ht == IsVar    = if t == [] then (Var hs) else parserTerm t (Var hs)
      | ht == PTerm    = if t == [] then parser hs else parserTerm t (parser hs)
      | ht == IsAbs    = (Abs hs (parserIn t))
      | ht == TTerm    = if t == [] then parseTTerm hs else parserTerm t (parseTTerm hs)

parseTTerm :: String -> Term
parseTTerm s
  | s == "&add"    = cTermPlus
  | s == "&plus"   = cTermPlus
  | s == "&times"  = cTermTimes
  | s == "&mult"   = cTermTimes
  | s == "&exp"    = cTermExp
  | s == "&suc"    = parser "\\mnfx.mf(nfx)"
  | s == "&succ"   = parser "\\mnfx.mf(nfx)"
  | s == "&pred"   = parser "\\nfx.n(\\gh.h(gf))(\\u.x)(\\u.u)"
  | s == "&succ"   = parser "\\mnfx.mf(nfx)"
  | s == "&true"   = parser "\\xy.x"
  | s == "&false"  = parser "\\xy.y"
  | s == "&tru"    = parser "\\xy.x"
  | s == "&fls"    = parser "\\xy.y"
  | s == "&test"   = parser "\\lmn.lmn"
  | s == "&and"    = parser "\\bc.bc &false"
  | s == "&or"     = parser "\\bc .b(\\xy.x)c"
  | s == "&not"    = parser "\\b.b(\\xy.y)(\\xy.x)"
  | s == "&pair"   = parser "\\fsb.bfs"
  | s == "&fst"    = parser "\\p.p &true"
  | s == "&snd"    = parser "\\p.p &false"
  | s == "&isZero" = parser "\\n.n(\\x.(\\xy.y))(\\xy.x)"
  | s == "&S"      = parser "\\xyz.xz(yz)"
  | s == "&K"      = parser "\\xy.x"
  | s == "&I"      = parser "\\x.x"
  | s == "&Y"      = parser "(\\f.(\\x.f(xx))(\\x.f(xx)))"
  | otherwise      = numToCTerm (read . tail $ s)

parserTerm :: [(String, Tag)] -> Term -> Term
parserTerm (h:t) acc
  | ht == IsVar     = if t == [] then (App acc (Var hs)) else parserTerm t (App acc (Var hs))
  | ht == PTerm     = if t == [] then (App acc (parser hs)) else parserTerm t (App acc (parser hs))
  | ht == IsAbs     = (Abs hs (parserTerm t acc))
  | ht == TTerm     = if t == [] then (App acc (parseTTerm hs)) else parserTerm t (App acc (parseTTerm hs))
  where
    hs = fst h
    ht = snd h

reverseTerm :: Term -> Term
reverseTerm (Var x) = (Var x)
reverseTerm (App x y) = (App (reverseTerm y) (reverseTerm x))
reverseTerm (Abs x y) = (Abs x (reverseTerm y))

parenCut :: String -> Integer -> (String, String)
parenCut [] _          = ("","")
parenCut ('(':t) 0     = parenCut t 1
parenCut ('(':t) n     = (\(f,s) -> ('(':f,s)) $ parenCut t (n + 1)
parenCut (')':t) 1     = ("",t)
parenCut (')':t) n     = (\(f,s) -> (')':f,s)) $ parenCut t (n - 1)
parenCut (h:t)   n     = (\(f,s) -> (h:f,s)) $ parenCut t n

{-
preReadTerm    :: String -> String
preReadTermAbs :: String -> String
preReadTermApp :: String -> String
preReadTerm [] = []
preReadTerm (h:t)
  | h == '('    = if s == "" then preReadTerm f else preReadTerm f
  | h == '\\'   = preReadTermAbs t
  | otherwise   = preReadTermApp (h:t)
  where
    y = parenCut (h:t) 0
    f = fst y
    s = snd y
preReadTermAbs (h:t)
  | h == '.'    = (preReadTerm t)
  | otherwise   = "(\\" ++ [h] ++ "." ++ (preReadTermAbs t) ++ ")"
preReadTermApp []  = []
preReadTermApp [h] = [h]
preReadTermApp (h:t)
  | h == '('    = preReadTermApp f ++ preReadTermApp s
  | otherwise   = (if s1 == [] then preReadTermAppIn f1 else "(" ++ preReadTermAppIn f1 ++ preReadTerm s1 ++ ")")
  where
    y = parenCut (h:t) 0
    f = fst y
    s = snd y
    z = break (\x -> x == '(') (h:t)
    f1 = fst z
    s1 = snd z

preReadTermAppIn  = reverse . preReadTermAppInPreRev . reverse . preReadTermAppInPre

preReadTermAppInPreRev :: [String] -> String
preReadTermAppInPreRev [] = ""
preReadTermAppInPreRev [e1]    = if length e1 == 1 then e1 else ")" ++ (preReadTermAppInPreRev . reverse . preReadTermAppInPre $ e1) ++ "("
preReadTermAppInPreRev [e1,e2] = ")" ++ preReadTermAppInPreRev [e1] ++ preReadTermAppInPreRev [e2] ++ "("
preReadTermAppInPreRev (h:t) = ")" ++ (preReadTermAppInPreRev . reverse . preReadTermAppInPre $ h) ++ (preReadTermAppInPreRev t) ++ "("

preReadTermAppInPre :: String -> [String]
preReadTermAppInPre [] = []
preReadTermAppInPre [h] = [[h]]
preReadTermAppInPre (h:t)
  | h == '('    = f:(preReadTermAppInPre s)
  | otherwise   = [h]:(preReadTermAppInPre t)
  where
    y = parenCut (h:t) 0
    f = fst y
    s = snd y

parenCut :: String -> Integer -> (String, String)
parenCut [] 0 = ("", "")
parenCut ('(':t) 0     = parenCut t 1
parenCut ('(':t) n     = (\(f,s) -> ('(':f,s)) $ parenCut t (n + 1)
parenCut (')':t) 1     = ("",t)
parenCut (')':t) n     = (\(f,s) -> (')':f,s)) $ parenCut t (n - 1)
parenCut (h:t)   n     = (\(f,s) -> (h:f,s)) $ parenCut t n

readTerm :: String -> Term
readTerm "$add"  = cTermPlus
readTerm "$mult" = cTermTimes
readTerm "$exp"  = cTermExp
readTerm [h]        = (Var [h])
readTerm ('\\':x:'.':t) = (Abs [x] (readTerm t))
readTerm (h:t)
  | h == '('     = if (s /= []) then (App (readTerm f) (readTerm s)) else (readTerm f)
  | otherwise    = (App (Var [h]) (readTerm t))
  where
    y = parenCut (h:t) 0
    f = fst y
    s = snd y
-}

-- 定数としてのラムダ式
numToCTermN :: Integer -> Term
numToCTermN 0 = (Var "x")
numToCTermN n = (App (Var "f") (numToCTermN (n-1)))

numToCTerm :: Integer -> Term
numToCTerm n = (Abs "f" (Abs "x" (numToCTermN n)))

--
cTermPlus :: Term
cTermPlus = (Abs "x" (Abs "y" (Abs "p" (Abs "q" (App (App (Var "x") (Var "p")) (App (App (Var "y") (Var "p")) (Var "q")) )))))

cTermTimes :: Term
cTermTimes = (Abs "x" (Abs "y" (Abs "z" (App (Var "x") (App (Var "y")(Var "z"))))))

cTermExp :: Term
cTermExp = (Abs "x" (Abs "y" (App (Var "y") (Var "x"))))

-- まだリストに与えられていない新しい変数名を作る関数
newVar :: String -> [String] -> String
newVar x l = t
  where
    z = takeWhile isAlpha x
    y = dropWhile isAlpha x
    w = if (foldr (&&) True $ map isNumber y) && (y /= []) then (read y) else -1
    h = head z
    t = head $ dropWhile (\x -> x `elem` l) (map (\x -> [x]) [h..(chr (min (ord 'z') ((ord h) + 3)))] ++ (map (\x -> [x]) ['a'..'z']) ++ [z ++ show (w + k) | k <- [1..]])

-- 自由変数の一覧を取得
fVar :: Term -> [String]
fVar (Var x) = [x]
fVar (App m n) = fVar m ++ fVar n
fVar (Abs x m) = delete x (fVar m)

isFVar :: String -> Term -> Bool
isFVar x m = x `elem` (fVar m)

-- 代入
subst :: Term -> String -> Term -> Term
subst (Var x) y z   = if (x == y) then z else (Var x)
subst (App m n) y l = (App (subst m y l) (subst n y l))
subst (Abs x m) y n =
  if (x == y) then (Abs x m) else
     if (isFVar x n) && (isFVar y m) then (Abs x1 (subst (subst m x (Var x1)) y n)) else (Abs x (subst m y n))
     where
       x1 = newVar x ((fVar m) ++ (fVar n) ++ [y])

-- ベータ基の一覧を返す
beta :: Term -> [Term]
beta (Var x) = []
beta (Abs x m) = beta m
beta (App (Abs x m) n) = (App (Abs x m) n) : (beta m) ++ (beta n)
beta (App m n) = (beta m) ++ (beta n)

-- ベータ基を簡約する
rdct :: Term -> (Maybe Term)
rdct (App (Abs x m) n) = Just (subst m x n)
rdct m = Nothing

-- 一番左にあるベータ基を探して簡約
brdctl :: Term -> (Maybe Term)
brdctl (Var x) = Nothing
brdctl (Abs x m) = brdctl m >>= (\m1 -> Just (Abs x m1))
brdctl (App (Abs x m) n) = Just (subst m x n)
brdctl (App m n) =
  if n1 /= Nothing then n1 else n2
    where
      n1 = brdctl m >>= (\m1 -> Just (App m1 n))
      n2 = brdctl n >>= (\m2 -> Just (App m m2))

-- Call by Nameで簡約
brdctlcbn :: Term -> (Maybe Term)
brdctlcbn (Var x) = Nothing
brdctlcbn (App (Abs x m) n) = Just (subst m x n)
brdctlcbn (App m n) = brdctlcbn m >>= (\m1 -> Just (App m1 n))
brdctlcbn (Abs x n) = Nothing

-- Call by Valueで簡約
brdctlcbv :: Term -> (Maybe Term)
brdctlcbv (Var x) = Nothing
brdctlcbv (Abs x m) = Nothing
brdctlcbv (App (Abs x m) n) =
  if n1 /= Nothing then n1 else Just (subst m x n)
    where
      n1 = brdctlcbv n >>= (\m1 -> Just (App (Abs x m) m1))
brdctlcbv (App m n) = brdctlcbv m >>= (\m1 -> Just (App m1 n))

-- 左にあるベータ基を探して簡約を続ける
brdctsl :: (Term -> (Maybe Term)) -> Term -> [Maybe Term]
brdctsl brdctxx x = [(Just x)] ++ brdctslm brdctxx (Just x)

-- 左にあるベータ基を探して簡約を続ける(内部)
brdctslm :: (Term -> (Maybe Term)) -> Maybe Term -> [Maybe Term]
brdctslm brdctxx Nothing = [Nothing]
brdctslm brdctxx (Just m) = if (v /= Nothing) then v:(brdctslm brdctxx v) else []
  where v = brdctxx m

-- ラムダ式をいい感じに表示
printTerm :: Term -> String
printTermS :: Term -> String
printTerm (Var m) = m
printTerm (Abs x m) = "\\" ++ x ++ printTermS m
printTerm (App (App m n) (Var x)) = printTerm (App m n) ++ x
printTerm (App (App m n) l) = printTerm (App m n) ++ "(" ++ printTerm l ++ ")"
printTerm (App (Var x) (Var y)) = x ++ y
printTerm (App (Var x) n) = x ++ "(" ++ printTerm n ++ ")"
printTerm (App m (Var y)) = "(" ++ printTerm m ++ ")" ++ y
printTerm (App m n) = "(" ++ printTerm m ++ ")(" ++ printTerm n ++ ")"
printTermS (Abs x m) = x ++ printTermS m
printTermS m = "." ++ printTerm m

-- Termの深さ(括弧のネストの深さ)を計算．
depthTerm :: Term -> Int
depthTermS :: Term -> Int
depthTerm (Var m) = 0
depthTerm (Abs x m) = depthTermS m
depthTerm (App (App m n) (Var x)) = depthTerm (App m n)
depthTerm (App (App m n) l) = max (depthTerm (App m n)) ((depthTerm l) + 1)
depthTerm (App (Var x) (Var y)) = 0
depthTerm (App (Var x) n) = (depthTerm n) + 1
depthTerm (App m (Var y)) = (depthTerm m) + 1
depthTerm (App m n) = 1 + max (depthTerm m) (depthTerm n)
depthTermS (Abs x m) = depthTermS m
depthTermS m = depthTerm m

printTermLaTeX :: Term -> String
printTermLaTeX t = printTermLaTeXIn t (depthTerm t)

printTermLaTeXIn :: Term -> Int -> String
printTermSLaTeXIn :: Term -> Int -> String
printTermLaTeXIn (Var m) size = m
printTermLaTeXIn (Abs x m) size = "\\lambda " ++ x ++ printTermSLaTeXIn m size
printTermLaTeXIn (App (App m n) (Var x)) size = printTermLaTeXIn (App m n) size ++ x
printTermLaTeXIn (App (App m n) l) size = printTermLaTeXIn (App m n) size ++ (printLargeParenLaTeX l (size - 1))
printTermLaTeXIn (App (Var x) (Var y)) size = x ++ y
printTermLaTeXIn (App (Var x) n) size = x ++ (printLargeParenLaTeX n (size - 1))
printTermLaTeXIn (App m (Var y)) size = (printLargeParenLaTeX m (size - 1)) ++ y
printTermLaTeXIn (App m n) size = (printLargeParenLaTeX m (depthTerm m)) ++ (printLargeParenLaTeX n (depthTerm n))
printTermSLaTeXIn (Abs x m) size = x ++ printTermSLaTeXIn m size
printTermSLaTeXIn m size = "." ++ printTermLaTeXIn m size

-- 括弧の大きさを指定
printLargeParenLaTeX :: Term -> Int -> String
printLargeParenLaTeX t n = "(" ++ (printTermLaTeXIn t 0) ++ ")"
{--
printLargeParenLaTeX t 0 = "(" ++ (printTermLaTeXIn t 0) ++ ")"
printLargeParenLaTeX t 1 = "\\bigl(" ++ (printTermLaTeXIn t 0) ++ "\\bigr)"
printLargeParenLaTeX t 2 = "\\Bigl(" ++ (printTermLaTeXIn t 1) ++ "\\Bigr)"
printLargeParenLaTeX t 3 = "\\biggl(" ++ (printTermLaTeXIn t 3) ++ "\\biggr)"
printLargeParenLaTeX t n = "\\Biggl(" ++ (printTermLaTeXIn t n) ++ "\\Biggr)"
-}

printTermM :: Maybe Term -> String
printTermM Nothing = []
printTermM (Just x) = printTerm x

printTermMLaTeX :: Maybe Term -> String
printTermMLaTeX Nothing = []
printTermMLaTeX (Just x) = printTermLaTeX x

showTermM :: Maybe Term -> IO()
showTermM = putStrLn . printTermM

printTermsM :: [Maybe Term] -> [String]
printTermsM []     = ["\n-/->"]
printTermsM [x]    = [printTermM x, "\n-/->"]
printTermsM (x:xs) = [printTermM x, "\n---> "] ++ (printTermsM xs)

showTermsM :: [Maybe Term] -> IO()
showTermsM = putStrLn . ("     " ++) . concat . printTermsM

printTermsMLaTeX :: [Maybe Term] -> [String]
printTermsMLaTeX []     = ["\\not\\to_{\\beta} & \\end{array}\\end{document}"]
printTermsMLaTeX [x]    = [printTermMLaTeX x, "\\\\\n\\not\\to_{\\beta} &\n\\end{array}\n$\n\\end{document}"]
printTermsMLaTeX (x:xs) = [printTermMLaTeX x, "\\\\\n\\to_{\\beta} &"] ++ (printTermsMLaTeX xs)


showTermsMLaTeX :: [Maybe Term] -> String
showTermsMLaTeX =  (w ++) . concat . printTermsMLaTeX
  where
    w = ("\\documentclass{standalone}\n\\begin{document}\n$\n\\begin{array}{rl}\n&")

-- ベータ基を指定して簡約
{-
rdctb :: Term -> Term -> Maybe Term
rdctb (Var x) n = Just (Var x)
rdctb (Abs x m) n = Just (Abs x (rdctb >>= m n))
rdctb (App (Abs x m) n) l =
  if (App (Abs x m) n) == l then rdct l else Just (App (Abs x (rdctb m l)) (rdctb m n))
rdctb (App m n) l = Just (App (rdctb m l) (rdctb n l))
-}

main = do
  (x:xs) <- getArgs
  let b
        | "--cbv" `elem` xs && "--cbn" `elem` xs    = error "簡約戦略は一つのみ指定する"
        | "--cbv" `elem` xs     = brdctsl brdctlcbv
        | "--cbn" `elem` xs     = brdctsl brdctlcbn
        | otherwise             = brdctsl brdctl
  if ("--latex" `elem` xs)
    then (\x -> writeFile "out.tex" x) $ showTermsMLaTeX $ b $ parser x
    else showTermsM $ b $ parser x
