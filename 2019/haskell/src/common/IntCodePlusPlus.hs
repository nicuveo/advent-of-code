module IntCodePlusPlus (parseProgram, compile, assemble, transpile) where

import           Control.Arrow        (left)
import           Control.Exception    (assert)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Extra
import           Control.Monad.State
import           Data.Bool
import           Data.Foldable
import qualified Data.Map             as M
import qualified Data.Set             as S
import           Prelude              hiding (compare)
import           Text.Parsec
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Token    as P
import           Text.Printf

import           AOC.Misc



-- grammar

type Program    = [Statement]

data Statement  = GotoLabel   String
                | IfBlock     Condition [Statement]
                | WhileBlock  Condition [Statement]
                | Goto        (Either String Expression)
                | Read        (Either String Expression)
                | Print       Expression
                | Assign      (Either String Expression) Expression
                deriving (Show, Eq)

data Condition  = C1  Condition1
                | BOr Condition1 Condition
                deriving (Show, Eq)

data Condition1 = C2   Condition2
                | BAnd Condition2 Condition1
                deriving (Show, Eq)

data Condition2 = Barenthesized  Condition
                | Not            Condition
                | CEQ Expression Expression
                | CNE Expression Expression
                | CGE Expression Expression
                | CGT Expression Expression
                | CLE Expression Expression
                | CLT Expression Expression
                deriving (Show, Eq)

data Expression = ETerm Term
                | Add   Term Expression
                | Sub   Term Expression
                deriving (Show, Eq)

data Term       = TFactor Factor
                | Mul     Factor Term
                | Div     Factor Term
                deriving (Show, Eq)

data Factor     = Parenthesized Expression
                | Variable      String
                | Literal       Int
                | Address       (Either Int String)
                | Deref         Expression
                deriving (Show, Eq)



-- parser

language = P.makeTokenParser P.LanguageDef { P.commentStart    = "/*"
                                           , P.commentEnd      = "*/"
                                           , P.commentLine     = "//"
                                           , P.nestedComments  = True
                                           , P.identStart      = letter
                                           , P.identLetter     = alphaNum
                                           , P.opStart         = parserZero
                                           , P.opLetter        = parserZero
                                           , P.reservedOpNames = []
                                           , P.reservedNames   = [ "if"
                                                                 , "while"
                                                                 , "read"
                                                                 , "print"
                                                                 , "goto"
                                                                 , "or"
                                                                 , "and"
                                                                 ]
                                           , P.caseSensitive   = True
                                           }

whiteSpace = P.whiteSpace    language
identifier = P.identifier    language
lexeme     = P.lexeme        language
symbol     = P.symbol        language
parens     = P.parens        language
braces     = P.braces        language
brackets   = P.brackets      language
iLiteral   = P.integer       language

tryAll :: [Parser a] -> Parser a
tryAll parsers = foldr1 (<|>) (map try parsers)

binary :: String -> Parser a -> Parser b -> (a -> b -> c) -> Parser c
binary op p1 p2 c = do
  e1 <- p1
  symbol op
  e2 <- p2
  whiteSpace
  return $ c e1 e2

unary :: String -> Parser a -> (a -> b) -> Parser b
unary prefix e c = do
  symbol prefix >> whiteSpace
  c <$> e


parseProgram :: String -> String -> Either String Program
parseProgram = left show ... parse program
  where  program = whiteSpace *> statement `sepEndBy` many newline <* eof
         statement = tryAll [ gotoLabel
                            , ifBlock
                            , whileBlock
                            , goto
                            , readI
                            , printI
                            , assign
                            ] <?> "a statement or a block"

         gotoLabel = GotoLabel <$> brackets identifier
         ifBlock = do
           symbol "if"
           c <- parens condition0
           b <- braces $ many statement
           return $ IfBlock c b
         whileBlock = do
           symbol "while"
           c <- parens condition0
           b <- braces $ many statement
           return $ WhileBlock c b
         goto    = unary "goto"  nameOrExpr Goto
         readI   = unary "read"  nameOrExpr Read
         printI  = unary "print" expression Print
         assign  = binary "=" nameOrExpr expression Assign

         nameOrExpr = tryAll [ Left <$> identifier
                             , char '$' >> Right . ETerm . TFactor . Variable <$> identifier
                             , char '@' >> Right . ETerm . TFactor . Literal . fromInteger <$> iLiteral
                             , char '$' >> Right <$> braces expression
                             ]
         condition0 = tryAll [ C1 <$> condition1
                             , binary "||" condition1 condition0 BOr
                             , binary "or" condition1 condition0 BOr
                             ]
         condition1 = tryAll [ C2 <$> condition2
                             , binary "&&"  condition2 condition1 BAnd
                             , binary "and" condition2 condition1 BAnd
                             ]
         condition2 = tryAll [ Barenthesized <$> parens condition0
                             , Not <$> (symbol "!"   >> parens condition0)
                             , Not <$> (symbol "not" >> parens condition0)
                             , binary "<"  expression expression CLT
                             , binary "<=" expression expression CLE
                             , binary ">"  expression expression CGT
                             , binary ">=" expression expression CGE
                             , binary "==" expression expression CEQ
                             , binary "!=" expression expression CNE
                             , binary "/=" expression expression CNE
                             ]

         expression = tryAll [ binary "+" term expression Add
                             , binary "-" term expression Sub
                             , ETerm <$> term
                             ]
         term       = tryAll [ binary "*" factor term Mul
                             , binary "/" factor term Div
                             , TFactor <$> factor
                             ]
         factor     = tryAll [ Parenthesized <$> parens expression
                             , Variable <$> identifier
                             , Literal . fromInteger <$> iLiteral
                             , char '@' >> Address . Left . fromInteger <$> iLiteral
                             , char '&' >> Address . Right <$> identifier
                             , char '$' >> Deref . ETerm . TFactor . Variable <$> identifier
                             , char '$' >> Deref <$> braces expression
                             ]



-- compiler

data Param = Immediate Int
           | Position  Int
           deriving (Show, Eq)

data Instruction = AddI  Param Param Int
                 | MulI  Param Param Int
                 | InI   Int
                 | OutI  Param
                 | JmpTI Param Param
                 | JmpFI Param Param
                 | LtI   Param Param Int
                 | EqI   Param Param Int
                 | End
                 | Var   String
                 deriving (Show, Eq)

instSize :: Instruction -> Int
instSize AddI {} = 4
instSize MulI {} = 4
instSize InI  {} = 2
instSize OutI {} = 2
instSize JmpTI{} = 3
instSize JmpFI{} = 3
instSize LtI  {} = 4
instSize EqI  {} = 4
instSize End  {} = 1
instSize Var  {} = 1


type PosMapping   = M.Map String Int
data CompileState = CS { csInstructions :: [Instruction]
                       , csAllVariables :: S.Set String
                       , csVarsInScope  :: S.Set String
                       , csCurrentPos   :: Int
                       , csBlockIndex   :: Int
                       , csAddresses    :: PosMapping
                       -- computed a posteriori (circular programming)
                       , csMapping      :: PosMapping
                       }

type Compilation = StateT CompileState (Except String)


compile :: Program -> Either String [Instruction]
compile p = res
  where (posMap, res) = doCompile $ CS [] S.empty S.empty 0 0 M.empty posMap
        doCompile s = case runExcept $ evalStateT (process p >> finalize) s of
                        Right x -> x
                        Left  e -> (M.empty, Left e)


isVarInScope :: String -> Compilation Bool
isVarInScope n = gets $ S.member n . csVarsInScope

getAddress :: String -> Compilation Int
getAddress name = do
  m <- gets csMapping
  return $ m M.! name

getTempVar :: Int -> Compilation Int
getTempVar d = do
  let n = '$' : show d
  registerVar n
  getAddress  n

getLabel :: String -> Compilation Int
getLabel l = getAddress $ '~' : l


currentPos :: Compilation Int
currentPos = gets csCurrentPos


registerVar :: String -> Compilation ()
registerVar v = modify $ \s ->
  s { csAllVariables = S.insert v $ csAllVariables s
    , csVarsInScope  = S.insert v $ csVarsInScope  s
    }

registerLabel :: String -> Compilation ()
registerLabel name = do
  m <- gets csAddresses
  p <- currentPos
  case M.lookup name m of
    Just _  -> throwError $ printf "error: label [%s] declared twice" name
    Nothing -> modify $ \s -> s { csAddresses = M.insert ('~' : name) p $ csAddresses s }

startBlock :: Compilation String
startBlock = do
  blockNumber <- gets csBlockIndex
  modify $ \s -> s { csBlockIndex = csBlockIndex s + 1 }
  return $ '>' : show blockNumber

finishBlock :: String -> Compilation ()
finishBlock bname = do
  p <- currentPos
  modify $ \s -> s { csAddresses = M.insert bname p $ csAddresses s }


appendInstruction :: Instruction -> Compilation ()
appendInstruction i = modify $ \s ->
  s { csInstructions = i : csInstructions s
    , csCurrentPos   = csCurrentPos s + instSize i
    }

appendGoto :: Either String Expression -> Compilation ()
appendGoto a = case a of
  Left name  -> do
    l <- getLabel name
    appendInstruction $ JmpTI (Immediate 1) $ Immediate l
  Right expr -> do
    p <- evaluate 0 expr
    appendInstruction $ JmpTI (Immediate 1) p

appendPrint :: Expression -> Compilation ()
appendPrint e = do
  p <- evaluate 0 e
  appendInstruction $ OutI p

inferAddress :: Either String Expression -> Int -> Int -> Compilation Int
inferAddress eve d offset = case eve of
  Left  n -> registerVar n >> getAddress n
  Right e -> do
    p <- evaluate d e
    case p of
      Immediate x -> return x
      _           -> do
        pos <- currentPos
        appendInstruction $ AddI p (Immediate 0) $ pos + offset
        return (-42)

appendRead :: Either String Expression -> Compilation ()
appendRead eve = do
  i <- inferAddress eve 0 5
  appendInstruction $ InI i

appendAssign :: Either String Expression -> Expression -> Compilation ()
appendAssign eve v = do
  p <- evaluate 0 v
  i <- inferAddress eve 1 7
  appendInstruction $ AddI p (Immediate 0) i

appendNot :: Param -> Compilation Param
appendNot (Immediate x) = return $ Immediate $ 1 - signum x
appendNot (Position  x) = do
  pos <- currentPos
  appendInstruction $ JmpTI (Position  x) $ Immediate $ pos + 10
  appendInstruction $ AddI  (Immediate 0) (Immediate 1) x
  appendInstruction $ JmpTI (Immediate 1) $ Immediate $ pos + 14
  appendInstruction $ AddI  (Immediate 0) (Immediate 0) x
  return $ Position x

appendSub :: Param -> Param -> Int -> Int -> Compilation ()
appendSub p1 p2 d dest = do
  tmp <- getTempVar $ d + 2
  appendInstruction $ MulI p2 (Immediate (-1)) tmp
  appendInstruction $ AddI p1 (Position tmp)   dest

appendDiv :: Param -> Param -> Int -> Int -> Compilation ()
appendDiv p1 p2 d dest = do
    pos <- currentPos
    res <- getTempVar $ d + 2
    acc <- getTempVar $ d + 3
    tmp <- getTempVar $ d + 4
    a   <- getTempVar $ d + 5
    b   <- getTempVar $ d + 6

    -- pos +  0: test that p2 /= 0
    appendInstruction $ JmpTI p2 $ Immediate $ pos + 6
    appendInstruction $ JmpTI (Immediate 1) $ Immediate (-10) -- crash the program
    appendInstruction $ AddI p1 (Immediate 0) a
    appendInstruction $ AddI p2 (Immediate 0) b
    -- pos + 14: if a < 0, switch both signs
    appendInstruction $ LtI  p1 (Immediate 0) tmp
    appendInstruction $ JmpFI (Position tmp) $ Immediate $ pos + 29
    appendInstruction $ MulI  (Immediate (-1)) (Position a) a
    appendInstruction $ MulI  (Immediate (-1)) (Position b) b
    -- pos + 29: test whether b < 0
    currentPos >>= \p -> when (p /= pos+29) $ error $ "expected 29 got" ++ show p
    appendInstruction $ LtI   (Position b) (Immediate 0) tmp
    appendInstruction $ JmpTI (Position tmp) $ Immediate $ pos + 66
    -- pos + 36: same sign
    currentPos >>= \p -> when (p /= pos+36) $ error $ "expected 36 got" ++ show p
    appendInstruction $ AddI  (Immediate 0)  (Immediate (-1)) res
    appendInstruction $ AddI  (Immediate 0)  (Immediate 0)    acc
    appendInstruction $ AddI  (Immediate 1)  (Position a)     a
    currentPos >>= \p -> when (p /= pos+48) $ error $ "expected 48 got" ++ show p
    appendInstruction $ LtI   (Position acc) (Position a)     tmp
    appendInstruction $ JmpFI (Position tmp) $ Immediate $ pos + 96
    appendInstruction $ AddI  (Position res) (Immediate 1)    res
    appendInstruction $ AddI  (Position acc) (Position  b)    acc
    appendInstruction $ JmpTI (Immediate 1)  $ Immediate $ pos + 48
    -- pos + 66: diff sign
    currentPos >>= \p -> when (p /= pos+66) $ error $ "expected 66 got" ++ show p
    appendInstruction $ AddI  (Immediate 0)    (Immediate 0)    res
    appendInstruction $ AddI  (Immediate 0)    (Immediate 0)    acc
    appendInstruction $ MulI  (Immediate (-1)) (Position  b)    b
    currentPos >>= \p -> when (p /= pos+78) $ error $ "expected 78 got" ++ show p
    appendInstruction $ LtI   (Position acc)   (Position a)     tmp
    appendInstruction $ JmpFI (Position tmp) $ Immediate $ pos + 96
    appendInstruction $ AddI  (Position res)   (Immediate (-1)) res
    appendInstruction $ AddI  (Position acc)   (Position  b)    acc
    appendInstruction $ JmpTI (Immediate 1)  $ Immediate $ pos + 78
    -- pos + 96: end
    currentPos >>= \p -> when (p /= pos+96) $ error $ "expected 96 got" ++ show p
    appendInstruction $ AddI (Position res) (Immediate 0) dest

appendVar :: String -> Compilation (String, Int)
appendVar n = do
  pos <- currentPos
  appendInstruction $ Var n
  return (n, pos)

restrictScope :: Compilation () -> Compilation ()
restrictScope action = do
  currentScope <- gets csVarsInScope
  action
  modify $ \s -> s { csVarsInScope = currentScope }

blockIf :: Condition -> [Statement] -> Compilation ()
blockIf c b = do
  iname <- startBlock
  p     <- compare c
  eob   <- getAddress iname
  appendInstruction $ JmpFI p $ Immediate eob
  restrictScope $ process b
  finishBlock iname

blockWhile :: Condition -> [Statement] -> Compilation ()
blockWhile c b = do
  wname    <- startBlock
  startPos <- currentPos
  p        <- compare c
  eob      <- getAddress wname
  appendInstruction $ JmpFI p $ Immediate eob
  restrictScope $ process b
  appendInstruction $ JmpTI (Immediate 1) $ Immediate startPos
  finishBlock wname


binaryOperation :: (Int -> a -> Compilation Param)
                -> (Int -> b -> Compilation Param)
                -> (Int -> Int -> Int)
                -> (Param -> Param -> Int -> Int -> Compilation ())
                -> Int -> a -> b -> Compilation Param
binaryOperation eval1 eval2 (#) doI d e1 e2 = do
  p1 <- eval1  d    e1
  p2 <- eval2 (d+1) e2
  case (p1, p2) of
    (Immediate x, Immediate y) -> return $ Immediate $ x # y
    _ -> do
      dest <- getTempVar d
      doI p1 p2 d dest
      return (Position dest)

binaryInstruction :: (Int -> a -> Compilation Param)
                  -> (Int -> b -> Compilation Param)
                  -> (Int -> Int -> Int)
                  -> (Param -> Param -> Int -> Instruction)
                  -> Int -> a -> b -> Compilation Param
binaryInstruction eval1 eval2 (#) createI =
  binaryOperation eval1 eval2 (#) (\p1 p2 _ d -> appendInstruction $ createI p1 p2 d)

evaluate :: Int -> Expression -> Compilation Param
evaluate = ee
  where ee d (ETerm t)   = et d t
        ee d (Add t1 e2) = binaryInstruction et ee (+) AddI d t1 e2
        ee d (Sub t1 e2) = binaryOperation et ee (-) appendSub d t1 e2
        et d (TFactor f) = ef d f
        et d (Mul f1 t2) = binaryInstruction ef et (*) MulI d f1 t2
        et d (Div f1 t2) = binaryOperation ef et div appendDiv d f1 t2
        ef d (Parenthesized e) = ee d e
        ef _ (Literal       i) = return $ Immediate i
        ef _ (Address       a) = case a of
                                   Left  i -> return $ Position i
                                   Right n -> getVar Immediate n
        ef _ (Variable      n) = getVar Position n
        ef d (Deref         e) = do
          dest <- getTempVar d
          p    <- evaluate d e
          case p of
            Immediate x -> appendInstruction $ AddI (Position x) (Immediate 0) dest
            Position  x -> do
              pos <- currentPos
              appendInstruction $ AddI (Position x)     (Immediate 0) $ pos + 5
              appendInstruction $ AddI (Position (-64)) (Immediate 0) dest
          return (Position dest)
        getVar f n = do
          unlessM (isVarInScope n) $ throwError $ printf "error: variable %s not in scope" n
          f <$> getAddress n

compare :: Condition -> Compilation Param
compare = ec0 0
  where ec0 d (C1   c1)    = ec1 d c1
        ec0 d (BOr  c1 c0) = binaryInstruction ec1 ec0 (+) AddI d c1 c0
        ec1 d (C2   c2)    = ec2 d c2
        ec1 d (BAnd c2 c1) = binaryInstruction ec2 ec1 (*) MulI d c2 c1
        ec2 d (Barenthesized c0) = ec0 d c0
        ec2 d (Not    c0)        = ec0 d c0 >>= appendNot
        ec2 d (CEQ e1 e2)        = bbi (==) EqI d e1 e2
        ec2 d (CLT e1 e2)        = bbi (<)  LtI d e1 e2
        ec2 d (CNE e1 e2)        = bbi (==) EqI d e1 e2 >>= appendNot
        ec2 d (CGE e1 e2)        = bbi (<)  LtI d e1 e2 >>= appendNot
        ec2 d (CLE e1 e2)        = ec2 d (CGE e2 e1)
        ec2 d (CGT e1 e2)        = ec2 d (CLT e2 e1)
        bbi op = binaryInstruction evaluate evaluate $ fromEnum ... op

process :: [Statement] -> Compilation ()
process = traverse_ step
  where step (GotoLabel n)    = registerLabel n
        step (Goto l)         = appendGoto l
        step (Read n)         = appendRead n
        step (Print e)        = appendPrint e
        step (Assign n e)     = appendAssign n e
        step (IfBlock c b)    = blockIf c b
        step (WhileBlock c b) = blockWhile c b

finalize :: Compilation (PosMapping, Either String [Instruction])
finalize = do
  appendInstruction End
  otherAddresses <- gets csAddresses
  allVars        <- gets csAllVariables
  varAddresses   <- fmap M.fromList $ traverse appendVar $ S.toList allVars
  instructions   <- gets csInstructions
  let allAdresses = M.unionWithKey failDuplicate varAddresses otherAddresses
  return (allAdresses, Right $ reverse instructions)
  where failDuplicate k a1 a2 = error $ printf "ICE: duplicate address %s: %d and %d" k a1 a2



-- assembler

assemble :: [Instruction] -> [Int]
assemble = (>>= expand)
  where expand i = let r = e_ i in assert (length r == instSize i) r
        e_ (AddI  p1 p2 d) = [op [p2, p1] "01", pv p1, pv p2, d]
        e_ (MulI  p1 p2 d) = [op [p2, p1] "02", pv p1, pv p2, d]
        e_ (InI         d) = [3, d]
        e_ (OutI      p  ) = [op [p] "04", pv p]
        e_ (JmpTI     p d) = [op [d, p] "05", pv p, pv d]
        e_ (JmpFI     p d) = [op [d, p] "06", pv p, pv d]
        e_ (LtI   p1 p2 d) = [op [p2, p1] "07", pv p1, pv p2, d]
        e_ (EqI   p1 p2 d) = [op [p2, p1] "08", pv p1, pv p2, d]
        e_ End             = [99]
        e_ (Var _)         = [0]
        op p o = read $ map pm p ++ o
        pm (Immediate _) = '1'
        pm (Position  _) = '0'
        pv (Immediate x) = x
        pv (Position  x) = x



-- pipeline

transpile :: String -> String -> Either String [Int]
transpile filename content = fmap assemble $ compile =<< parseProgram filename content
