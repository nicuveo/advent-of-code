{-# LANGUAGE TupleSections #-}

module IntCodePlusPlus (parseProgram, compile, assemble, transpile) where

import           Control.Arrow        (left)
import           Control.Exception    (assert)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Extra
import           Control.Monad.State
import           Data.Bool
import qualified Data.Map             as M
import qualified Data.Set             as S
import           Prelude              hiding (compare)
import           Text.Parsec
import qualified Text.Parsec.Token    as P
import           Text.Printf

import           AOC.Misc



-- grammar

type Program    = [Statement]

data Statement  = GotoLabel   String
                | IfBlock     Condition [Statement]
                | WhileBlock  Condition [Statement]
                | Goto        String
                | Read        String
                | Print       Expression
                | Assign      String Expression
                deriving (Show, Eq)

data Condition  = Equal    Expression Expression
                | NotEqual Expression Expression
                | Greater  Expression Expression
                | Less     Expression Expression
                deriving (Show, Eq)

data Expression = ETerm Term
                | Add   Term Term
                deriving (Show, Eq)

data Term       = TFactor Factor
                | Mul     Factor Factor
                deriving (Show, Eq)

data Factor     = Parenthesized Expression
                | Variable      String
                | Literal       Int
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
                                           , P.reservedOpNames = ["*", "+", "=", "==", "<", "!=", ">"]
                                           , P.reservedNames   = ["if", "while", "read", "print", "goto"]
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


parseProgram :: String -> String -> Either String Program
parseProgram = left show ... parse program
  where  program = whiteSpace *> statement `sepEndBy` many newline <* eof
         statement = choice [ try gotoLabel
                            , try ifBlock
                            , try whileBlock
                            , try goto
                            , try readI
                            , try printI
                            , try assign
                            ]

         gotoLabel = do
          l <- brackets identifier
          return $ GotoLabel l
         ifBlock = do
           symbol "if"
           c <- parens condition
           b <- braces $ many statement
           return $ IfBlock c b
         whileBlock = do
           symbol "while"
           c <- parens condition
           b <- braces $ many statement
           return $ WhileBlock c b
         goto = do
           symbol "goto" >> whiteSpace
           l <- identifier
           return $ Goto l
         readI = do
           symbol "read" >> whiteSpace
           v <- identifier
           return $ Read v
         printI = do
           symbol "print" >> whiteSpace
           e <- expression
           return $ Print e
         assign = do
           v <- identifier
           symbol "="
           e <- expression
           return $ Assign v e

         binary op e c = do
           e1 <- e
           symbol op
           e2 <- e
           return $ c e1 e2

         condition = choice [try lesser, try greater, try equal, try unequal]
           where lesser  = binary "<"  expression Less
                 greater = binary ">=" expression Greater
                 equal   = binary "==" expression Equal
                 unequal = binary "!=" expression NotEqual
         expression = try add <|> try eterm
           where eterm = ETerm <$> term
                 add   = binary "+" term Add
         term = try mul <|> try tfactor
           where tfactor = TFactor <$> factor
                 mul     = binary "*" factor Mul
         factor = parenthesized <|> variable <|> literal
           where parenthesized = Parenthesized <$> parens expression
                 variable      = Variable <$> identifier
                 literal       = Literal . fromInteger <$> iLiteral



-- compiler

data Param = Immediate Int | Position Int deriving (Show, Eq)

data Instruction = AddI  Param Param Int
                 | MulI  Param Param Int
                 | InI   Int
                 | OutI  Param
                 | JmpTI Param Int
                 | JmpFI Param Int
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
data CompileState = CS { csInst :: [Instruction]
                       , csVMap :: PosMapping
                       , csLMap :: PosMapping
                       , csSVar :: S.Set String
                       , csLVar :: S.Set String
                       , csPos  :: Int
                       , csBN   :: Int
                       , csBI   :: PosMapping
                       , csBO   :: PosMapping
                       }

type Compilation = StateT CompileState (Except String)

compile :: Program -> Either String [Instruction]
compile p = res
  where (vMap, bMap, res) = doCompile $ CS [] vMap M.empty S.empty S.empty 0 0 bMap M.empty
        doCompile s = case runExcept $ evalStateT (process p >> finalize) s of
                        Right x -> x
                        Left  e -> (M.empty, M.empty, Left e)


createLabel :: String -> Compilation ()
createLabel name = do
  m <- gets csLMap
  case M.lookup name m of
    Just _  -> throwError $ printf "error: label [%s] declared twice" name
    Nothing -> modify $ \s -> s { csLMap = M.insert name (csPos s) $ csLMap s }

registerVar :: String -> Compilation ()
registerVar v = modify $ \s -> s { csSVar = S.insert v $ csSVar s
                                 , csLVar = S.insert v $ csLVar s
                                 }

varAddress :: String -> Compilation Int
varAddress n = (M.! n) <$> gets csVMap

appendInstruction :: Instruction -> Compilation ()
appendInstruction i = modify $ \s -> s { csInst = i : csInst s
                                       , csPos  = csPos s + instSize i
                                       }

appendGoto :: String -> Compilation ()
appendGoto name = do
  m <- gets csLMap
  case M.lookup name m of
    Nothing -> throwError $ printf "error: unknwon label [%s]" name
    Just p  -> appendInstruction $ JmpTI (Immediate 1) p

appendRead :: String -> Compilation ()
appendRead n = do
  registerVar n
  i <- varAddress n
  appendInstruction $ InI i

appendPrint :: Expression -> Compilation ()
appendPrint e = do
  p <- evaluate 0 e
  appendInstruction $ OutI p

appendAssign :: String -> Expression -> Compilation ()
appendAssign n e = do
  p <- evaluate 0 e
  i <- varAddress n
  registerVar n
  appendInstruction $ AddI p (Immediate 0) i

appendVar :: String -> Compilation (String, Int)
appendVar n = do
  pos <- gets csPos
  appendInstruction $ Var n
  return (n, pos)

startBlock :: Compilation String
startBlock = do
  blockNumber <- gets csBN
  modify $ \s -> s { csBN = csBN s + 1 }
  return $ '@' : show blockNumber

blockIf :: Condition -> [Statement] -> Compilation ()
blockIf c b = do
  varsInScope <- gets csLVar
  eobName     <- startBlock
  (p, i)      <- fmap (bool JmpTI JmpFI) <$> compare c
  eob         <- gets $ \s -> csBI s M.! eobName
  appendInstruction $ i p eob
  process b
  modify $ \s -> s { csLVar = varsInScope
                   , csBO = M.insert eobName (csPos s) $ csBO s
                   }

blockWhile :: Condition -> [Statement] -> Compilation ()
blockWhile c b = do
  varsInScope <- gets csLVar
  eobName     <- startBlock
  startPos    <- gets csPos
  (p, i)      <- fmap (bool JmpTI JmpFI) <$> compare c
  eob         <- gets $ \s -> csBI s M.! eobName
  appendInstruction $ i p eob
  process b
  appendInstruction $ JmpTI (Immediate 1) startPos
  modify $ \s -> s { csLVar = varsInScope
                   , csBO = M.insert eobName (csPos s) $ csBO s
                   }


binaryOperation :: (Int -> a -> Compilation Param)
                -> (Int -> Int -> Int)
                -> (Param -> Param -> Int -> Instruction)
                -> Int -> a -> a -> Compilation Param
binaryOperation eval (#) createI d e1 e2 = do
  p1 <- eval  d    e1
  p2 <- eval (d+1) e2
  case (p1, p2) of
    (Immediate x, Immediate y) -> return $ Immediate $ x # y
    _ -> do
      let vName = '$' : show d
      registerVar vName
      dest <- varAddress vName
      appendInstruction $ createI p1 p2 dest
      return (Position dest)

evaluate :: Int -> Expression -> Compilation Param
evaluate = ee
  where ee d (ETerm t)   = et d t
        ee d (Add t1 t2) = binaryOperation et (+) AddI d t1 t2
        et d (TFactor f) = ef d f
        et d (Mul f1 f2) = binaryOperation ef (*) MulI d f1 f2
        ef d (Parenthesized e) = ee d e
        ef _ (Literal       i) = return $ Immediate i
        ef _ (Variable      n) = do
          whenM (gets $ \s -> n `S.notMember` csLVar s) $
            throwError $ printf "error: variable %s not in scope" n
          i <- varAddress n
          return $ Position i

compare :: Condition -> Compilation (Param, Bool)
compare c = case c of
  (Less     e1 e2) -> bo (<)  LtI True  e1 e2
  (Greater  e1 e2) -> bo (>)  LtI False e1 e2
  (Equal    e1 e2) -> bo (==) EqI True  e1 e2
  (NotEqual e1 e2) -> bo (/=) EqI False e1 e2
  where bo op i b = fmap (,b) ... binaryOperation evaluate (fromEnum ... op) i 0

process :: [Statement] -> Compilation ()
process = void . traverse step
  where step (GotoLabel n)    = createLabel n
        step (Goto l)         = appendGoto l
        step (Read n)         = appendRead n
        step (Print e)        = appendPrint e
        step (Assign n e)     = appendAssign n e
        step (IfBlock c b)    = blockIf c b
        step (WhileBlock c b) = blockWhile c b

finalize :: Compilation (PosMapping, PosMapping, Either String [Instruction])
finalize = do
  appendInstruction End
  vMap  <- fmap M.fromList $ traverse appendVar . S.toList =<< gets csSVar
  bMap  <- gets csBO
  insts <- gets csInst
  return (vMap, bMap, Right $ reverse insts)



-- assembler

assemble :: [Instruction] -> [Int]
assemble = (>>= expand)
  where expand i = let r = e_ i in assert (length r == instSize i) $ r
        e_ (AddI  p1 p2 d) = [op [p2, p1] "01", pv p1, pv p2, d]
        e_ (MulI  p1 p2 d) = [op [p2, p1] "02", pv p1, pv p2, d]
        e_ (InI         d) = [3, d]
        e_ (OutI      p  ) = [op [p] "04", pv p]
        e_ (JmpTI     p d) = [read ['1', pm p, '0', '5'], pv p, d]
        e_ (JmpFI     p d) = [read ['1', pm p, '0', '6'], pv p, d]
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
