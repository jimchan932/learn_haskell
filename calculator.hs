{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Calculator where
import Text.Parsec 
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr
import System.IO
import Control.Applicative hiding ((<|>),many)
import Control.Monad.State 
import Control.Monad.Error
import Data.List (transpose, intercalate)
import Data.Either (isLeft,isRight)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M 

infixr 8 :^:
infixl 7 :*:, :%:, :/:
infixl 6 :+:, :-:


data Expression = Fail String
                | Literal Double 
		| Identifier String
                | Fac Expression  
                | Sqrt Expression
                | Sin Expression 
                | Cos Expression 
                | Tan Expression 
		| Asin Expression 
 		| Acos Expression
                | Atan Expression
 		| Max Expression Expression
                | Min Expression Expression                 
                | Expression :+: Expression
                | Expression :-: Expression
  		| Expression :*: Expression
                | Expression :^: Expression
 		| Expression :/: Expression
                | Expression :%: Expression 
                | Negation Expression     
	        | FunctionInvocation String [Expression]
                | LambdaInvocation LambdaAbstraction [Expression] 
 		| If Condition Expression Expression 
                | Bool Condition 
   		deriving Show                        

data Ops = Plus | Minus | Times | Div | Rem

infixl 3 :&:			
infixr 2 :|:
	
data Condition = Condition :&: Condition 
  	       | Condition :|: Condition
 	       | Not Condition
 	       | Equal Expression Expression 
 	       | LessThan Expression Expression 
  	       | LessThanEqual Expression Expression  
               deriving Show
                                  
type PatternMatch = Either Double String
                                    
data LambdaAbstraction = LambdaAbstraction [PatternMatch] Expression
		       deriving Show                    

data DefinitionType = Mutable | Immutable deriving Show

data Statement = Eval Expression
 	       | Undefine String  
               | ToMutable String
 	       | ToImmutable String	 	          		                				
               | AssignStatement DefinitionType String Expression 
  	       | FunctionDefinition DefinitionType String [LambdaAbstraction]  
   	       deriving Show 	


lexer = makeTokenParser $ javaStyle { opStart  = oneOf "+-^*/%|&=!<>~"
 				    , opLetter = oneOf "+-^*/%|&=!<>~"
  				    , reservedNames = ["_","const","constant","immutable"
  						      ,"undefine","undef"
						      ,"let"
						      ,"toMutable","toVariable"
						      ,"toImmutable","toConstant"
						      ,"if","then","else","main"
                                                      ,"sin","cos","tan","sqrt"
                                                      ,"max","min"]}

        
                        
parseNumber = do
  val <- naturalOrFloat lexer
  case val of
    Left i -> return (Literal $ fromIntegral i)
    Right n -> return (Literal n)

parseIdentifier = identifier lexer >>= return . Identifier

parseCoefficient = do
  liftA2 (:*:) (parseNumber <|> parens lexer parseExpression)
    (choice [ parens lexer parseExpression
            , parens lexer parseConditional
            , try (parens lexer parseCoefficient)  
            , parens lexer parseNumber              
            , try parseFunctionInvocation  
 	    , parseIdentifier ])


parseExpression = (flip buildExpressionParser) parseTerm [
    [ Prefix (Negation <$ reservedOp lexer "-") 
    , Prefix (id <$ reservedOp lexer "+") ]
  , [ Postfix (Fac <$ reservedOp lexer "!") ]  
  , [ Infix  ((:^:) <$ reservedOp lexer "^") AssocRight ]
  , [ Infix  ((:*:) <$ reservedOp lexer "*") AssocLeft
    , Infix  ((:/:) <$ reservedOp lexer "/") AssocLeft 
    , Infix  ((:%:) <$ reservedOp lexer "%") AssocLeft ] 
  , [ Infix  ((:+:) <$ reservedOp lexer "+") AssocLeft 
    , Infix  ((:-:) <$ reservedOp lexer "-") AssocLeft ] 
  ]                                  

parseTerm = try parseFunctionInvocation
	    <|> try parseIdentifier
            <|> parens lexer parseExpression
            <|> try parseCoefficient
            <|> parseNumber    
            <|> try parseConditional 

parsePrefixFunctionApp = parens lexer $ do  
  expr <- parseTerm 
  f <- (reserved lexer "max" >> return (Max expr)) 
       <|> (reserved lexer "min" >> return (Min expr)) 
       <|> (reservedOp lexer "^" >> return (expr :^:)) 
       <|> (reservedOp lexer "/" >> return (expr :/:))
       <|> (reservedOp lexer "%" >> return (expr :%:)) 
       <|> (reservedOp lexer "*" >> return (expr :*:)) 
       <|> (reservedOp lexer "+" >> return (expr :+:))
       <|> (reservedOp lexer "-" >> return (expr :-:))
  return (LambdaAbstraction [Right "y"] (f (Identifier "y")))

parsePostfixFunctionApp = parens lexer $ do  
  op <- (reservedOp lexer "^" >> return (:^:)) 
        <|> (reservedOp lexer "/" >> return (:/:))
        <|> (reservedOp lexer "%" >> return (:%:)) 
        <|> (reservedOp lexer "*" >> return (:*:)) 
        <|> (reservedOp lexer "+" >> return (:+:))
        <|> (reservedOp lexer "-" >> return (:-:))
  f <- fmap op parseTerm
  return (LambdaAbstraction [Right "y"] (f (Identifier "y")))

parseLambdaAbstraction = try parseBase <|> (parens lexer $ do 
  x <- parsePattern
  reservedOp lexer "->"
  (LambdaAbstraction xs expr) <- parseLambdaAbstraction
  return (LambdaAbstraction (x:xs) expr)) 
  where parseBase = parens lexer $ do 
          x <- parsePattern   
          reservedOp lexer "->"
          expr <- parseExpression 
          return (LambdaAbstraction [x] expr)  			 
        parsePattern = (parseNumber >>= \(Literal n) -> return $ Left n) 
                       <|> (identifier lexer >>= \x -> return $ Right x)

parseLambdaInvocation = do  
  lambdaFunc <- try parseLambdaAbstraction <|> try parsePrefixFunctionApp <|> parsePostfixFunctionApp 
  args <- parseArgument `sepBy1` spaces 
  return (LambdaInvocation lambdaFunc args)

parseFunctionApply = try parseBase <|> do 
  name <- identifier lexer 
  reservedOp lexer "$"
  expr <- parseFunctionApply
  return (FunctionInvocation name [expr])
  where parseBase = do 
          name <- identifier lexer  
   	  reservedOp lexer "$"
  	  expr <- parseExpression
          return (FunctionInvocation name [expr])

parseArgument = choice [ try (parens lexer parseExpression)
                          , parens lexer parseCoefficient
                          , try (parens lexer parseFunctionInvocation)     
                          , parens lexer parseConditional  
 		          , parseNumber
                          , parseIdentifier ] 

parseFunctionInvocation = choice [ parseFun1 "sin" Sin
 			    	 , parseFun1 "cos" Cos
 				 , parseFun1 "tan" Tan
 			  	 , parseFun1 "asin" Asin
 				 , parseFun1 "acos" Acos
 				 , parseFun1 "atan" Atan
                                 , parseFun1 "sqrt" Sqrt
                                 , parseFun2 "max" Max 
 				 , parseFun2 "min" Min
 				 , try parseFunctionApply
 				 , parseUserDefinedFunctions
                                 , parseLambdaInvocation
                                 ]
  where parseFun1 name func = do
          reserved lexer name 
          expr <- try (reservedOp lexer "$" >>
		       parseExpression >>= \e -> return e)
                   <|> parseArgument
          return (func expr)  
        parseFun2 name func = do 
          reserved lexer name 
 	  a <- parseArgument
          spaces
  	  b <- parseArgument
          return (a,b)
          return (func a b)
        parseUserDefinedFunctions =  do  
  	  ident <- identifier lexer 
  	  args <- parseArgument `sepBy1` spaces                 
  	  return $ FunctionInvocation ident args

parseConditional = do 
  reserved lexer "if"
  c <- parseCondition
  reserved lexer "then"
  e1 <- parseExpression
  reserved lexer "else"
  e2 <- parseExpression
  return (If c e1 e2) 


parseCondition = (flip buildExpressionParser) parseConditionalTerm $ [
    [ Prefix (Not <$ reservedOp lexer "~") ]
  , [ Infix ((:&:) <$ reservedOp lexer "&&") AssocLeft
    , Infix ((:|:) <$ reservedOp lexer "||")  AssocLeft ]
  ]


parseConditionalTerm = do 
  parens lexer parseCondition
  <|> parseComparison


parseComparison  = do 
 e1 <- parseExpression
 f <- (Equal e1 <$ reservedOp lexer "==")
      <|> (Not . Equal e1 <$ (reservedOp lexer "!=" <|> reservedOp lexer "/="))
      <|> (LessThan e1 <$ reservedOp lexer "<")
      <|> (Not . LessThanEqual e1 <$ reservedOp lexer ">")
      <|> (LessThanEqual e1 <$ reservedOp lexer "<=")
      <|> (Not . LessThan e1 <$ reservedOp lexer ">=")
 e2 <- parseExpression
 return (f e2)                                              

parseDefinitionType = 
  reserved lexer "let" >>
  ((choice [try $ reserved lexer "const",reserved lexer "constant",reserved lexer "immutable"]
  >> return Immutable)
  <|> return Mutable)

parseUndefine = choice [reserved lexer "undef",reserved lexer "undefine"] >> 
  identifier lexer >>= \name -> return $ Undefine name 		
		
parseToMutable = do 
  choice [reserved lexer "toMutable",reserved lexer "toVariable"]
  name <- identifier lexer 
  return (ToMutable name)

parseToImmutable = do 
  choice [reserved lexer "toImmutable",reserved lexer "toConstant"]
  name <- identifier lexer 
  return (ToImmutable name)

parseFunctionDefinition  = do   
  definitionType <- parseDefinitionType   
  fn <- identifier lexer
  patterns <- parameters
  reservedOp lexer "="
  expr <- parseExpression
  xs <- many (parseDefinition fn) 
  return $ FunctionDefinition definitionType fn (LambdaAbstraction patterns expr:xs)
  where parseDefinition fn = do 
          comma lexer
  	  parserReturn fn
          spaces
  	  patterns <- parameters 
  	  reservedOp lexer "="
  	  expr <- parseExpression
  	  return (LambdaAbstraction patterns expr)
        parameters = sepEndBy1 ((parseNumber >>= \(Literal n) -> return $ Left n) 
                               <|> ((identifier lexer <|> parsecMap (\() -> "_") (reserved lexer "_")) >>= \x -> return $ Right x)) 
                               spaces
parseAssignment = do          
  definitionType <- parseDefinitionType    
  ident <- identifier lexer
  reservedOp lexer "="
  expr <- parseExpression 
  return $ AssignStatement definitionType ident expr

parseLineInput = do 
  statements <- (spaces >> ((parseExpression >>= \e -> return (Eval e)) 
		<|> parseUndefine
                <|> try parseFunctionDefinition
       		<|> parseAssignment)) `sepEndBy1` (char ';')                      
  eof         

  return statements 

-- Interpreter 
type StoredVal = (DefinitionType, Either Double [LambdaAbstraction]) 
type CannotFailCalculator = StateT (M.Map String StoredVal) IO 
type Calculator a = ErrorT String CannotFailCalculator a

interpretCondition :: Condition -> Calculator Bool 
interpretCondition  (Not c) = fmap not (interpretCondition c)
interpretCondition (c1 :&: c2) = liftA2 (&&) (interpretCondition c1) (interpretCondition c2)
interpretCondition (c1 :|: c2) = liftA2 (||) (interpretCondition c1) (interpretCondition c2)
interpretCondition (Equal e1 e2) = liftA2 (==) (interpretExpression e1) (interpretExpression e2)
interpretCondition (LessThan e1 e2) = liftA2 (<) (interpretExpression e1) (interpretExpression e2)
interpretCondition (LessThanEqual e1 e2) = liftA2 (<=) (interpretExpression e1) (interpretExpression e2)

fromDegrees deg = deg * pi / 180 
toDegrees   rad = rad * 180 / pi

interpretExpression :: Expression -> Calculator Double 
interpretExpression (Literal n) = return n
interpretExpression (Identifier i) = do 
  ctx <- get 
  case M.lookup i ctx of 
    Nothing -> fail ("Unknown identifier: " ++ i) 
    Just (_, Left n) ->  return n 
 

interpretExpression (LambdaInvocation (LambdaAbstraction parameters expr) arguments) = do 
    when (nParameters > nArguments) 
      (fail $ "The anonymous function is applied to too few arguments. Evaluation fails.") 
    when (nParameters < nArguments) 
      (fail $ "The anonymous function is applied to too many arguments. Evaluation fails.") 
    ctx <- get
    argVals <- mapM interpretExpression arguments
    let patternMatches = zip parameters argVals
    if (or [name/=arg | (Left name,arg) <- patternMatches])
       then (fail "Non-exhaustive patterns in the anonymous function")
       else do modify (`fun` [(name,(Mutable, Left x)) | (Right name, x) <- patternMatches])
      	       r <- interpretExpression expr
    	       put ctx
    	       return r     
  where nParameters = length parameters
        nArguments  = length arguments                                   
        fun = foldl (flip (uncurry M.insert))

interpretExpression (FunctionInvocation fn arguments) = do  
  ctx <- get  
  case M.lookup fn ctx of  
    Nothing -> fail ("Unknown function: " ++ fn)
    Just (_, Left _) -> fail ("Cannot call constant: " ++ fn)
    Just (_, Right defintions) -> do
      when (nParameters > nArguments) (fail $ fn ++ " is applied to too few arguments. Evaluation fails.") 
      when (nParameters < nArguments) (fail $ fn ++ " is applied to too many arguments. Evaluation fails.")
      argVals <- mapM interpretExpression arguments
      case matchPattern fn defintions argVals of
        Left e -> fail e 
        Right (xs,expr) -> do
          modify (`fun` xs) 
          r <- interpretExpression expr
          put ctx 
          return r
      where nParameters = length $ (\(LambdaAbstraction ps _) -> ps) $ head defintions
            nArguments  = length arguments                                   
            fun = foldl (flip (uncurry M.insert))
            matchPattern fn [] args = Left ("Non-exhaustive patterns in function" ++ fn)
            matchPattern fn (LambdaAbstraction ps expr:xs) args 
              | or [n/=arg | (Left n, arg) <- paramters_args] = matchPattern fn xs args
              | otherwise = Right ([(name,(Mutable, Left arg)) | (Right name,arg) <- paramters_args],expr)
              where paramters_args = zip ps args
           
interpretExpression (If cond e1 e2) = do 
  b <- interpretCondition cond
  if b then interpretExpression e1 else interpretExpression e2 
interpretExpression (Fac e) = do 
  n <- interpretExpression e 
  return (product [1..(fromIntegral . floor) n]) 

interpretExpression (Sqrt e) = fmap sqrt (interpretExpression e)
interpretExpression (Sin e) = fmap (sin . fromDegrees) (interpretExpression e)
interpretExpression (Cos e) = fmap (cos . fromDegrees) (interpretExpression e)
interpretExpression (Tan e) = fmap (tan . fromDegrees) (interpretExpression e) 
interpretExpression (Asin e) = fmap (toDegrees . asin) (interpretExpression e)
interpretExpression (Acos e) = fmap (toDegrees . acos) (interpretExpression e)
interpretExpression (Atan e) = fmap (toDegrees . atan) (interpretExpression e)
interpretExpression (Max e1 e2) = 
  liftA2 max (interpretExpression e1)
  	     (interpretExpression e2)
interpretExpression (Min e1 e2) = 
  liftA2 min (interpretExpression e1)
             (interpretExpression e2)
interpretExpression (e1 :+: e2) =
  liftA2 (+) (interpretExpression e1)
             (interpretExpression e2)
interpretExpression (e1 :-: e2) =
  liftA2 (-) (interpretExpression e1)
             (interpretExpression e2)
interpretExpression (e1 :^: e2) =
  liftA2 (**) (interpretExpression e1)
             (interpretExpression e2)
interpretExpression (e1 :*: e2) =
  liftA2 (*) (interpretExpression e1)
             (interpretExpression e2)
interpretExpression (e1 :/: e2) =
  liftA2 (/) (interpretExpression e1)
             (interpretExpression e2)
interpretExpression (e1 :%: e2) = do 
  v1 <- interpretExpression e1  
  v2 <- interpretExpression e2
  let i1 = floor v1 
      i2 = floor v2
      m = i1 `mod` i2
  return $ fromIntegral m
interpretExpression (Negation expr) = fmap negate (interpretExpression expr) 
                                           
interpretStatement :: Statement -> Calculator ()
interpretStatement (Eval expr) = do 
  n <- interpretExpression expr
  liftIO $ putStrLn (" = " ++ show n) 
interpretStatement (Undefine name) = do 
  ctx <- get 
  when (M.notMember name ctx) (fail $ "unknown object: " ++ name)
  modify (M.delete name)
interpretStatement (AssignStatement definitionType ident expr) = do
  n <- interpretExpression expr
  ctx <- get  
  case M.lookup ident ctx of
    Just (Immutable, Left _) -> fail $ "the variable " ++ ident ++ " is immutable"
    Just (Immutable, Right _) -> fail $ "the function " ++ ident ++ " is immutable" 
    otherwise -> modify $ M.insert ident (definitionType, Left n)
interpretStatement (FunctionDefinition definitionType fn definitions@((LambdaAbstraction parameters _):xs)) = do 
  ctx <- get 
  case M.lookup fn ctx of 
    Just (Immutable, Left _) -> fail $ "the variable " ++ fn ++ " is immutable"
    Just (Immutable, Right _) -> fail $ "the function " ++ fn ++ " is immutable" 
    otherwise -> do 
      when (any (\x -> length parameters /= length x) (getParameters xs))
        (fail $ "Equations for " ++ fn ++ " have different number of arguments") 
      when (or [length columns > 1 && any isRight (init columns) | columns <- transpose $ getParameters definitions])
       (fail $ "Pattern matches are overlapped\nIn the equations for " ++ fn)

      modify $ M.insert fn (definitionType, Right definitions)
  where getParameters = map (\(LambdaAbstraction xs e) -> xs) 
interpretStatement (ToMutable name) = do 
  ctx <- get 
  case M.lookup name ctx of  
    Nothing -> fail $ "unknown object " ++ name
    Just (Mutable,Left _) -> fail $ "the identifier " ++ name ++ " is already mutable"
    Just (Mutable,Right _) -> fail $ "the function " ++ name ++ " is already mutable"
    otherwise -> modify (M.adjust (\(_,definition) -> (Mutable,definition)) name)
interpretStatement (ToImmutable name) = do 
  ctx <- get 
  case M.lookup name ctx of  
    Nothing -> fail $ "unknown object " ++ name
    Just (Immutable,Left _) -> fail $ "the identifier " ++ name ++ " is already immmutable"
    Just (Immutable,Right _) -> fail $ "the function " ++ name ++ " is already immmutable"
    otherwise -> modify (M.adjust (\(_,definition) -> (Immutable,definition)) name)

interpretStatements :: [Statement] -> Calculator ()
interpretStatements = mapM_ interpretStatement

defaultVars :: M.Map String StoredVal 
defaultVars = M.fromList                 
            [ ("e",(Immutable,Left (exp 1))) 
            , ("pi",(Immutable,Left pi))
            , ("phi",(Immutable,Left ((1 + sqrt 5) / 2)))
  	    ]
			     
calculate :: String -> CannotFailCalculator ()
calculate s = 
  case ret of
    (Left e)  -> liftIO $ putStrLn $ "error: "++show e
    (Right n) -> do res <- runErrorT (interpretStatements n) 
                    case res of
                      Left e' -> liftIO $ putStrLn $ "run error: " ++ e'
                      Right _ -> return ()
  where
  ret = runParser parseLineInput M.empty "" s

calculator :: CannotFailCalculator ()
calculator = 
  liftIO getContents >>= (mapM_ calculate) . lines

main :: IO ()
main = evalStateT calculator defaultVars 

