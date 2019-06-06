{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Typelevel.Exercises.Intro.GADTs where

data Kilometers
data Miles

----------------------------------------
-- GADTs: Motivation
----------------------------------------

data ELit
  = LBool Bool
  | LInt Int

data Expr
  = ELit ELit
  | EAdd Expr Expr
  | EAnd Expr Expr
  | EEq Expr Expr
  | EIf Expr Expr Expr

eval :: Expr -> Either [Char] ELit
eval expr =
  case expr of
    ELit lit -> Right lit
    EAdd e1 e2 -> do
      res1 <- eval e1
      case res1 of
        LInt n1 -> do
          res2 <- eval e2
          case res2 of
            LInt n2 -> Right (LInt (n1 + n2))
            LBool _ -> Left "Cannot add EBool"
        LBool _ -> Left "Cannot add EBool"
    -- Similar implementation as EAdd
    EAnd e1 e2 -> undefined
    EEq e1 e2 -> do
      res1 <- eval e1
      res2 <- eval e2
      case (res1, res2) of
        (LBool b1, LBool b2) -> Right (LBool (b1 && b2))
        (LInt n1, LInt n2) -> Right (LInt (n1 + n2))
        _ -> Left "Cannot compare equality on different types"
    ----------------------------------------
    -- Exercise 1
    --
    --   Implement the evaluation logic of
    --   the EIf expression
    ----------------------------------------
    EIf c e e' -> undefined

-- | Should Succeed
test_Exercise1_a =
  eval $
    EIf (EAnd (ELit (LBool True)) (ELit (LBool False)))
      (EAdd (ELit (LInt 5)) (ELit (LInt 2)))
      (ELit (LInt 6))

-- | Should Fail
test_Exercise1_b =
  eval $
    EIf (EAnd (ELit (LInt 1)) (ELit (LBool True)))
      (ELit (LInt 5))
      (EEq (ELit (LInt 1)) (ELit (LInt 1)))

----------------------------------------
-- GADTs: Motivation (Phantom)
----------------------------------------

data PLit
  = PBool Bool
  | PInt Int

data PExpr a
  = PLit PLit
  | PAdd (PExpr Int) (PExpr Int)
  | PAnd (PExpr Bool) (PExpr Bool)
  | PEq (PExpr a) (PExpr a)
  | PIf (PExpr Bool) (PExpr a) (PExpr a)

peval :: PExpr a -> Either [Char] PLit
peval pexpr =
  case pexpr of
    PLit plit -> Right plit
    PAdd pe1 pe2 -> do
      res1 <- peval pe1
      case res1 of
        PBool _ -> Left "Cannot add PBool"
        PInt n1 -> do
          res2 <- peval pe2
          case res2 of
            PBool _ -> Left "Cannot add PBool"
            PInt n2 -> pure (PInt (n1 + n2))
    PAnd e1 e2 -> undefined
    PEq e1 e2 -> undefined
    PIf c e1 e2 -> undefined

peval_Example_1 :: Either [Char] PLit
peval_Example_1 =
  peval $
    PAnd
      (PLit (PBool True))
      (PLit (PBool False))

peval_Example_2 :: Either [Char] PLit
peval_Example_2 =
  peval $
    PEq
      (PLit (PInt 1))
      (PLit (PBool True))

----------------------------------------
-- GADTs: Solution
----------------------------------------

-- | Isomporphic to PLit
data GLit a where
  GBool :: Bool -> GLit Bool
  GInt :: Int -> GLit Int

-- | Isomorphic to PExpr, but more "type-safe"!
-- Now the 'Expr' datatype is _generalized_ over a return type, and the return
-- type of the constructor can _depend_ on the types of the constructor fields.
data GExpr a where
  GLit :: GLit a -> GExpr a
  GAdd :: GExpr Int -> GExpr Int -> GExpr Int
  GAnd :: GExpr Bool -> GExpr Bool -> GExpr Bool
  GEq :: Eq a => GExpr a -> GExpr a -> GExpr Bool
  GIf :: GExpr Bool -> GExpr a -> GExpr a -> GExpr a

gexpr_Example_1 :: GExpr Int
gexpr_Example_1 =
  GIf (GAnd (GLit (GBool True)) (GLit (GBool False)))
    (GAdd (GLit (GInt 5)) (GLit (GInt 2)))
    (GLit (GInt 6))

----------------------------------------
-- Exercise 2a
--
--   Implement the 'GExpr' AST that
--   describes the code:
--
--     if (True && False)
--       then (7 + 2)
--       else (1 == 1)
--
----------------------------------------
gexpr_Exercise_2a :: GExpr a
gexpr_Exercise_2a = undefined

----------------------------------------
-- Exercise 2b
--
--   Implement the rest of the 'geval'
--   function.
--
--   - How does the genralization of the
--     return type of the 'GExpr' and
--     'GLit' expressions help us?
--
--   - How should we change the GEq
--     constructor to make this compile?
--     [Hint]: The compiler may tell you
--     what to do!
--
--   - How many ways can we implement
--     the body of this function?
--
----------------------------------------
gevalLit :: GLit a -> a
gevalLit (GBool b) = b
gevalLit (GInt n)  = n

geval :: GExpr a -> a
geval = undefined
