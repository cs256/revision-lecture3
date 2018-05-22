{-# LANGUAGE GADTs, DataKinds, TypeFamilies, KindSignatures, FlexibleInstances, ScopedTypeVariables #-}

module Lib where

-- A data type for natural numbers
data Nat = Zero | Succ Nat

-- Type family for addition
type family Add (n :: Nat) (m :: Nat) :: Nat where
    Add Zero     m = m
    Add (Succ n) m = Succ (Add n m)

--data Expr = IntVal Int | ...

-- Expression language
data Expr (t :: *) where
    IntVal :: Int -> Expr Int
    BoolVal :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    If :: Expr Bool -> Expr a -> Expr a -> Expr a

data Ty = IntTy | BoolTy

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f [] = pure []
--mapA f (x:xs) = (\y ys -> y : ys) <$> f x <*> mapA f xs
mapA f (x:xs) = (:) <$> f x <*> mapA f xs

{-eval :: Expr -> Either Int Bool
eval (IntVal x) = Left x
eval (BoolVal x) = Right x
eval (Add l r) = case eval l of
    Left n -> case eval r of
        Left m -> Left (n+m)
        Right _ -> error "Got back a boolean"
    Right _ -> error "Got back a boolean"
eval (If c t f) = case eval c of
    Left n -> error "Got back an integer"
    Right b -> if b then eval t else eval f
-}
-- Nat Singletons
data SNat (n :: Nat) where
    SZero :: SNat 'Zero
    SSucc :: SNat n -> SNat ('Succ n)

-- Proxy type for Nats
data NatProxy (n :: Nat) = NatProxy

-- Reification
class ReifyNat (n :: Nat) where
    reifyNat :: NatProxy n -> Nat

instance ReifyNat Zero where
    reifyNat _ = Zero

instance ReifyNat n => ReifyNat (Succ n) where
    -- n here is a type variable referring to the n in the
    -- instance head
    reifyNat _ = Succ (reifyNat (NatProxy :: NatProxy n))

-- Reification for singletons (bit silly)
class ReifySNat (n :: Nat) where
    reifySNat :: SNat n -> Nat

instance ReifySNat Zero where
    reifySNat _ = Zero

instance ReifySNat n => ReifySNat (Succ n) where
    reifySNat (SSucc x) = Succ (reifySNat x)
