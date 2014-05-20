{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

{-|
Module      : DualALU
Copyright   : (c) Jaco Bos, 2014
License     : see libraries/base/LICENSE
Maintainer  : University of Twente
Stability   : experimental
Portability : non-portable (experimental)
  
Minimal dual ALU example with 'blockRom' 
Evaluate as: > sampleN 9 $ expectedOutput (topEntity 1)

CLaSHi, version 0.3.1 (using clash-lib, version 0.3.1)
http://christiaanb.github.io/clash2/
-}

module DualALU where

import CLaSH.Prelude

-- | Type synonyms
type Word      = Unsigned 4
type ProcState = (Word, Word, Word)

-- | Type declarations
data OpCode    = ADD | SUB | BRZ | NoOp deriving (Eq, Show)
data Oper      = Imm Word | Reg0 | Reg1 | None deriving (Show)
data Instr     = A0 OpCode Oper Oper | A1 OpCode Oper Oper | Empty deriving (Show)
data MachCode  = MachCode { opcA0, opcA1 :: OpCode, a0opa, a0opb, a1opa, a1opb :: Oper }

-- | Create default instance of 'Instr' (used in 'blockRom')
instance Default Instr where def = A1 BRZ (Imm 0) (Imm 0)

-- | Program instructions
program :: Vec 4 (Instr, Instr)
program = (A0 ADD (Imm 0) (Imm 1), A1 ADD (Imm 0) (Imm 15)) :>
          (A0 ADD Reg0    (Imm 1), A1 SUB Reg1    (Imm 1))  :> 
          (A0 ADD Reg0    (Imm 0), A1 BRZ Reg1    (Imm 0))  :> 
          (A0 ADD Reg0    (Imm 0), A1 BRZ (Imm 0) (Imm 1))  :> Nil

-- | Default decoder output
nullcode :: MachCode
nullcode = MachCode { opcA0 = NoOp, opcA1 = NoOp, 
                      a0opa = None, a0opb = None, 
                      a1opa = None, a1opb = None }

-- | Instruction decoder
decode :: (Instr, Instr) -> MachCode
decode (instr1, instr2) = subDecode (subDecode nullcode instr1) instr2 where
    subDecode n i = case i of
        A0 c opa opb -> n { opcA0 = c, a0opa = opa, a0opb = opb }
        A1 c opa opb -> n { opcA1 = c, a1opa = opa, a1opb = opb }
        Empty -> n

-- | Multiplexer to select 'alu' operands 
getOpers :: Oper -> Oper -> Word -> Word -> (Word, Word)
getOpers opa opb reg0 reg1 = ((mux opa), (mux opb)) where 
    mux op = case op of
        Imm x -> x
        Reg0  -> reg0
        Reg1  -> reg1
        None  -> 0 

-- | Arithmetic logical unit
alu :: (Num a, Eq a) => OpCode -> (a, a) -> a -> a -> (a, a)
alu opCode (a, b) pc regVal = (result, pc') where
    pc' = if opCode == BRZ then (if a == 0 then b else pc + 1) else pc + 1
    result = case opCode of
        ADD -> a + b
        SUB -> a - b
        _   -> regVal

-- | Wrap up the above components
processor :: ProcState -> (Instr, Instr) -> (ProcState, (Word, Word, Word))
processor (reg0, reg1, pc) instr = ((reg0', reg1', pc'), (outA0, outA1, pc')) where
    MachCode {..}  = decode instr
    (reg0', _  )   = alu opcA0 (getOpers a0opa a0opb reg0 reg1) pc reg0
    (reg1', pc')   = alu opcA1 (getOpers a1opa a1opb reg0 reg1) pc reg1
    (outA0, outA1) = (reg0, reg1)

-- | Block ROM with initialized values
{-# NOINLINE blockRom #-}
blockRom :: forall n m a . (KnownNat n, KnownNat m, Pack a, Default a)
    => SNat n                -- ^ Size n of the blockRom
    -> Vec n a               -- ^ Initial values
    -> Signal (Unsigned m)   -- ^ Read address r
    -> Signal a              -- ^ Value of the blockRom at address r from the previous clock cycle
blockRom n inp rd = pack $ (brom' <^> binit) rd
  where
    binit :: (Vec n a,a)
    binit = (inp,def)

    brom' :: (Vec n a, a) -> Unsigned m -> (((Vec n a), a), a)
    brom' (rom, o) r = ((rom, o'), o)
      where o' = rom ! r

-- | Initialize top entity component
topEntity :: Signal Word -> Signal (Word, Word, Word)
topEntity _ = pack (a0, a1, pc) where
    (a0, a1, pc) = (processor <^> (0, 0, 0)) (unpack instr)
    instr = blockRom d4 (vreverse program) pc 

-- | Output verification
expectedOutput :: Signal (Word, Word, Word) -> Signal Bool
expectedOutput = outputVerifier $(v [(0, 0, 0) :: (Unsigned 4, Unsigned 4, Unsigned 4), 
    (0, 0, 1), (1, 15, 2), (2, 14, 3), (2, 14, 1), (2, 14, 2), (3, 13, 3), (3, 13, 1) ])

