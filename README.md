CλaSH dual ALU processor
==============

A simple synthesizable example of a dual-ALU processor in less than 100 lines of code to get an idea of how a processor can be described with CλaSH. Sub-blocks in a conceptual schematic (see pdf) are identified and have direct relations to corresponding CλaSH functions.

![dual-alu](https://cloud.githubusercontent.com/assets/7501668/7552237/c7ecc540-f6a9-11e4-89b4-79526755ebd7.png)

Below first lines of the dual-ALU processor description, a module name is defined and the CλaSH library is imported. Two type synonyms are defined, `Word` is used to set a data width of 4 bits and the `ProcState` type is used to explicitly name the 3-tuple of `Words` to denote the processor state which are the registers. Using a type synonym can be used to easily adjust the data width in the future. Furthermore, algebraic data types (ADTs) are defined for the opcodes, operands, instructions and control values. These type definitions define a very small embedded (assembly) language. 

```haskell
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
```

These definitions are already enough to write a program. Below an example program which is a vector of 2-tuples with instructions. Each first element of the 2-tuple is an instruction for the first ALU (`A0`) and each second instruction for the second ALU (`A1`). With this program `A0` should count up from 0, and `A1` should count down from 15. When `Reg1` becomes zero the program starts over by setting the program counter to zero, or else the program counter is set to 1.

```haskell
-- | Program instructions
program :: Vec 4 (Instr, Instr)
program = (A0 ADD (Imm 0) (Imm 1), A1 ADD (Imm 0) (Imm 15)) :>
          (A0 ADD Reg0    (Imm 1), A1 SUB Reg1    (Imm 1))  :> 
          (A0 ADD Reg0    (Imm 0), A1 BRZ Reg1    (Imm 0))  :> 
          (A0 ADD Reg0    (Imm 0), A1 BRZ (Imm 0) (Imm 1))  :> Nil
```

The program vector is stored in memory and the program counter selects a 2-tuple of this program vector. A decoder is then required to take such an instruction tuple and needs to produce the correct control signals on its output wires. below the decode function. The nullcode function returns default control values, and when required, fields are replaced according to the instruction tuple by the decode function. As a result, a `Controls` record is produced that contains the control signals for the multiplexers and ALUs. 

```haskell
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
```

Now that the control values are set, the operand values need to be retrieved. This is either an immediate, specified by the program and received from the decoder, a register value or zero. below the function `getOpers` that selects the operands for one ALU. This function implements two multiplexers and produces a 2-tuple with the operands `a` and `b`.

```haskell
-- | Multiplexer to select 'alu' operands 
getOpers :: Oper -> Oper -> Word -> Word -> (Word, Word)
getOpers opa opb reg0 reg1 = ((mux opa), (mux opb)) where 
    mux op = case op of
        Imm x -> x
        Reg0  -> reg0
        Reg1  -> reg1
        None  -> 0 
```

With the operands at the outputs of the multiplexers the ALUs can perform the actual operation. Below the ALU description. Additionally, an ALU also updates the program counter. When the `BRZ` instruction is detected the program counter is set to the value given by operand `b` if operand `a` is zero, else the program counter is increased by 1. Note that updating the program-counter in ALU `A0` is ignored since only one program counter is required. When the top-level is described (further on) the new `pc` produced by `A0` is not used, the synthesis tool will detect this
and will remove unused logic and wires. Notice how a single description can be used to infer slightly different circuits. 

There is also an additional element in the function definition namely: (Num a, Eq a). This means that the function arguments `a` need to be a member of the numeric (`Num`) and equality (`Eq`) type classes. This guarantees that it is allowed to perform arithmetic operations on the operands and that it is allowed to compare the `opCode`. Finally, as a result new values for the registers `reg0`, `reg1` and `pc` are computed.

```haskell
-- | Arithmetic logical unit
alu :: (Num a, Eq a) => OpCode -> (a, a) -> a -> a -> (a, a)
alu opCode (a, b) pc regVal = (result, pc') where
    pc' = if opCode == BRZ then (if a == 0 then b else pc + 1) else pc + 1
    result = case opCode of
        ADD -> a + b
        SUB -> a - b
        _   -> regVal
```

With the above functions all required functional blocks are individually described. The following code shows the function `processor` that connects the decoder, multiplexers, ALUs and registers. This function takes the current state (`Procstate`) and an instruction-tuple and then produces a new state and two outputs. Note that through grouping values in tuples this function is in the Mealy machine form of state → input → (newstate, output). Therefore, from this combinational function a synchronous and state-full function can be created at the top level. The decode function decodes the instruction-tuple and returns the control values. With the `{..}` notation all field names of the record become available in this function. To use this, the `RecordWildCards` language extension is required. The ALUs are supplied with their required arguments where the `getOpers` function retrieves the operand values.

```haskell
-- | Wrap up the above components
processor :: ProcState -> (Instr, Instr) -> (ProcState, (Word, Word, Word))
processor (reg0, reg1, pc) instr = ((reg0', reg1', pc'), (outA0, outA1, pc')) where
    MachCode {..}  = decode instr
    (reg0', _  )   = alu opcA0 (getOpers a0opa a0opb reg0 reg1) pc reg0
    (reg1', pc')   = alu opcA1 (getOpers a1opa a1opb reg0 reg1) pc reg1
    (outA0, outA1) = (reg0, reg1)
```

To model memory in a realistic way, a custom `blockROM` function is created. This generic function is able to hold a vector of values of an arbitrary type from which one element can be read with a 1-cycle delay which it typical for memories. For the purpose of this example the `blockROM` function is written such that it can be translated to logic instead of dedicated RAM. A VHDL simulation is therefore possible where loading the program into RAM is not necessary.

Finally the required `topEntity` component can be described which indicates the top level. The processor is “lifted” into a component with the initial state `(0,0,0)`. By “lifting” this Mealy machine function with the (`<^>`) operator, it becomes a sequential circuit that operates on infinite streams of values of type `Signal` synchronized to a global clock. This means that the three values of `ProcState` are stored in registers, and that a stream of instructions is the circuit input. The circuit output is the 3-tuple signal `(a0,a1,pc)`. The program is passed to the `blockROM` where the `pc` selects the required instruction. The sequential `blockROM` function produces an instruction of type `Signal (Instr,Instr)` but the (lifted)`processor` function expects the type `(Signal Instr, Signal Instr)`. Therefore
the `unpack` function is required to match the expected type.

```haskell
-- | Initialize top entity component
topEntity :: Signal Word -> Signal (Word, Word, Word)
topEntity _ = pack (a0, a1, pc) where
    (a0, a1, pc) = (processor <^> (0, 0, 0)) (unpack instr)
    instr = blockRom d4 (vreverse program) pc 
```

To verify whether the dual-ALU processor works as expected, the `expectedOutput` function is used. It defines a vector of 3-tuples which expected (consecutive) output values, these can be compared to the actual output of the processor to verify its behavior.

```haskell
-- | Output verification
expectedOutput :: Signal (Word, Word, Word) -> Signal Bool
expectedOutput = outputVerifier $(v [(0, 0, 0) :: (Unsigned 4, Unsigned 4, Unsigned 4), 
    (0, 0, 1), (1, 15, 2), (2, 14, 3), (2, 14, 1), (2, 14, 2), (3, 13, 3), (3, 13, 1) ])
```

With the command below, executed in the GHCi environment, it is demonstrated that the processor produces the first 8 expected values defined by the `expectedOutput` function. Because the 9th expected value is not defined, a mismatch message is returned.

```
> sampleN 9 $ expectedOutput (topEntity 1)
[False,False,False,False,False,False,False,False,
expected value:
(3,13,1), not equal to actual value:
(3,13,2) True]
```

All code listings combined can be translated by CλaSH to VHDL.

http://www.clash-lang.org/
