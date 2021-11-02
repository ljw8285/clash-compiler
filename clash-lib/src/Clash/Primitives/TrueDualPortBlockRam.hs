{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Clash.Primitives.TrueDualPortBlockRam where

import           Data.Text.Lazy                     (pack)
import           Text.Trifecta.Result               (Result(Success))
import qualified Data.String.Interpolate            as I
import qualified Data.String.Interpolate.Util       as I

import           Clash.Netlist.BlackBox.Parser      (runParse)
import           Clash.Netlist.BlackBox.Types
  (BlackBoxFunction, BlackBoxMeta(..), TemplateKind(TDecl), emptyBlackBoxMeta)
import           Clash.Netlist.Types(BlackBox(BBTemplate))

import           Clash.Core.TermLiteral
import           Clash.Core.Pretty
import Clash.Explicit.BlockRam(WriteMode(..))

trueDualPortBlockRamVerilog :: BlackBoxFunction
trueDualPortBlockRamVerilog _isD _primName args _ty = return bb
  where
    meta bbKi = emptyBlackBoxMeta{bbKind=bbKi}
    [_,_,_,_,_,_,_,_,_,_,Left wmA, Left outRegA, Left wmB, Left outRegB,_clkA, _enA, _wEnA, _nAddrA, _datA, _clkB, _enB, _wEnB, _nAddrB, _datB] = args
    bb = case runParse (pack $ I.unindent $ pre ++ portA ++ portB ++ post) of
       Success t    -> Right (meta TDecl, BBTemplate t)
       _            -> Left "internal error: parse fail"

    pre = [I.i|
        // trueDualPortBlockRam begin
        // Shared memory
        reg [~SIZE[~TYP[14]]-1:0] ~GENSYM[mem][0] [~LIT[1]-1:0];

        reg ~SIGD[~GENSYM[outputA][1]][14];
        reg ~SIGD[~GENSYM[outputB][2]][19];|] ++
         (if (termToData outRegA == Right True) then [I.i|
        reg ~SIGD[~GENSYM[outputA2][3]][14];|] else "") ++
         if (termToData outRegB == Right True) then [I.i|
        reg ~SIGD[~GENSYM[outputB2][4]][19];|] else ""

    portA' = case (termToData wmA) of
      Right WriteFirst -> [I.i|
        // Port A WriteFirst
        always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[10]) begin
            if(~ARG[11]) begin
                ~SYM[1] <= ~SYM[0][~ARG[13]];
                if(~ARG[12]) begin
                    ~SYM[1] <= ~ARG[14];
                    ~SYM[0][~ARG[13]] <= ~ARG[14];
                end|]
      Right ReadFirst -> [I.i|
        // Port A ReadFirst
            always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[10]) begin
                if(~ARG[11]) begin
                    ~SYM[1] <= ~SYM[0][~ARG[13]];
                    if(~ARG[12]) begin
                        ~SYM[0][~ARG[13]] <= ~ARG[14];
                    end|]
      Right NoChange -> [I.i|
        // Port A NoChange
            always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[10]) begin
                if(~ARG[11]) begin
                    if(~ARG[12]) begin
                        ~SYM[0][~ARG[13]] <= ~ARG[14];
                    end else begin
                        ~SYM[1] <= ~SYM[0][~ARG[13]];
                    end|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port A template, type is " ++ (showPpr t)

    portA = portA' ++ (if (termToData outRegA == Right True) then [I.i|
                ~SYM[3] <= ~SYM[1];|] else "") ++ [I.i|
            end
        end|]

    portB' = case (termToData wmB) of
      Right WriteFirst -> [I.i|
        // Port B WriteFirst
        always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[15]) begin
            if(~ARG[16]) begin
                ~SYM[2] <= ~SYM[0][~ARG[18]];
                if(~ARG[17]) begin
                    ~SYM[2] <= ~ARG[19];
                    ~SYM[0][~ARG[18]] <= ~ARG[19];
                end|]
      Right ReadFirst -> [I.i|
        // Port B ReadFirst
            always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[15]) begin
                if(~ARG[16]) begin
                    ~SYM[2] <= ~SYM[0][~ARG[18]];
                    if(~ARG[17]) begin
                        ~SYM[0][~ARG[18]] <= ~ARG[19];
                    end|]
      Right NoChange -> [I.i|
        // Port B NoChange
            always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[15]) begin
                if(~ARG[16]) begin
                    if(~ARG[17]) begin
                        ~SYM[0][~ARG[18]] <= ~ARG[19];
                    end else begin
                        ~SYM[2] <= ~SYM[0][~ARG[18]];
                    end|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port B template, type is " ++ (showPpr t)

    portB = portB' ++ (if (termToData outRegB == Right True) then [I.i|
                ~SYM[4] <= ~SYM[2];|] else "") ++ [I.i|
            end
        end|]

    post = case (termToData outRegA, termToData outRegB) of
        (Right False, Right False) ->   [I.i|
        assign ~RESULT = {~SYM[1], ~SYM[2]};
        // end trueDualPortBlockRam"|]

        (Right False, Right True) ->    [I.i|
        assign ~RESULT = {~SYM[1], ~SYM[4]};
        // end trueDualPortBlockRam"|]

        (Right True, Right False) ->    [I.i|
        assign ~RESULT = {~SYM[3], ~SYM[2]};
        // end trueDualPortBlockRam"|]

        (Right True, Right True) ->     [I.i|
        assign ~RESULT = {~SYM[3], ~SYM[4]};
        // end trueDualPortBlockRam"|]

        (_,_) -> error $ "Couldn't generate trueDualPortBlockRam post template"

trueDualPortBlockRamSystemVerilog :: BlackBoxFunction
trueDualPortBlockRamSystemVerilog _isD _primName args _ty = return bb
  where
    meta bbKi = emptyBlackBoxMeta{bbKind=bbKi}
    [_,_,_,_,_,_,_,_,_,_,Left wmA, Left outRegA, Left wmB, Left outRegB,_clkA, _enA, _wEnA, _nAddrA, _datA, _clkB, _enB, _wEnB, _nAddrB, _datB] = args
    bb = case runParse (pack $ I.unindent $ pre ++ portA ++ portB ++ post) of
       Success t    -> Right (meta TDecl, BBTemplate t)
       _            -> Left "internal error: parse fail"
    pre = [I.i|
        // trueDualPortBlockRam begin
        // Shared memory
        logic [~SIZE[~TYP[14]]-1:0] ~GENSYM[mem][0] [~LIT[1]-1:0];

        ~SIGD[~GENSYM[outputA][1]][14];
        ~SIGD[~GENSYM[outputB][2]][19];|] ++
         (if (termToData outRegA == Right True) then [I.i|
        ~SIGD[~GENSYM[outputA2][3]][14];|] else "") ++
         if (termToData outRegB == Right True) then [I.i|
        ~SIGD[~GENSYM[outputB2][4]][19];|] else ""

    portA' = case (termToData wmA) of
      Right WriteFirst -> [I.i|
        // Port A WriteFirst
        always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[10]) begin
            if(~ARG[11]) begin
                ~SYM[1] <= ~SYM[0][~ARG[13]];
                if(~ARG[12]) begin
                    ~SYM[1] <= ~ARG[14];
                    ~SYM[0][~ARG[13]] <= ~ARG[14];
                end|]
      Right ReadFirst -> [I.i|
        // Port A ReadFirst
            always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[10]) begin
                if(~ARG[11]) begin
                    ~SYM[1] <= ~SYM[0][~ARG[13]];
                    if(~ARG[12]) begin
                        ~SYM[0][~ARG[13]] <= ~ARG[14];
                    end|]
      Right NoChange -> [I.i|
        // Port A NoChange
            always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[10]) begin
                if(~ARG[11]) begin
                    if(~ARG[12]) begin
                        ~SYM[0][~ARG[13]] <= ~ARG[14];
                    end else begin
                        ~SYM[1] <= ~SYM[0][~ARG[13]];
                    end|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port A template, type is " ++ (showPpr t)

    portA = portA' ++ (if (termToData outRegA == Right True) then [I.i|
                ~SYM[3] <= ~SYM[1];|] else "") ++ [I.i|
            end
        end|]

    portB' = case (termToData wmB) of
      Right WriteFirst -> [I.i|
        // Port B WriteFirst
        always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[15]) begin
            if(~ARG[16]) begin
                ~SYM[2] <= ~SYM[0][~ARG[18]];
                if(~ARG[17]) begin
                    ~SYM[2] <= ~ARG[19];
                    ~SYM[0][~ARG[18]] <= ~ARG[19];
                end|]
      Right ReadFirst -> [I.i|
        // Port B ReadFirst
        always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[15]) begin
            if(~ARG[16]) begin
                ~SYM[2] <= ~SYM[0][~ARG[18]];
                if(~ARG[17]) begin
                    ~SYM[0][~ARG[18]] <= ~ARG[19];
                end|]
      Right NoChange -> [I.i|
        // Port B NoChange
        always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[15]) begin
            if(~ARG[16]) begin
                if(~ARG[17]) begin
                    ~SYM[0][~ARG[18]] <= ~ARG[19];
                end else begin
                    ~SYM[2] <= ~SYM[0][~ARG[18]];
                end|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port B template, type is " ++ (showPpr t)

    portB = portB' ++ (if (termToData outRegB == Right True) then [I.i|
                ~SYM[4] <= ~SYM[2];|] else "") ++ [I.i|
            end
        end|]

    post = case (termToData outRegA, termToData outRegB) of
        (Right False, Right False) ->   [I.i|
        assign ~RESULT = {~SYM[1], ~SYM[2]};
        // end trueDualPortBlockRam"|]

        (Right False, Right True) ->    [I.i|
        assign ~RESULT = {~SYM[1], ~SYM[4]};
        // end trueDualPortBlockRam"|]

        (Right True, Right False) ->    [I.i|
        assign ~RESULT = {~SYM[3], ~SYM[2]};
        // end trueDualPortBlockRam"|]

        (Right True, Right True) ->     [I.i|
        assign ~RESULT = {~SYM[3], ~SYM[4]};
        // end trueDualPortBlockRam"|]

        (_,_) -> error $ "Couldn't generate trueDualPortBlockRam post template"

trueDualPortBlockRamVHDL :: BlackBoxFunction
trueDualPortBlockRamVHDL _isD _primName args _ty = return bb
  where
    meta bbKi = emptyBlackBoxMeta{bbKind=bbKi}
    [_,_,_,_,_,_,_,_,_,_,Left wmA, Left outRegA, Left wmB, Left outRegB,_clkA, _enA, _wEnA, _nAddrA, _datA, _clkB, _enB, _wEnB, _nAddrB, _datB] = args
    bb = case runParse (pack $ I.unindent $ pre ++ portA ++ portB ++ post) of
       Success t    -> Right (meta TDecl, BBTemplate t)
       _            -> Left "internal error: parse fail"
    pre = [I.i|
        -- trueDualPortBlockRam begin
        ~GENSYM[~RESULT_trueDualPortBlockRam][1] : block
        -- Shared memory
        type mem_type is array (~LIT[1]-1 downto 0 ) of ~TYP[14];
        shared variable mem : mem_type;

        signal ~GENSYM[outputA][2] : ~TYP[14];
        signal ~GENSYM[outputB][3] : ~TYP[19];|] ++
         (if (termToData outRegA == Right True) then [I.i|
        signal ~GENSYM[outputA2][4] : ~TYP[14];|] else "") ++
         (if (termToData outRegB == Right True) then [I.i|
        signal ~GENSYM[outputB2][5] : ~TYP[19];|] else "") ++ [I.i|
        begin
        |]

    portA' = case (termToData wmA) of
      Right WriteFirst -> [I.i|
        -- Port A WriteFirst
        process(~ARG[10])
        begin
            if(rising_edge(~ARG[10])) then
                  if(~ARG[11]) then
                    if(~ARG[12]) then
                        mem(to_integer(~ARG[13])) := ~ARG[14];
                    end if;
                    ~SYM[2] <= mem(to_integer(~ARG[13]));|]
      Right ReadFirst -> [I.i|
        -- Port A ReadFirst
        process(~ARG[10])
        begin
            if(rising_edge(~ARG[10])) then
                  if(~ARG[11]) then
                    ~SYM[2] <= mem(to_integer(~ARG[13]));
                    if(~ARG[12]) then
                        mem(to_integer(~ARG[13])) := ~ARG[14];
                    end if;|]
      Right NoChange -> [I.i|
        -- Port A NoChange
        process(~ARG[10])
        begin
            if(rising_edge(~ARG[10])) then
                  if(~ARG[11]) then
                    if(~ARG[12]) then
                        mem(to_integer(~ARG[13])) := ~ARG[14];
                    else
                        ~SYM[2] <= mem(to_integer(~ARG[13]));
                    end if;|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port A template, type is " ++ (showPpr t)

    portA = portA' ++ (if (termToData outRegA == Right True) then [I.i|
                ~SYM[4] <= ~SYM[2];|] else "") ++ [I.i|
            end if;
        end if;
    end process;|]

    portB' = case (termToData wmB) of
      Right WriteFirst -> [I.i|
        -- Port A WriteFirst
        process(~ARG[15])
        begin
            if(rising_edge(~ARG[15])) then
                  if(~ARG[16]) then
                    if(~ARG[17]) then
                        mem(to_integer(~ARG[18])) := ~ARG[19];
                    end if;
                    ~SYM[3] <= mem(to_integer(~ARG[18]));|]
      Right ReadFirst -> [I.i|
        -- Port A ReadFirst
        process(~ARG[15])
        begin
            if(rising_edge(~ARG[15])) then
                  if(~ARG[16]) then
                    ~SYM[3] <= mem(to_integer(~ARG[18]));
                    if(~ARG[17]) then
                        mem(to_integer(~ARG[18])) := ~ARG[19];
                    end if;|]
      Right NoChange -> [I.i|
        -- Port A NoChange
        process(~ARG[15])
        begin
            if(rising_edge(~ARG[15])) then
                  if(~ARG[16]) then
                    if(~ARG[17]) then
                        mem(to_integer(~ARG[18])) := ~ARG[19];
                    else
                        ~SYM[3] <= mem(to_integer(~ARG[18]));
                    end if;|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port B template, type is " ++ (showPpr t)

    portB = portB' ++ (if (termToData outRegB == Right True) then [I.i|
                ~SYM[5] <= ~SYM[3];|] else "") ++ [I.i|
            end if;
        end if;
    end process;|]

    post = case (termToData outRegA, termToData outRegB) of
        (Right False, Right False) ->   [I.i|
        ~RESULT <= (~SYM[2], ~SYM[3]);
        end block ~SYM[1];
        -- end trueDualPortBlockRam"|]

        (Right False, Right True) ->    [I.i|
        ~RESULT <= (~SYM[2], ~SYM[5]);
        end block ~SYM[1];
        -- end trueDualPortBlockRam"|]

        (Right True, Right False) ->    [I.i|
        ~RESULT <= (~SYM[4], ~SYM[3]);
        end block ~SYM[1];
        -- end trueDualPortBlockRam"|]

        (Right True, Right True) ->     [I.i|
        ~RESULT <= (~SYM[4], ~SYM[5]);
        end block ~SYM[1];
        -- end trueDualPortBlockRam"|]

        (_,_) -> error $ "Couldn't generate trueDualPortBlockRam post template"
