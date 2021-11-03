{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Clash.Primitives.TrueDualPortBlockRam where

import       Data.Text.Lazy           (pack)
import       Text.Trifecta.Result         (Result(Success))
import qualified Data.String.Interpolate      as I
import qualified Data.String.Interpolate.Util     as I

import       Clash.Netlist.BlackBox.Parser    (runParse)
import       Clash.Netlist.BlackBox.Types
  (BlackBoxFunction, BlackBoxMeta(..), TemplateKind(TDecl), emptyBlackBoxMeta)
import       Clash.Netlist.Types(BlackBox(BBTemplate))

import       Clash.Core.TermLiteral
import       Clash.Core.Pretty
import Clash.Explicit.BlockRam(WriteMode(..))

trueDualPortBlockRamVerilog :: BlackBoxFunction
trueDualPortBlockRamVerilog _isD _primName args _ty = return bb
  where
    meta bbKi = emptyBlackBoxMeta{bbKind=bbKi}
    [_,_,_,_,_,_,_,_,_,_,Left wmA, Left outRegA, Left wmB, Left outRegB,_clkA, _enA, _rstA, _wEnA, _nAddrA, _datA, _clkB, _enB, _rstB, _wEnB, _nAddrB, _datB] = args
    bb = case runParse (pack $ I.unindent $ pre ++ portA ++ portB ++ post) of
      Success t  -> Right (meta TDecl, BBTemplate t)
      _      -> Left "internal error: parse fail"

    pre = [I.i|
      // trueDualPortBlockRam begin
      // Shared memory
      reg [~SIZE[~TYP[15]]-1:0] ~GENSYM[mem][0] [~LIT[1]-1:0];

      reg ~SIGD[~GENSYM[outputA][1]][15];
      reg ~SIGD[~GENSYM[outputB][2]][21];|] ++
      (if (termToData outRegA == Right True) then [I.i|
      reg ~SIGD[~GENSYM[outputA2][3]][15];|] else "") ++
      if (termToData outRegB == Right True) then [I.i|
      reg ~SIGD[~GENSYM[outputB2][4]][21];|] else ""

    portAPre = [I.i|
      // Port A
      always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[10], ~IF~ISACTIVEHIGH[2]~THENposedge~ELSEnegedge~FI ~ARG[12]) begin
        if (~IF~ISACTIVEHIGH[2]~THEN~ARG[12]~ELSE!~ARG[12]~FI) begin
          ~SYM[1] <= 0;|] ++ (if (termToData outRegA == Right True) then [I.i|
          ~SYM[3] <= 0;|] else "") ++ [I.i|
        end else begin|]

    portA' = case (termToData wmA) of
      Right WriteFirst -> [I.i|
        //Write first
        if(~ARG[11]) begin
          ~SYM[1] <= ~SYM[0][~ARG[14]];
          if(~ARG[13]) begin
            ~SYM[1] <= ~ARG[15];
            ~SYM[0][~ARG[14]] <= ~ARG[15];
          end|]
      Right ReadFirst -> [I.i|
          //Read first
          if(~ARG[11]) begin
            ~SYM[1] <= ~SYM[0][~ARG[14]];
            if(~ARG[13]) begin
              ~SYM[0][~ARG[14]] <= ~ARG[15];
            end|]
      Right NoChange -> [I.i|
          // No change
          if(~ARG[11]) begin
            if(~ARG[13]) begin
              ~SYM[0][~ARG[14]] <= ~ARG[15];
            end else begin
              ~SYM[1] <= ~SYM[0][~ARG[14]];
            end|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port A template, type is " ++ (showPpr t)

    portA = portAPre ++ portA' ++ (if (termToData outRegA == Right True) then [I.i|
          ~SYM[3] <= ~SYM[1];|] else "") ++ [I.i|
          end
        end
      end|]

    portBPre = [I.i|
      // Port B
      always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[16], ~IF~ISACTIVEHIGH[2]~THENposedge~ELSEnegedge~FI ~ARG[18]) begin
        if (~IF~ISACTIVEHIGH[3]~THEN~ARG[18]~ELSE!~ARG[18]~FI) begin
          ~SYM[2] <= 0;|] ++ (if (termToData outRegB == Right True) then [I.i|
          ~SYM[4] <= 0;|] else "") ++ [I.i|
        end else begin|]

    portB' = case (termToData wmB) of
      Right WriteFirst -> [I.i|
        // Write first
        if(~ARG[17]) begin
          ~SYM[2] <= ~SYM[0][~ARG[20]];
          if(~ARG[19]) begin
            ~SYM[2] <= ~ARG[21];
            ~SYM[0][~ARG[20]] <= ~ARG[21];
          end|]
      Right ReadFirst -> [I.i|
        // Read first
        if(~ARG[17]) begin
          ~SYM[2] <= ~SYM[0][~ARG[20]];
          if(~ARG[19]) begin
            ~SYM[0][~ARG[20]] <= ~ARG[21];
          end|]
      Right NoChange -> [I.i|
        // No change
        if(~ARG[17]) begin
          if(~ARG[19]) begin
            ~SYM[0][~ARG[20]] <= ~ARG[21];
          end else begin
            ~SYM[2] <= ~SYM[0][~ARG[20]];
          end|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port B template, type is " ++ (showPpr t)

    portB = portBPre ++ portB' ++ (if (termToData outRegB == Right True) then [I.i|
          ~SYM[4] <= ~SYM[2];|] else "") ++ [I.i|
          end
        end
      end|]

    post = case (termToData outRegA, termToData outRegB) of
      (Right False, Right False) ->   [I.i|
      assign ~RESULT = {~SYM[1], ~SYM[2]};
      // end trueDualPortBlockRam"|]

      (Right False, Right True) ->  [I.i|
      assign ~RESULT = {~SYM[1], ~SYM[4]};
      // end trueDualPortBlockRam"|]

      (Right True, Right False) ->  [I.i|
      assign ~RESULT = {~SYM[3], ~SYM[2]};
      // end trueDualPortBlockRam"|]

      (Right True, Right True) ->   [I.i|
      assign ~RESULT = {~SYM[3], ~SYM[4]};
      // end trueDualPortBlockRam"|]

      (_,_) -> error $ "Couldn't generate trueDualPortBlockRam post template"

trueDualPortBlockRamSystemVerilog :: BlackBoxFunction
trueDualPortBlockRamSystemVerilog _isD _primName args _ty = return bb
  where
    meta bbKi = emptyBlackBoxMeta{bbKind=bbKi}
    [_,_,_,_,_,_,_,_,_,_,Left wmA, Left outRegA, Left wmB, Left outRegB,_clkA, _enA, _rstA, _wEnA, _nAddrA, _datA, _clkB, _enB, _rstB, _wEnB, _nAddrB, _datB] = args
    bb = case runParse (pack $ I.unindent $ pre ++ portA ++ portB ++ post) of
      Success t  -> Right (meta TDecl, BBTemplate t)
      _      -> Left "internal error: parse fail"
    pre = [I.i|
      // trueDualPortBlockRam begin
      // Shared memory
      logic [~SIZE[~TYP[15]]-1:0] ~GENSYM[mem][0] [~LIT[1]-1:0];

      ~SIGD[~GENSYM[outputA][1]][15];
      ~SIGD[~GENSYM[outputB][2]][21];|] ++
      (if (termToData outRegA == Right True) then [I.i|
      ~SIGD[~GENSYM[outputA2][3]][15];|] else "") ++
      if (termToData outRegB == Right True) then [I.i|
      ~SIGD[~GENSYM[outputB2][4]][21];|] else ""

    portAPre = [I.i|
      // Port A
      always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[10], ~IF~ISACTIVEHIGH[2]~THENposedge~ELSEnegedge~FI ~ARG[12]) begin
        if (~IF~ISACTIVEHIGH[2]~THEN~ARG[12]~ELSE!~ARG[12]~FI) begin
          ~SYM[1] <= 0;|] ++ (if (termToData outRegA == Right True) then [I.i|
          ~SYM[3] <= 0;|] else "") ++ [I.i|
        end else begin|]

    portA' = case (termToData wmA) of
      Right WriteFirst -> [I.i|
        // Write first
        if(~ARG[11]) begin
          ~SYM[1] <= ~SYM[0][~ARG[14]];
          if(~ARG[13]) begin
            ~SYM[1] <= ~ARG[15];
            ~SYM[0][~ARG[14]] <= ~ARG[15];
          end|]
      Right ReadFirst -> [I.i|
        // Read first
        if(~ARG[11]) begin
          ~SYM[1] <= ~SYM[0][~ARG[14]];
          if(~ARG[13]) begin
            ~SYM[0][~ARG[14]] <= ~ARG[15];
          end|]
      Right NoChange -> [I.i|
        // No change
        if(~ARG[11]) begin
          if(~ARG[13]) begin
            ~SYM[0][~ARG[14]] <= ~ARG[15];
          end else begin
            ~SYM[1] <= ~SYM[0][~ARG[14]];
          end|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port A template, type is " ++ (showPpr t)

    portA = portAPre ++ portA' ++ (if (termToData outRegA == Right True) then [I.i|
          ~SYM[3] <= ~SYM[1];|] else "") ++ [I.i|
          end
        end
      end|]

    portBPre = [I.i|
      // Port B
      always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[16], ~IF~ISACTIVEHIGH[3]~THENposedge~ELSEnegedge~FI ~ARG[18]) begin
        if (~IF~ISACTIVEHIGH[3]~THEN~ARG[18]~ELSE!~ARG[18]~FI) begin
          ~SYM[2] <= 0;|] ++ (if (termToData outRegB == Right True) then [I.i|
          ~SYM[4] <= 0;|] else "") ++ [I.i|
        end else begin|]

    portB' = case (termToData wmB) of
      Right WriteFirst -> [I.i|
          // Write first
          if(~ARG[17]) begin
            ~SYM[2] <= ~SYM[0][~ARG[20]];
            if(~ARG[19]) begin
              ~SYM[2] <= ~ARG[21];
              ~SYM[0][~ARG[20]] <= ~ARG[21];
            end|]
      Right ReadFirst -> [I.i|
          // Read first
          if(~ARG[17]) begin
            ~SYM[2] <= ~SYM[0][~ARG[20]];
            if(~ARG[19]) begin
              ~SYM[0][~ARG[20]] <= ~ARG[21];
            end|]
      Right NoChange -> [I.i|
          // No change
          if(~ARG[17]) begin
            if(~ARG[19]) begin
              ~SYM[0][~ARG[20]] <= ~ARG[21];
            end else begin
              ~SYM[2] <= ~SYM[0][~ARG[20]];
            end|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port B template, type is " ++ (showPpr t)

    portB = portBPre ++ portB' ++ (if (termToData outRegB == Right True) then [I.i|
          ~SYM[4] <= ~SYM[2];|] else "") ++ [I.i|
          end
        end
      end|]

    post = case (termToData outRegA, termToData outRegB) of
      (Right False, Right False) ->   [I.i|
      assign ~RESULT = {~SYM[1], ~SYM[2]};
      // end trueDualPortBlockRam"|]

      (Right False, Right True) ->  [I.i|
      assign ~RESULT = {~SYM[1], ~SYM[4]};
      // end trueDualPortBlockRam"|]

      (Right True, Right False) ->  [I.i|
      assign ~RESULT = {~SYM[3], ~SYM[2]};
      // end trueDualPortBlockRam"|]

      (Right True, Right True) ->   [I.i|
      assign ~RESULT = {~SYM[3], ~SYM[4]};
      // end trueDualPortBlockRam"|]

      (_,_) -> error $ "Couldn't generate trueDualPortBlockRam post template"

trueDualPortBlockRamVHDL :: BlackBoxFunction
trueDualPortBlockRamVHDL _isD _primName args _ty = return bb
  where
    meta bbKi = emptyBlackBoxMeta{bbKind=bbKi}
    [_,_,_,_,_,_,_,_,_,_,Left wmA, Left outRegA, Left wmB, Left outRegB,_clkA, _enA, _rstA, _wEnA, _nAddrA, _datA, _clkB, _enB, _rstB, _wEnB, _nAddrB, _datB] = args
    bb = case runParse (pack $ I.unindent $ pre ++ portA ++ portB ++ post) of
      Success t  -> Right (meta TDecl, BBTemplate t)
      _      -> Left "internal error: parse fail"
    pre = [I.i|
      -- trueDualPortBlockRam begin
      ~GENSYM[~RESULT_trueDualPortBlockRam][1] : block
      -- Shared memory
      type mem_type is array (~LIT[1]-1 downto 0 ) of ~TYP[15];
      shared variable mem : mem_type;

      signal ~GENSYM[outputA][2] : ~TYP[15];
      signal ~GENSYM[outputB][3] : ~TYP[21];|] ++
      (if (termToData outRegA == Right True) then [I.i|
      signal ~GENSYM[outputA2][4] : ~TYP[15];|] else "") ++
      (if (termToData outRegB == Right True) then [I.i|
      signal ~GENSYM[outputB2][5] : ~TYP[21];|] else "") ++ [I.i|
      begin
      |]

    portAPre = [I.i|
      -- Port A
      process(~ARG[10], ~ARG[12])
      begin
        if (~ARG[12] = ~IF~ISACTIVEHIGH[2]~THEN'1'~ELSE'0'~FI) then
          ~SYM[2] <= std_logic_vector(to_unsigned(0,~ARG[15]'length));|] ++ (if (termToData outRegA == Right True) then [I.i|
          ~SYM[4] <= std_logic_vector(to_unsigned(0,~ARG[15]'length));|] else "") ++ [I.i|
        elsif ~IF~ACTIVEEDGE[Rising][2]~THENrising_edge~ELSEfalling_edge~FI(~ARG[10]) then|]

    portA' = case (termToData wmA) of
      Right WriteFirst -> [I.i|
          -- Write first
          if(~ARG[11]) then
            if(~ARG[13]) then
              mem(to_integer(~ARG[14])) := ~ARG[15];
            end if;
            ~SYM[2] <= mem(to_integer(~ARG[14]));|]
      Right ReadFirst -> [I.i|
          -- Read first
          if(~ARG[11]) then
            ~SYM[2] <= mem(to_integer(~ARG[14]));
            if(~ARG[13]) then
              mem(to_integer(~ARG[14])) := ~ARG[15];
            end if;|]
      Right NoChange -> [I.i|
          -- No change
          if(~ARG[11]) then
            if(~ARG[13]) then
              mem(to_integer(~ARG[14])) := ~ARG[15];
            else
              ~SYM[2] <= mem(to_integer(~ARG[14]));
            end if;|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port A template, type is " ++ (showPpr t)

    portA = portAPre ++ portA' ++ (if (termToData outRegA == Right True) then [I.i|
          ~SYM[4] <= ~SYM[2];|] else "") ++ [I.i|
        end if;
      end if;
    end process;|]

    portBPre = [I.i|
      -- Port B
      process(~ARG[16], ~ARG[18])
      begin
        if (~ARG[18] = ~IF~ISACTIVEHIGH[3]~THEN'1'~ELSE'0'~FI) then
          ~SYM[3] <= std_logic_vector(to_unsigned(0,~ARG[21]'length));|] ++ (if (termToData outRegB == Right True) then [I.i|
          ~SYM[5] <= std_logic_vector(to_unsigned(0,~ARG[21]'length));|] else "") ++ [I.i|
        elsif ~IF~ACTIVEEDGE[Rising][3]~THENrising_edge~ELSEfalling_edge~FI(~ARG[16]) then|]

    portB' = case (termToData wmB) of
      Right WriteFirst -> [I.i|
          -- Write first
          if(~ARG[17]) then
            if(~ARG[19]) then
              mem(to_integer(~ARG[20])) := ~ARG[21];
            end if;
            ~SYM[3] <= mem(to_integer(~ARG[20]));|]
      Right ReadFirst -> [I.i|
          -- Read first
          if(~ARG[17]) then
            ~SYM[3] <= mem(to_integer(~ARG[20]));
            if(~ARG[19]) then
              mem(to_integer(~ARG[20])) := ~ARG[21];
            end if;|]
      Right NoChange -> [I.i|
          -- No change
          if(~ARG[17]) then
            if(~ARG[19]) then
              mem(to_integer(~ARG[20])) := ~ARG[21];
            else
              ~SYM[3] <= mem(to_integer(~ARG[20]));
            end if;|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port B template, type is " ++ (showPpr t)

    portB = portBPre ++ portB' ++ (if (termToData outRegB == Right True) then [I.i|
          ~SYM[5] <= ~SYM[3];|] else "") ++ [I.i|
        end if;
      end if;
    end process;|]

    post = case (termToData outRegA, termToData outRegB) of
      (Right False, Right False) ->   [I.i|
      ~RESULT <= (~SYM[2], ~SYM[3]);
      end block ~SYM[1];
      -- end trueDualPortBlockRam"|]

      (Right False, Right True) ->  [I.i|
      ~RESULT <= (~SYM[2], ~SYM[5]);
      end block ~SYM[1];
      -- end trueDualPortBlockRam"|]

      (Right True, Right False) ->  [I.i|
      ~RESULT <= (~SYM[4], ~SYM[3]);
      end block ~SYM[1];
      -- end trueDualPortBlockRam"|]

      (Right True, Right True) ->   [I.i|
      ~RESULT <= (~SYM[4], ~SYM[5]);
      end block ~SYM[1];
      -- end trueDualPortBlockRam"|]

      (_,_) -> error $ "Couldn't generate trueDualPortBlockRam post template"
