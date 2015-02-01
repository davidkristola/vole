with Interfaces;

with kv.avm.references; use kv.avm.references;

package kv.avm.Instructions is

   pragma preelaborate;

   Invalid_Compute_Operation : exception;

   type operation_type is (Add, Sub, Mul, Div, Modulo, Exp, Eq, Neq, L_t, Lte, G_t, Gte, Shift_Left, Shift_Right, B_And, B_Or, B_Xor, Negate, Op_Pos, Op_Neg);
   for operation_type'SIZE use 5;

   subtype Binary_Op_Type is Operation_Type range Add .. B_Xor;
   subtype Unary_Op_Type is Operation_Type range Negate .. Op_Neg;
   subtype Compare_Type is Operation_Type range Eq .. Gte;

   type op_code_type is
      (terminate_program, halt_actor, stop_frame, no_op, -- format 0
       emit, assert, reply, jump, -- format 1
       jump_abs, jump_rel, -- format 1b
       set, fold, -- format 2
       branch_abs, branch_rel, branch_neq, -- format 2b
       peek, trap, new_actor, -- format 3
       peek_immediate, -- format 3b
       compute, -- format 4
       Actor_Call, -- format 5a1 reply := args=>actor.message
       Actor_Send, Actor_Tail_Send, Actor_Tail_Call, -- format 5a2 args=>actor.message (no reply to me)
       Self_Call, Super_Call, -- format 5b1 reply := args=>message
       Self_Send, Self_Tail_Send, Self_Tail_Call, Super_Tail_Call); -- format 5b2 args=>message
   for op_code_type'SIZE use 6;

   subtype format_0_type is op_code_type range terminate_program .. no_op;
   subtype format_1_type is op_code_type range emit .. jump;
   subtype format_1b_type is op_code_type range jump_abs .. jump_rel;
   subtype format_2_type is op_code_type range set .. fold;
   subtype format_2b_type is op_code_type range branch_abs .. branch_neq;
   subtype format_3_type is op_code_type range peek .. new_actor;
   subtype format_3b_type is op_code_type range peek_immediate .. peek_immediate;
   subtype format_4_type is op_code_type range compute .. compute;
   subtype format_5a1_type is op_code_type range Actor_Call .. Actor_Call;
   subtype format_5a2_type is op_code_type range Actor_Send .. Actor_Tail_Call;
   subtype format_5b1_type is op_code_type range Self_Call .. Super_Call;
   subtype format_5b2_type is op_code_type range Self_Send .. Super_Tail_Call;

   type Instruction_Type(op_code : op_code_type := no_op) is
      record
         case op_code is
            when format_0_type =>
               null;
            when format_1_type =>
               value : reference_type;
            when format_1b_type =>
               jump : Interfaces.Unsigned_32;
            when format_2_type =>
               lhs : reference_type;
               rhs : reference_type;
            when format_2b_type =>
               condition : reference_type;
               target : Interfaces.Unsigned_32;
            when format_3_type =>
               a : reference_type;
               x : reference_type;
               y : reference_type;
            when format_3b_type =>
               lvalue : reference_type;
               tuple  : reference_type;
               index  : offset_type;
            when format_4_type =>
               action : operation_type;
               result : reference_type;
               left   : reference_type;
               right  : reference_type;
            when format_5a1_type =>
               rply_5a1 : reference_type;
               args_5a1 : reference_type;
               actr_5a1 : reference_type;
               mdef_5a1 : reference_type;
            when format_5a2_type =>
               args_5a2 : reference_type;
               actr_5a2 : reference_type;
               mdef_5a2 : reference_type;
            when format_5b1_type =>
               rply_5b1 : reference_type;
               mdef_5b1 : reference_type;
               args_5b1 : reference_type;
            when format_5b2_type =>
               mdef_5b2 : reference_type;
               args_5b2 : reference_type;
            when others =>
               null;
         end case;
      end record;
   for Instruction_Type use
      record
          op_code at 0 range 0 .. 5;
          value at 2 range 0 .. 15;
          lhs at 4 range 0 .. 15;
          rhs at 6 range 0 .. 15;

          jump at 4 range 0 .. 31;

          condition at 2 range 0 .. 15;
          target at 4 range 0 .. 31;

          a at 2 range 0 .. 15;
          x at 4 range 0 .. 15;
          y at 6 range 0 .. 15;

          lvalue at 2 range 0 .. 15;
          tuple  at 4 range 0 .. 15;
          index  at 6 range 0 .. 15;

          action at 0 range 6 .. 11;
          result at 2 range 0 .. 15;
          left   at 4 range 0 .. 15;
          right  at 6 range 0 .. 15;

          rply_5a1 at 2 range 00 .. 11;
          args_5a1 at 2 range 12 .. 23;
          actr_5a1 at 2 range 24 .. 35;
          mdef_5a1 at 2 range 36 .. 47;

          args_5a2 at 2 range 0 .. 15;
          actr_5a2 at 4 range 0 .. 15;
          mdef_5a2 at 6 range 0 .. 15;

          rply_5b1 at 2 range 0 .. 15;
          mdef_5b1 at 4 range 0 .. 15;
          args_5b1 at 6 range 0 .. 15;

          mdef_5b2 at 4 range 0 .. 15;
          args_5b2 at 6 range 0 .. 15;
      end record;
   for Instruction_Type'SIZE use 64;

   function Encode_Operation(Op : Operation_Type) return String;
   function Decode_Operation(Op_Img : String) return Operation_Type;

   type Code_Type is array (Interfaces.Unsigned_32 range <>) of Instruction_Type;
   type Code_Access is access all Code_Type;


end kv.avm.Instructions;
