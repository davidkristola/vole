
with Ada.Text_IO;
with Ada.Exceptions;
with Text_IO; -- Old style
with Vole_Tokens;
with Vole_Goto;
with Vole_Shift_Reduce;
with vole_lex_dfa;
with kv.avm.Vole_Lex;
with kv.avm.Vole_Tree;
with kv.avm.Instructions;
with kv.avm.Registers;

use Ada.Text_IO;
use Ada.Exceptions;
use Text_IO; -- Old style
use Vole_Tokens;
use Vole_Goto;
use Vole_Shift_Reduce;
use vole_lex_dfa;
use kv.avm.Vole_Lex;
use kv.avm.Vole_Tree;
use kv.avm.Instructions;
use kv.avm.Registers;

package body kv.avm.vole_parser is

   procedure YYError(Text : in String) is
   begin
      New_Line;
      Put_Line("PARSE ERROR on line"&Positive'IMAGE(kv.avm.Vole_Lex.Line_Number)&" parsing '"&vole_lex_dfa.yytext&"'");
   end YYError;

procedure YYParse is

   -- Rename User Defined Packages to Internal Names.
    package yy_goto_tables         renames
      Vole_Goto;
    package yy_shift_reduce_tables renames
      Vole_Shift_Reduce;
    package yy_tokens              renames
      Vole_Tokens;

   use yy_tokens, yy_goto_tables, yy_shift_reduce_tables;

   procedure yyerrok;
   procedure yyclearin;


   package yy is

       -- the size of the value and state stacks
       stack_size : constant Natural := 300;

       -- subtype rule         is natural;
       subtype parse_state  is natural;
       -- subtype nonterminal  is integer;

       -- encryption constants
       default           : constant := -1;
       first_shift_entry : constant :=  0;
       accept_code       : constant := -3001;
       error_code        : constant := -3000;

       -- stack data used by the parser
       tos                : natural := 0;
       value_stack        : array(0..stack_size) of yy_tokens.yystype;
       state_stack        : array(0..stack_size) of parse_state;

       -- current input symbol and action the parser is on
       action             : integer;
       rule_id            : rule;
       input_symbol       : yy_tokens.token;


       -- error recovery flag
       error_flag : natural := 0;
          -- indicates  3 - (number of valid shifts after an error occurs)

       look_ahead : boolean := true;
       index      : integer;

       -- Is Debugging option on or off
        DEBUG : boolean renames Verbose;

    end yy;


    function goto_state
      (state : yy.parse_state;
       sym   : nonterminal) return yy.parse_state;

    function parse_action
      (state : yy.parse_state;
       t     : yy_tokens.token) return integer;

    pragma inline(goto_state, parse_action);


    function goto_state(state : yy.parse_state;
                        sym   : nonterminal) return yy.parse_state is
        index : integer;
    begin
        index := goto_offset(state);
        while  integer(goto_matrix(index).nonterm) /= sym loop
            index := index + 1;
        end loop;
        return integer(goto_matrix(index).newstate);
    end goto_state;


    function parse_action(state : yy.parse_state;
                          t     : yy_tokens.token) return integer is
        index      : integer;
        tok_pos    : integer;
        default    : constant integer := -1;
    begin
        tok_pos := yy_tokens.token'pos(t);
        index   := shift_reduce_offset(state);
        while integer(shift_reduce_matrix(index).t) /= tok_pos and then
              integer(shift_reduce_matrix(index).t) /= default
        loop
            index := index + 1;
        end loop;
        return integer(shift_reduce_matrix(index).act);
    end parse_action;

-- error recovery stuff

    procedure handle_error is
      temp_action : integer;
    begin

      if yy.error_flag = 3 then -- no shift yet, clobber input.
      if yy.debug then
          text_io.put_line("Ayacc.YYParse: Error Recovery Clobbers " &
                   yy_tokens.token'image(yy.input_symbol));
      end if;
        if yy.input_symbol = yy_tokens.end_of_input then  -- don't discard,
        if yy.debug then
            text_io.put_line("Ayacc.YYParse: Can't discard END_OF_INPUT, quiting...");
        end if;
        raise yy_tokens.syntax_error;
        end if;

            yy.look_ahead := true;   -- get next token
        return;                  -- and try again...
    end if;

    if yy.error_flag = 0 then -- brand new error
        yyerror("Syntax Error");
    end if;

    yy.error_flag := 3;

    -- find state on stack where error is a valid shift --

    if yy.debug then
        text_io.put_line("Ayacc.YYParse: Looking for state with error as valid shift");
    end if;

    loop
        if yy.debug then
          text_io.put_line("Ayacc.YYParse: Examining State " &
               yy.parse_state'image(yy.state_stack(yy.tos)));
        end if;
        temp_action := parse_action(yy.state_stack(yy.tos), error);

            if temp_action >= yy.first_shift_entry then
                if yy.tos = yy.stack_size then
                    text_io.put_line(" Stack size exceeded on state_stack");
                    raise yy_Tokens.syntax_error;
                end if;
                yy.tos := yy.tos + 1;
                yy.state_stack(yy.tos) := temp_action;
                exit;
            end if;

        Decrement_Stack_Pointer :
        begin
          yy.tos := yy.tos - 1;
        exception
          when Constraint_Error =>
            yy.tos := 0;
        end Decrement_Stack_Pointer;

        if yy.tos = 0 then
          if yy.debug then
            text_io.put_line("Ayacc.YYParse: Error recovery popped entire stack, aborting...");
          end if;
          raise yy_tokens.syntax_error;
        end if;
    end loop;

    if yy.debug then
        text_io.put_line("Ayacc.YYParse: Shifted error token in state " &
              yy.parse_state'image(yy.state_stack(yy.tos)));
    end if;

    end handle_error;

   -- print debugging information for a shift operation
   procedure shift_debug(state_id: yy.parse_state; lexeme: yy_tokens.token) is
   begin
       text_io.put_line("Ayacc.YYParse: Shift "& yy.parse_state'image(state_id)&" on input symbol "&
               yy_tokens.token'image(lexeme) );
   end;

   -- print debugging information for a reduce operation
   procedure reduce_debug(rule_id: rule; state_id: yy.parse_state) is
   begin
       text_io.put_line("Ayacc.YYParse: Reduce by rule "&rule'image(rule_id)&" goto state "&
               yy.parse_state'image(state_id));
   end;

   -- make the parser believe that 3 valid shifts have occured.
   -- used for error recovery.
   procedure yyerrok is
   begin
       yy.error_flag := 0;
   end yyerrok;

   -- called to clear input symbol that caused an error.
   procedure yyclearin is
   begin
       -- yy.input_symbol := yylex;
       yy.look_ahead := true;
   end yyclearin;


begin
    -- initialize by pushing state 0 and getting the first input symbol
    yy.state_stack(yy.tos) := 0;


    loop

        yy.index := shift_reduce_offset(yy.state_stack(yy.tos));
        if integer(shift_reduce_matrix(yy.index).t) = yy.default then
            yy.action := integer(shift_reduce_matrix(yy.index).act);
        else
            if yy.look_ahead then
                yy.look_ahead   := false;

                yy.input_symbol := yylex;
            end if;
            yy.action :=
             parse_action(yy.state_stack(yy.tos), yy.input_symbol);
        end if;


        if yy.action >= yy.first_shift_entry then  -- SHIFT

            if yy.debug then
                shift_debug(yy.action, yy.input_symbol);
            end if;

            -- Enter new state
            if yy.tos = yy.stack_size then
                text_io.put_line(" Stack size exceeded on state_stack");
                raise yy_Tokens.syntax_error;
            end if;
            yy.tos := yy.tos + 1;
            yy.state_stack(yy.tos) := yy.action;
              yy.value_stack(yy.tos) := yylval;

        if yy.error_flag > 0 then  -- indicate a valid shift
            yy.error_flag := yy.error_flag - 1;
        end if;

            -- Advance lookahead
            yy.look_ahead := true;

        elsif yy.action = yy.error_code then       -- ERROR

            handle_error;

        elsif yy.action = yy.accept_code then
            if yy.debug then
                text_io.put_line("Ayacc.YYParse: Accepting Grammar...");
            end if;
            exit;

        else -- Reduce Action

            -- Convert action into a rule
            yy.rule_id  := -1 * yy.action;

            -- Execute User Action
            -- user_action(yy.rule_id);


                case yy.rule_id is

when  1 =>
--#line  64

         Build_Program(
yyval.Node, Line_Number, 
yy.value_stack(yy.tos-1).Node, 
yy.value_stack(yy.tos).Node);
         Save_Program(
yyval.Node);
      

when  4 =>
--#line  76
Put_Line("unimp 01");

when  5 =>
--#line  80
Put_Line("unimp 02");

when  6 =>
--#line  84
Put_Line("unimp 03");

when  7 =>
--#line  89

         
yyval.Node := 
yy.value_stack(yy.tos-2).Node; -- Copy up
         
yy.value_stack(yy.tos-2).Node := null; -- Only keep one reference to this node.
         Add_Next(
yyval.Node, 
yy.value_stack(yy.tos).Node);
      

when  8 =>
--#line  97
 
yyval.Node := null;

when  9 =>
--#line  98
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  10 =>
--#line  103
 Build_Actor_Node(
yyval.Node, Line_Number, 
yy.value_stack(yy.tos-4).Node, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos-1).Node); 

when  11 =>
--#line  105
 Build_Actor_Node(
yyval.Node, Line_Number, 
yy.value_stack(yy.tos-6).Node, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos-1).Node, 
yy.value_stack(yy.tos-4).Node); 

when  12 =>
--#line  109
 
yyval.Node := null;

when  13 =>
--#line  110
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  14 =>
--#line  115

         
yyval.Node := 
yy.value_stack(yy.tos-1).Node; -- Copy up
         
yy.value_stack(yy.tos-1).Node := null; -- Only keep one reference to this node.
         Add_Next(
yyval.Node, 
yy.value_stack(yy.tos).Node);
      

when  15 =>
--#line  123
Build_Attribute(
yyval.Node, Line_Number, 
yy.value_stack(yy.tos-3).Node, 
yy.value_stack(yy.tos-1).Node);

when  16 =>
--#line  124
Build_Attribute(
yyval.Node, Line_Number, 
yy.value_stack(yy.tos-1).Node, 
yy.value_stack(yy.tos).Node);

when  17 =>
--#line  128
Build_Kind(
yyval.Node, Line_Number, Bit_Or_Boolean);

when  18 =>
--#line  129
Build_Kind(
yyval.Node, Line_Number, Bit_Or_Boolean, 
yy.value_stack(yy.tos-1).Node);

when  19 =>
--#line  133
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  20 =>
--#line  134
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  21 =>
--#line  135
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  22 =>
--#line  136
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  23 =>
--#line  137
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  24 =>
--#line  138
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  25 =>
--#line  139
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  26 =>
--#line  144
Build_Kind(
yyval.Node, Line_Number, Signed_Integer);

when  27 =>
--#line  146
Build_Kind(
yyval.Node, Line_Number, Signed_Integer, 
yy.value_stack(yy.tos).Node);

when  28 =>
--#line  151
Build_Kind(
yyval.Node, Line_Number, Unsigned_Integer);

when  29 =>
--#line  153
Build_Kind(
yyval.Node, Line_Number, Unsigned_Integer, 
yy.value_stack(yy.tos).Node);

when  30 =>
--#line  157
Put_Line("unimp 06");

when  31 =>
--#line  158
Put_Line("unimp 07");

when  32 =>
--#line  162
Put_Line("unimp 08");

when  33 =>
--#line  163
Build_Kind(
yyval.Node, Line_Number, Immutable_String, 
yy.value_stack(yy.tos).Node);

when  34 =>
--#line  168
Build_Kind(
yyval.Node, Line_Number, Actor_Definition);

when  35 =>
--#line  170
Build_Kind(
yyval.Node, Line_Number, Actor_Definition, 
yy.value_stack(yy.tos).Node);

when  36 =>
--#line  172
Build_Kind(
yyval.Node, Line_Number, Actor_Definition, New_Constructor_Send(
yy.value_stack(yy.tos-1).Node, 
yy.value_stack(yy.tos).Node)); 

when  37 =>
--#line  176
Put_Line("unimp 10");

when  38 =>
--#line  177
Put_Line("unimp 11");

when  39 =>
--#line  182
Build_Kind(
yyval.Node, Line_Number, Bit_Or_Boolean);

when  40 =>
--#line  184
Build_Kind(
yyval.Node, Line_Number, Bit_Or_Boolean, 
yy.value_stack(yy.tos).Node);

when  41 =>
--#line  190
Build_Kind(
yyval.Node, Line_Number, Signed_Integer);

when  42 =>
--#line  192
Build_Kind(
yyval.Node, Line_Number, Unsigned_Integer);

when  43 =>
--#line  194
Build_Kind(
yyval.Node, Line_Number, Floatingpoint);

when  44 =>
--#line  196
Build_Kind(
yyval.Node, Line_Number, Actor_Definition);

when  45 =>
--#line  198
Build_Kind(
yyval.Node, Line_Number, Tuple);

when  46 =>
--#line  200
Build_Kind(
yyval.Node, Line_Number, Immutable_String);

when  47 =>
--#line  205

         
yyval.Node := 
yy.value_stack(yy.tos-2).Node; -- Copy up
         
yy.value_stack(yy.tos-2).Node := null; -- Only keep one reference to this node.
         Add_Next(
yyval.Node, 
yy.value_stack(yy.tos).Node);
      

when  48 =>
--#line  213
 
yyval.Node := null;

when  49 =>
--#line  214
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  50 =>
--#line  219
Build_Message(
yyval.Node, Line_Number, True, 
yy.value_stack(yy.tos-4).Node, 
yy.value_stack(yy.tos-3).Node, 
yy.value_stack(yy.tos-1).Node, 
yy.value_stack(yy.tos).Node, null);

when  51 =>
--#line  221
Build_Message(
yyval.Node, Line_Number, True, 
yy.value_stack(yy.tos-4).Node, 
yy.value_stack(yy.tos-3).Node, 
yy.value_stack(yy.tos-1).Node, 
yy.value_stack(yy.tos).Node, 
yy.value_stack(yy.tos-6).Node);

when  52 =>
--#line  223
Build_Message(
yyval.Node, Line_Number, False, 
yy.value_stack(yy.tos-4).Node, 
yy.value_stack(yy.tos-3).Node, 
yy.value_stack(yy.tos-1).Node, 
yy.value_stack(yy.tos).Node, null);

when  53 =>
--#line  225
Build_Constructor(
yyval.Node, Line_Number, 
yy.value_stack(yy.tos-1).Node, 
yy.value_stack(yy.tos).Node);

when  54 =>
--#line  230

yyval.Node := 
yy.value_stack(yy.tos-1).Node;

when  55 =>
--#line  234
 
yyval.Node := null;

when  56 =>
--#line  235
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  57 =>
--#line  240

yyval.Node := 
yy.value_stack(yy.tos).Node;

when  58 =>
--#line  242

         
yyval.Node := 
yy.value_stack(yy.tos-2).Node; -- Copy up
         
yy.value_stack(yy.tos-2).Node := null; -- Only keep one reference to this node.
         Add_Next(
yyval.Node, 
yy.value_stack(yy.tos).Node);
      

when  59 =>
--#line  251
Build_Arg(
yyval.Node, Line_Number, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  60 =>
--#line  256
Build_Id_Node(
yyval.Node, Line_Number, yytext);

when  61 =>
--#line  260
 
yyval.Node := 
yy.value_stack(yy.tos-1).Node;

when  62 =>
--#line  264
 
yyval.Node := null;

when  63 =>
--#line  265
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  64 =>
--#line  270

         
yyval.Node := 
yy.value_stack(yy.tos-2).Node; -- Copy up
         
yy.value_stack(yy.tos-2).Node := null; -- Only keep one reference to this node.
         Add_Next(
yyval.Node, 
yy.value_stack(yy.tos).Node);
      

when  65 =>
--#line  278
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  66 =>
--#line  279
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  67 =>
--#line  280
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  68 =>
--#line  281
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  69 =>
--#line  282
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  70 =>
--#line  283
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  71 =>
--#line  284
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  72 =>
--#line  285
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  73 =>
--#line  289
Build_While(
yyval.Node, Line_Number, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  74 =>
--#line  290
Build_While(
yyval.Node, Line_Number, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  75 =>
--#line  291
Build_While(
yyval.Node, Line_Number, null,    
yy.value_stack(yy.tos).Node);

when  76 =>
--#line  295
Build_Assert(
yyval.Node, Line_Number, 
yy.value_stack(yy.tos).Node);

when  77 =>
--#line  299
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  78 =>
--#line  303
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  79 =>
--#line  304
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  80 =>
--#line  308
Build_If(
yyval.Node, Line_Number, 
yy.value_stack(yy.tos-4).Node, 
yy.value_stack(yy.tos-1).Node, 
yy.value_stack(yy.tos).Node);

when  81 =>
--#line  312
 
yyval.Node := null;

when  82 =>
--#line  313
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  83 =>
--#line  314
Build_If(
yyval.Node, Line_Number, 
yy.value_stack(yy.tos-5).Node, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos-1).Node);

when  84 =>
--#line  318
Build_Return(
yyval.Node, Line_Number, 
yy.value_stack(yy.tos).Node);

when  85 =>
--#line  322
 
yyval.Node := null;

when  86 =>
--#line  323
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  87 =>
--#line  324
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  88 =>
--#line  325
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  89 =>
--#line  326
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  90 =>
--#line  327
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  91 =>
--#line  332
 
yyval.Node := 
yy.value_stack(yy.tos-1).Node; 

when  92 =>
--#line  336
Put_Line("unimp 30");

when  93 =>
--#line  341
Put_Line("unimp 29");

when  94 =>
--#line  345
Build_Send_Statement(
yyval.Node, Line_Number, Self, null, 
yy.value_stack(yy.tos-1).Node, 
yy.value_stack(yy.tos).Node);

when  95 =>
--#line  349
Build_Send_Statement(
yyval.Node, Line_Number, Actor, 
yy.value_stack(yy.tos-3).Node, 
yy.value_stack(yy.tos-1).Node, 
yy.value_stack(yy.tos).Node);

when  96 =>
--#line  353
Build_Call_Statement(
yyval.Node, Line_Number, Self, null, 
yy.value_stack(yy.tos-1).Node, 
yy.value_stack(yy.tos).Node);

when  97 =>
--#line  357
Build_Call_Statement(
yyval.Node, Line_Number, Super, null, 
yy.value_stack(yy.tos-1).Node, 
yy.value_stack(yy.tos).Node);

when  98 =>
--#line  361
Build_Call_Statement(
yyval.Node, Line_Number, Actor, 
yy.value_stack(yy.tos-3).Node, 
yy.value_stack(yy.tos-1).Node, 
yy.value_stack(yy.tos).Node);

when  99 =>
--#line  365
Build_Assignment(
yyval.Node, Line_Number, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  100 =>
--#line  366
Raise_Exception(Unimplemented_Error'IDENTITY, "Rule statement_assign (tuple)");

when  101 =>
--#line  370
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  102 =>
--#line  371
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  103 =>
--#line  372
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  104 =>
--#line  373
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  105 =>
--#line  377
 Build_Op_Expression(
yyval.Node, Line_Number, Op_Pos, null, 
yy.value_stack(yy.tos).Node); 

when  106 =>
--#line  381
 Build_Kind(
yyval.Node, Line_Number, Actor_Definition, New_Constructor_Send(
yy.value_stack(yy.tos-1).Node, 
yy.value_stack(yy.tos).Node)); 

when  107 =>
--#line  386
Build_Var_Expression(
yyval.Node, Line_Number, True, 
yy.value_stack(yy.tos).Node);

when  108 =>
--#line  388
Build_Var_Expression(
yyval.Node, Line_Number, False, 
yy.value_stack(yy.tos).Node);

when  109 =>
--#line  392
Put_Line("unimp 35");

when  110 =>
--#line  393
Put_Line("unimp 36");

when  111 =>
--#line  394
Put_Line("unimp 37");

when  112 =>
--#line  395
Put_Line("unimp 38");

when  113 =>
--#line  399
 
yyval.Node := null;

when  114 =>
--#line  401
 
yyval.Node := 
yy.value_stack(yy.tos).Node; 

when  115 =>
--#line  406
 
yyval.Node := 
yy.value_stack(yy.tos).Node; 

when  116 =>
--#line  408

         
yyval.Node := 
yy.value_stack(yy.tos-2).Node; -- Copy up
         
yy.value_stack(yy.tos-2).Node := null; -- Only keep one reference to this node.
         Add_Next(
yyval.Node, 
yy.value_stack(yy.tos).Node);
      

when  117 =>
--#line  416
Build_Op_Expression(
yyval.Node, Line_Number, Add, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  118 =>
--#line  417
Build_Op_Expression(
yyval.Node, Line_Number, Sub, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  119 =>
--#line  418
Build_Op_Expression(
yyval.Node, Line_Number, Mul, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  120 =>
--#line  419
Build_Op_Expression(
yyval.Node, Line_Number, Div, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  121 =>
--#line  420
Build_Op_Expression(
yyval.Node, Line_Number, Modulo, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  122 =>
--#line  421
Build_Op_Expression(
yyval.Node, Line_Number, Exp, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  123 =>
--#line  422
Build_Op_Expression(
yyval.Node, Line_Number, Eq, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  124 =>
--#line  423
Build_Op_Expression(
yyval.Node, Line_Number, Neq, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  125 =>
--#line  424
Build_Op_Expression(
yyval.Node, Line_Number, L_t, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  126 =>
--#line  425
Build_Op_Expression(
yyval.Node, Line_Number, Lte, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  127 =>
--#line  426
Build_Op_Expression(
yyval.Node, Line_Number, G_t, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  128 =>
--#line  427
Build_Op_Expression(
yyval.Node, Line_Number, Gte, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  129 =>
--#line  428
Build_Op_Expression(
yyval.Node, Line_Number, Shift_Left, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  130 =>
--#line  429
Build_Op_Expression(
yyval.Node, Line_Number, Shift_Right, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  131 =>
--#line  430
Build_Op_Expression(
yyval.Node, Line_Number, B_And, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  132 =>
--#line  431
Build_Op_Expression(
yyval.Node, Line_Number, B_Or , 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  133 =>
--#line  432
Build_Op_Expression(
yyval.Node, Line_Number, B_Xor, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  134 =>
--#line  433
 
yyval.Node := 
yy.value_stack(yy.tos-1).Node;

when  135 =>
--#line  434
Build_Op_Expression(
yyval.Node, Line_Number, Negate, 
yy.value_stack(yy.tos).Node);

when  136 =>
--#line  435
Build_Op_Expression(
yyval.Node, Line_Number, Op_Neg, 
yy.value_stack(yy.tos).Node);

when  137 =>
--#line  436
Build_Op_Expression(
yyval.Node, Line_Number, Op_Pos, 
yy.value_stack(yy.tos).Node);

when  138 =>
--#line  437
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  139 =>
--#line  438
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  140 =>
--#line  442
Build_Literal_Expression(
yyval.Node, Line_Number, Signed_Integer, yytext);

when  141 =>
--#line  443
Build_Literal_Expression(
yyval.Node, Line_Number, Floatingpoint, yytext);

when  142 =>
--#line  444
Build_Literal_Expression(
yyval.Node, Line_Number, Immutable_String, yytext);

when  143 =>
--#line  445
 
yyval.Node := 
yy.value_stack(yy.tos).Node;

when  144 =>
--#line  449
Build_Literal_Expression(
yyval.Node, Line_Number, Bit_Or_Boolean, yytext);

when  145 =>
--#line  450
Build_Literal_Expression(
yyval.Node, Line_Number, Bit_Or_Boolean, yytext);

when  146 =>
--#line  455
Build_Var_Def(
yyval.Node, Line_Number, 
yy.value_stack(yy.tos-2).Node, 
yy.value_stack(yy.tos).Node);

when  147 =>
--#line  459
Build_Emit(
yyval.Node, Line_Number, 
yy.value_stack(yy.tos).Node);

                    when others => null;
                end case;


            -- Pop RHS states and goto next state
            yy.tos      := yy.tos - rule_length(yy.rule_id) + 1;
            if yy.tos > yy.stack_size then
                text_io.put_line(" Stack size exceeded on state_stack");
                raise yy_Tokens.syntax_error;
            end if;
            yy.state_stack(yy.tos) := goto_state(yy.state_stack(yy.tos-1) ,
                                 get_lhs_rule(yy.rule_id));

              yy.value_stack(yy.tos) := yyval;

            if yy.debug then
                reduce_debug(yy.rule_id,
                    goto_state(yy.state_stack(yy.tos - 1),
                               get_lhs_rule(yy.rule_id)));
            end if;

        end if;


    end loop;


end yyparse;

end kv.avm.vole_parser;
