
with Ada.Text_IO;
use Ada.Text_IO;

package body kv.avm.Vole_Lex is

   procedure Report is
   begin
      Put_Line(yytext);
   end Report;

   procedure Inc_Line is
   begin
      Line_Number := Line_Number + 1;
   end Inc_Line;

function YYLex return Token is
subtype short is integer range -32768..32767;
    yy_act : integer;
    yy_c : short;

-- returned upon end-of-file
YY_END_TOK : constant integer := 0;
YY_END_OF_BUFFER : constant := 76;
subtype yy_state_type is integer;
yy_current_state : yy_state_type;
INITIAL : constant := 0;
yy_accept : constant array(0..244) of short :=
    (   0,
        0,    0,   76,   75,   74,   73,   48,   75,   72,   37,
       45,   68,   69,   35,   33,   64,   34,   63,   36,   60,
       62,   65,   43,   38,   40,   58,   58,   58,   58,   58,
       58,   58,   58,   70,   71,   58,   58,   58,   58,   58,
       58,   58,   58,   58,   58,   58,   58,   58,   58,   58,
       58,   66,   46,   67,   74,   39,    0,   61,    0,   72,
       32,    0,   60,    0,   44,   30,   42,   38,   41,   31,
       58,    0,   58,   58,   58,   58,   58,   58,   58,   58,
       58,   58,   58,   58,   58,   58,   58,   26,   58,   58,
       58,   58,   58,   12,   58,   58,   58,   58,   58,   58,

       46,   58,   58,   58,   58,   58,   58,   58,   58,   59,
       58,   58,   58,   58,   58,   58,   58,   58,   58,   58,
       45,   58,   58,   58,   58,   58,   58,   58,   58,   11,
       58,   58,   58,   58,   58,   37,   28,   48,   58,   58,
       58,   58,   58,   58,   58,   58,   58,   47,   59,    0,
        0,   58,   58,   58,   58,   58,   58,   56,   58,   58,
       58,   58,   58,    4,   58,    6,    8,   58,   58,   58,
       58,   27,   58,   58,   58,   58,   19,   20,   58,   22,
       58,   24,   58,    0,   59,   49,   58,   57,   53,   58,
       58,   55,   58,    1,   58,   58,   58,   58,    9,   58,

       58,   16,   58,   58,   58,   58,   21,   23,   25,   59,
        0,   58,   58,   54,   58,    2,   58,   58,    7,   58,
       13,   58,   18,   58,   14,   50,   51,   58,   58,   58,
       10,   17,   58,   15,   52,   58,   58,   58,    3,   58,
       29,   58,    5,    0
    ) ;

yy_ec : constant array(ASCII.NUL..Character'Last) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    2,    3,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    2,    4,    5,    6,    1,    7,    8,    1,    9,
       10,   11,   12,   13,   14,   15,   16,   17,   17,   17,
       17,   17,   17,   17,   17,   17,   17,   18,   19,   20,
       21,   22,    1,    1,   23,   24,   25,   25,   26,   27,
       25,   25,   28,   25,   25,   25,   25,   25,   25,   25,
       25,   25,   29,   30,   31,   25,   25,   25,   25,   25,
       32,   33,   34,    1,   35,    1,   36,   37,   38,   39,

       40,   41,   42,   43,   44,   25,   25,   45,   46,   47,
       48,   49,   25,   50,   51,   52,   53,   25,   54,   55,
       25,   25,   56,   57,   58,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,

        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1
    ) ;

yy_meta : constant array(0..58) of short :=
    (   0,
        1,    1,    2,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    3,    1,    1,    1,
        1,    1,    3,    3,    3,    3,    3,    3,    3,    3,
        3,    1,    1,    1,    3,    3,    3,    3,    3,    3,
        3,    3,    3,    3,    3,    3,    3,    3,    3,    3,
        3,    3,    3,    3,    3,    1,    1,    1
    ) ;

yy_base : constant array(0..247) of short :=
    (   0,
        0,    0,  459,  460,  456,  460,  436,   54,    0,  460,
      460,  460,  460,  445,  460,  460,  460,  460,  434,   45,
      433,  460,   43,  432,   44,   32,   26,  417,   33,   36,
       37,   38,   46,  460,  460,   47,   60,   42,   57,   49,
       65,   66,   70,   80,   72,   74,   81,   90,   84,   88,
       78,  460,  460,  460,  449,  460,  112,  460,  447,    0,
      460,  432,  117,  431,  460,  460,  460,  460,  460,  460,
      412,  411,   94,  100,   41,  101,   98,  103,  104,  105,
      107,  109,   40,  112,  116,  120,  125,  410,  124,  121,
      127,  129,  132,  409,  134,  138,  139,  145,  135,  142,

      408,  152,  143,  153,  144,  161,  150,  162,  168,  187,
      172,  170,  173,  174,  176,  177,  179,  188,  182,  190,
      407,  194,  193,  195,  196,  197,  201,  204,  205,  406,
      206,  214,  207,  209,  216,  405,  404,  403,  222,  211,
      217,  227,  228,  220,  230,  234,  235,  402,  256,  260,
      419,  236,  243,  244,  241,  250,  252,  400,  254,  253,
      255,  262,  263,  399,  265,  266,  398,  267,  268,  269,
      271,  397,  278,  274,  276,  283,  396,  395,  286,  394,
      288,  393,  289,  410,  308,  391,  291,  390,  389,  295,
      296,  388,  297,  387,  299,  302,  305,  306,  386,  307,

      310,  385,  314,  313,  315,  319,  384,  383,  382,  340,
      399,  323,  324,  380,  325,  379,  326,  328,  378,  329,
      377,  332,  375,  333,  336,  369,  368,  338,  341,  347,
      365,  364,  343,  363,  362,  348,  349,  351,  361,  354,
      359,  355,  357,  460,  405,  408,  138
    ) ;

yy_def : constant array(0..247) of short :=
    (   0,
      244,    1,  244,  244,  244,  244,  244,  245,  246,  244,
      244,  244,  244,  244,  244,  244,  244,  244,  244,  244,
      244,  244,  244,  244,  244,  247,  247,  247,  247,  247,
      247,  247,  247,  244,  244,  247,  247,  247,  247,  247,
      247,  247,  247,  247,  247,  247,  247,  247,  247,  247,
      247,  244,  244,  244,  244,  244,  245,  244,  245,  246,
      244,  244,  244,  244,  244,  244,  244,  244,  244,  244,
      247,  247,  247,  247,  247,  247,  247,  247,  247,  247,
      247,  247,  247,  247,  247,  247,  247,  247,  247,  247,
      247,  247,  247,  247,  247,  247,  247,  247,  247,  247,

      247,  247,  247,  247,  247,  247,  247,  247,  247,  244,
      247,  247,  247,  247,  247,  247,  247,  247,  247,  247,
      247,  247,  247,  247,  247,  247,  247,  247,  247,  247,
      247,  247,  247,  247,  247,  247,  247,  247,  247,  247,
      247,  247,  247,  247,  247,  247,  247,  247,  244,  244,
      244,  247,  247,  247,  247,  247,  247,  247,  247,  247,
      247,  247,  247,  247,  247,  247,  247,  247,  247,  247,
      247,  247,  247,  247,  247,  247,  247,  247,  247,  247,
      247,  247,  247,  244,  244,  247,  247,  247,  247,  247,
      247,  247,  247,  247,  247,  247,  247,  247,  247,  247,

      247,  247,  247,  247,  247,  247,  247,  247,  247,  244,
      244,  247,  247,  247,  247,  247,  247,  247,  247,  247,
      247,  247,  247,  247,  247,  247,  247,  247,  247,  247,
      247,  247,  247,  247,  247,  247,  247,  247,  247,  247,
      247,  247,  247,    0,  244,  244,  244
    ) ;

yy_nxt : constant array(0..518) of short :=
    (   0,
        4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
       14,   15,   16,   17,   18,   19,   20,   21,   22,   23,
       24,   25,   26,   27,   28,   28,   29,   30,   31,   32,
       33,   34,    4,   35,    4,   36,   28,   37,   38,   39,
       40,   28,   28,   41,   42,   43,   44,   45,   46,   47,
       48,   49,   28,   50,   51,   52,   53,   54,   58,   62,
       72,   63,   66,   67,   69,   70,   72,   72,   75,   73,
       72,   72,   72,   74,   72,   72,   72,   76,  121,   64,
       72,   72,   77,   72,   82,  113,   59,   79,   78,   88,
       80,   72,   81,   83,   72,   86,   93,   84,   85,   72,

       72,   89,   90,   91,   72,   94,   72,   87,   72,   97,
       95,   92,   72,   96,   72,   72,   58,   98,   72,   99,
      103,  101,   72,  102,   72,  109,  106,  100,   72,  104,
      108,   62,   72,   63,   72,   72,  107,   72,   72,   72,
       71,   72,  105,   72,   59,  111,   72,  112,  114,  115,
       72,   64,  116,  118,   72,   72,  117,  119,   72,   72,
      120,   72,  122,   72,  127,  128,   72,  123,   72,   72,
      124,  125,   72,   72,  126,  132,   72,   72,   72,   72,
      129,  130,  131,  136,   72,  133,   72,   72,  137,  134,
      135,  139,  143,  138,  140,   72,   72,  141,  145,  142,

      144,  146,   72,  149,   72,  147,   72,   72,   72,  155,
       72,   72,  150,   72,  153,  156,   72,  148,  158,  152,
      157,  151,   72,  154,   72,  160,  150,   72,   72,   72,
       72,   72,  159,  162,  164,   72,  166,  161,   72,   72,
       72,   72,  163,   72,  169,   72,  165,  168,   72,  171,
       72,   72,  167,  170,   72,  172,   72,  177,  174,  173,
      175,   72,   72,  176,   72,  178,  180,  179,   72,   72,
       72,  184,  149,  184,  181,   72,  185,   72,   72,  183,
      182,  150,  187,  188,   72,  186,   72,   72,   72,   72,
      151,  190,  189,  192,  193,  150,   72,   72,  191,   72,

       72,   72,   72,   72,  194,   72,  196,  199,   72,  198,
       72,  195,   72,  203,  200,  202,  197,   72,  201,  205,
       72,  204,   72,   72,  210,   72,  212,  208,  209,   72,
       72,   72,  206,   72,  213,  207,   72,  214,  217,   72,
       72,   72,  211,  215,   72,  220,  219,   72,   72,   72,
      216,  223,  224,   72,  218,  222,  210,   72,   72,   72,
       72,  221,   72,   72,  228,  225,   72,   72,  233,  226,
       72,  232,   72,  227,  211,   72,  235,   72,  229,  231,
      230,   72,   72,   72,  237,   72,  234,  239,   72,   72,
      241,   72,  236,   72,  238,   72,   72,   72,   72,   72,

      240,  242,   72,   72,  243,   57,   57,   57,   60,   72,
       60,   72,   72,   72,   72,  210,   72,   72,   72,   72,
       72,   72,   72,   72,   72,   72,  185,   72,   72,   72,
       72,   72,   72,   72,   72,  149,   72,   72,   72,   72,
       72,   72,   72,   72,   72,  244,   72,   63,  110,  244,
       55,   72,   68,   65,   56,   61,   56,   55,  244,    3,
      244,  244,  244,  244,  244,  244,  244,  244,  244,  244,
      244,  244,  244,  244,  244,  244,  244,  244,  244,  244,
      244,  244,  244,  244,  244,  244,  244,  244,  244,  244,
      244,  244,  244,  244,  244,  244,  244,  244,  244,  244,

      244,  244,  244,  244,  244,  244,  244,  244,  244,  244,
      244,  244,  244,  244,  244,  244,  244,  244
    ) ;

yy_chk : constant array(0..518) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    8,   20,
       27,   20,   23,   23,   25,   25,   26,   29,   29,   26,
       30,   31,   32,   27,   83,   75,   38,   29,   83,   20,
       33,   36,   30,   40,   36,   75,    8,   32,   31,   38,
       32,   39,   33,   36,   37,   37,   40,   36,   36,   41,

       42,   39,   39,   39,   43,   41,   45,   37,   46,   43,
       41,   39,   51,   42,   44,   47,   57,   43,   49,   44,
       47,   45,   50,   46,   48,   51,   49,   44,   73,   48,
       50,   63,   77,   63,   74,   76,   49,   78,   79,   80,
      247,   81,   48,   82,   57,   73,   84,   74,   76,   77,
       85,   63,   78,   80,   86,   90,   79,   81,   89,   87,
       82,   91,   84,   92,   90,   91,   93,   85,   95,   99,
       86,   87,   96,   97,   89,   96,  100,  103,  105,   98,
       92,   93,   95,   98,  107,   96,  102,  104,   99,   97,
       97,  102,  105,  100,  103,  106,  108,  104,  107,  104,

      106,  108,  109,  110,  112,  108,  111,  113,  114,  114,
      115,  116,  110,  117,  112,  115,  119,  109,  117,  111,
      116,  110,  118,  113,  120,  119,  110,  123,  122,  124,
      125,  126,  118,  122,  124,  127,  126,  120,  128,  129,
      131,  133,  123,  134,  129,  140,  125,  128,  132,  132,
      135,  141,  127,  131,  144,  133,  139,  141,  135,  134,
      139,  142,  143,  140,  145,  142,  144,  143,  146,  147,
      152,  150,  149,  150,  145,  155,  150,  153,  154,  147,
      146,  149,  153,  154,  156,  152,  157,  160,  159,  161,
      149,  156,  155,  159,  160,  149,  162,  163,  157,  165,

      166,  168,  169,  170,  161,  171,  163,  168,  174,  166,
      175,  162,  173,  173,  169,  171,  165,  176,  170,  175,
      179,  174,  181,  183,  185,  187,  187,  181,  183,  190,
      191,  193,  176,  195,  190,  179,  196,  191,  196,  197,
      198,  200,  185,  193,  201,  200,  198,  204,  203,  205,
      195,  204,  205,  206,  197,  203,  210,  212,  213,  215,
      217,  201,  218,  220,  215,  206,  222,  224,  224,  212,
      225,  222,  228,  213,  210,  229,  228,  233,  217,  220,
      218,  230,  236,  237,  230,  238,  225,  236,  240,  242,
      238,  243,  229,  241,  233,  239,  235,  234,  232,  231,

      237,  240,  227,  226,  242,  245,  245,  245,  246,  223,
      246,  221,  219,  216,  214,  211,  209,  208,  207,  202,
      199,  194,  192,  189,  188,  186,  184,  182,  180,  178,
      177,  172,  167,  164,  158,  151,  148,  138,  137,  136,
      130,  121,  101,   94,   88,   72,   71,   64,   62,   59,
       55,   28,   24,   21,   19,   14,    7,    5,    3,  244,
      244,  244,  244,  244,  244,  244,  244,  244,  244,  244,
      244,  244,  244,  244,  244,  244,  244,  244,  244,  244,
      244,  244,  244,  244,  244,  244,  244,  244,  244,  244,
      244,  244,  244,  244,  244,  244,  244,  244,  244,  244,

      244,  244,  244,  244,  244,  244,  244,  244,  244,  244,
      244,  244,  244,  244,  244,  244,  244,  244
    ) ;


-- copy whatever the last rule matched to the standard output

procedure ECHO is
begin
   if (text_io.is_open(user_output_file)) then
     text_io.put( user_output_file, yytext );
   else
     text_io.put( yytext );
   end if;
end ECHO;

-- enter a start condition.
-- Using procedure requires a () after the ENTER, but makes everything
-- much neater.

procedure ENTER( state : integer ) is
begin
     yy_start := 1 + 2 * state;
end ENTER;

-- action number for EOF rule of a given start state
function YY_STATE_EOF(state : integer) return integer is
begin
     return YY_END_OF_BUFFER + state + 1;
end YY_STATE_EOF;

-- return all but the first 'n' matched characters back to the input stream
procedure yyless(n : integer) is
begin
        yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
        yy_cp := yy_bp + n;
        yy_c_buf_p := yy_cp;
        YY_DO_BEFORE_ACTION; -- set up yytext again
end yyless;

-- redefine this if you have something you want each time.
procedure YY_USER_ACTION is
begin
        null;
end;

-- yy_get_previous_state - get the state just before the EOB char was reached

function yy_get_previous_state return yy_state_type is
    yy_current_state : yy_state_type;
    yy_c : short;
begin
    yy_current_state := yy_start;

    for yy_cp in yytext_ptr..yy_c_buf_p - 1 loop
	yy_c := yy_ec(yy_ch_buf(yy_cp));
	if ( yy_accept(yy_current_state) /= 0 ) then
	    yy_last_accepting_state := yy_current_state;
	    yy_last_accepting_cpos := yy_cp;
	end if;
	while ( yy_chk(yy_base(yy_current_state) + yy_c) /= yy_current_state ) loop
	    yy_current_state := yy_def(yy_current_state);
	    if ( yy_current_state >= 245 ) then
		yy_c := yy_meta(yy_c);
	    end if;
	end loop;
	yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
    end loop;

    return yy_current_state;
end yy_get_previous_state;

procedure yyrestart( input_file : file_type ) is
begin
   open_input(text_io.name(input_file));
end yyrestart;

begin -- of YYLex
<<new_file>>
        -- this is where we enter upon encountering an end-of-file and
        -- yywrap() indicating that we should continue processing

    if ( yy_init ) then
        if ( yy_start = 0 ) then
            yy_start := 1;      -- first start state
        end if;

        -- we put in the '\n' and start reading from [1] so that an
        -- initial match-at-newline will be true.

        yy_ch_buf(0) := ASCII.LF;
        yy_n_chars := 1;

        -- we always need two end-of-buffer characters.  The first causes
        -- a transition to the end-of-buffer state.  The second causes
        -- a jam in that state.

        yy_ch_buf(yy_n_chars) := YY_END_OF_BUFFER_CHAR;
        yy_ch_buf(yy_n_chars + 1) := YY_END_OF_BUFFER_CHAR;

        yy_eof_has_been_seen := false;

        yytext_ptr := 1;
        yy_c_buf_p := yytext_ptr;
        yy_hold_char := yy_ch_buf(yy_c_buf_p);
        yy_init := false;
    end if; -- yy_init

    loop                -- loops until end-of-file is reached


        yy_cp := yy_c_buf_p;

        -- support of yytext
        yy_ch_buf(yy_cp) := yy_hold_char;

        -- yy_bp points to the position in yy_ch_buf of the start of the
        -- current run.
	yy_bp := yy_cp;
	yy_current_state := yy_start;
	loop
		yy_c := yy_ec(yy_ch_buf(yy_cp));
		if ( yy_accept(yy_current_state) /= 0 ) then
		    yy_last_accepting_state := yy_current_state;
		    yy_last_accepting_cpos := yy_cp;
		end if;
		while ( yy_chk(yy_base(yy_current_state) + yy_c) /= yy_current_state ) loop
		    yy_current_state := yy_def(yy_current_state);
		    if ( yy_current_state >= 245 ) then
			yy_c := yy_meta(yy_c);
		    end if;
		end loop;
		yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
	    yy_cp := yy_cp + 1;
if ( yy_current_state = 244 ) then
    exit;
end if;
	end loop;
	yy_cp := yy_last_accepting_cpos;
	yy_current_state := yy_last_accepting_state;

<<next_action>>
	    yy_act := yy_accept(yy_current_state);
            YY_DO_BEFORE_ACTION;
            YY_USER_ACTION;

        if aflex_debug then  -- output acceptance info. for (-d) debug mode
            text_io.put( Standard_Error, "--accepting rule #" );
            text_io.put( Standard_Error, INTEGER'IMAGE(yy_act) );
            text_io.put_line( Standard_Error, "(""" & yytext & """)");
        end if;


<<do_action>>   -- this label is used only to access EOF actions
            case yy_act is
		when 0 => -- must backtrack
		-- undo the effects of YY_DO_BEFORE_ACTION
		yy_ch_buf(yy_cp) := yy_hold_char;
		yy_cp := yy_last_accepting_cpos;
		yy_current_state := yy_last_accepting_state;
		goto next_action;



when 1 => 
--# line 11 "vole_lex.l"
 return Key_Actor; 

when 2 => 
--# line 12 "vole_lex.l"
 return Key_Assert; 

when 3 => 
--# line 13 "vole_lex.l"
 return Key_Attribute; 

when 4 => 
--# line 14 "vole_lex.l"
 return Key_Case; 

when 5 => 
--# line 15 "vole_lex.l"
 return Key_Constructor; 

when 6 => 
--# line 16 "vole_lex.l"
 return Key_Else; 

when 7 => 
--# line 17 "vole_lex.l"
 return Key_Elseif; 

when 8 => 
--# line 18 "vole_lex.l"
 return Key_Emit; 

when 9 => 
--# line 19 "vole_lex.l"
 return Key_Endif; 

when 10 => 
--# line 20 "vole_lex.l"
 return Key_Extends; 

when 11 => 
--# line 21 "vole_lex.l"
 return Key_For; 

when 12 => 
--# line 22 "vole_lex.l"
 return Key_If; 

when 13 => 
--# line 23 "vole_lex.l"
 return Key_Import; 

when 14 => 
--# line 24 "vole_lex.l"
 return Key_Return; 

when 15 => 
--# line 25 "vole_lex.l"
 return Key_Returns; 

when 16 => 
--# line 26 "vole_lex.l"
 return Key_Local; 

when 17 => 
--# line 27 "vole_lex.l"
 return Key_Message; 

when 18 => 
--# line 28 "vole_lex.l"
 return Key_Method; 

when 19 => 
--# line 29 "vole_lex.l"
 return Key_Self; 

when 20 => 
--# line 30 "vole_lex.l"
 return Key_Send; 

when 21 => 
--# line 31 "vole_lex.l"
 return Key_Super; 

when 22 => 
--# line 32 "vole_lex.l"
 return Key_Then; 

when 23 => 
--# line 33 "vole_lex.l"
 return Key_Tuple; 

when 24 => 
--# line 34 "vole_lex.l"
 return Key_When; 

when 25 => 
--# line 35 "vole_lex.l"
 return Key_While; 

when 26 => 
--# line 36 "vole_lex.l"
 return Key_Do; 

when 27 => 
--# line 37 "vole_lex.l"
 return Key_Loop; 

when 28 => 
--# line 38 "vole_lex.l"
 return Key_New; 

when 29 => 
--# line 39 "vole_lex.l"
 return Key_Predicate; 

when 30 => 
--# line 41 "vole_lex.l"
 return Op_Shift_Left; 

when 31 => 
--# line 42 "vole_lex.l"
 return Op_Shift_Right; 

when 32 => 
--# line 43 "vole_lex.l"
 return Op_Exp; 

when 33 => 
--# line 44 "vole_lex.l"
 return Op_Add; 

when 34 => 
--# line 45 "vole_lex.l"
 return Op_Sub; 

when 35 => 
--# line 46 "vole_lex.l"
 return Op_Mul; 

when 36 => 
--# line 47 "vole_lex.l"
 return Op_Div; 

when 37 => 
--# line 48 "vole_lex.l"
 return Op_Mod; 

when 38 => 
--# line 49 "vole_lex.l"
 return Op_Eq; 

when 39 => 
--# line 50 "vole_lex.l"
 return Op_Not_Eq; 

when 40 => 
--# line 51 "vole_lex.l"
 return Op_Gt; 

when 41 => 
--# line 52 "vole_lex.l"
 return Op_Gt_Eq; 

when 42 => 
--# line 53 "vole_lex.l"
 return Op_Lt_Eq; 

when 43 => 
--# line 54 "vole_lex.l"
 return Op_Lt; 

when 44 => 
--# line 55 "vole_lex.l"
 return Op_Assign; 

when 45 => 
--# line 56 "vole_lex.l"
 return Op_And; 

when 46 => 
--# line 57 "vole_lex.l"
 return Op_Or; 

when 47 => 
--# line 58 "vole_lex.l"
 return Op_Xor; 

when 48 => 
--# line 59 "vole_lex.l"
 return Op_Not; 

when 49 => 
--# line 61 "vole_lex.l"
 return Actor_Type; 

when 50 => 
--# line 62 "vole_lex.l"
 return Boolean_Type; 

when 51 => 
--# line 63 "vole_lex.l"
 return Integer_Type; 

when 52 => 
--# line 64 "vole_lex.l"
 return Unsigned_Type; 

when 53 => 
--# line 65 "vole_lex.l"
 return Float_Type; 

when 54 => 
--# line 66 "vole_lex.l"
 return String_Type; 

when 55 => 
--# line 67 "vole_lex.l"
 return Tuple_Type; 

when 56 => 
--# line 69 "vole_lex.l"
 return True_Literal; 

when 57 => 
--# line 70 "vole_lex.l"
 return False_Literal; 

when 58 => 
--# line 72 "vole_lex.l"
 return Id_Token; 

when 59 => 
--# line 74 "vole_lex.l"
 return Float_Literal; 

when 60 => 
--# line 75 "vole_lex.l"
 return Integer_Literal; 

when 61 => 
--# line 76 "vole_lex.l"
 return String_Literal; 

when 62 => 
--# line 78 "vole_lex.l"
 return Colon_Token; 

when 63 => 
--# line 79 "vole_lex.l"
 return Dot_Token; 

when 64 => 
--# line 80 "vole_lex.l"
 return Comma_Token; 

when 65 => 
--# line 81 "vole_lex.l"
 return Eos_Token; 

when 66 => 
--# line 82 "vole_lex.l"
 return Block_Begin; 

when 67 => 
--# line 83 "vole_lex.l"
 return Block_End; 

when 68 => 
--# line 84 "vole_lex.l"
 return Paren_Begin; 

when 69 => 
--# line 85 "vole_lex.l"
 return Paren_End; 

when 70 => 
--# line 86 "vole_lex.l"
 return Tuple_Begin; 

when 71 => 
--# line 87 "vole_lex.l"
 return Tuple_End; 

when 72 => 
--# line 89 "vole_lex.l"
 null; 

when 73 => 
--# line 90 "vole_lex.l"
 Inc_Line; 

when 74 => 
--# line 91 "vole_lex.l"
 null; 

when 75 => 
--# line 93 "vole_lex.l"
ECHO;
when YY_END_OF_BUFFER + INITIAL + 1 => 
    return End_Of_Input;
                when YY_END_OF_BUFFER =>
                    -- undo the effects of YY_DO_BEFORE_ACTION
                    yy_ch_buf(yy_cp) := yy_hold_char;

                    yytext_ptr := yy_bp;

                    case yy_get_next_buffer is
                        when EOB_ACT_END_OF_FILE =>
                            begin
                            if ( yywrap ) then
                                -- note: because we've taken care in
                                -- yy_get_next_buffer() to have set up yytext,
                                -- we can now set up yy_c_buf_p so that if some
                                -- total hoser (like aflex itself) wants
                                -- to call the scanner after we return the
                                -- End_Of_Input, it'll still work - another
                                -- End_Of_Input will get returned.

                                yy_c_buf_p := yytext_ptr;

                                yy_act := YY_STATE_EOF((yy_start - 1) / 2);

                                goto do_action;
                            else
                                --  start processing a new file
                                yy_init := true;
                                goto new_file;
                            end if;
                            end;
                        when EOB_ACT_RESTART_SCAN =>
                            yy_c_buf_p := yytext_ptr;
                            yy_hold_char := yy_ch_buf(yy_c_buf_p);
                        when EOB_ACT_LAST_MATCH =>
                            yy_c_buf_p := yy_n_chars;
                            yy_current_state := yy_get_previous_state;

                            yy_cp := yy_c_buf_p;
                            yy_bp := yytext_ptr;
                            goto next_action;
                        when others => null;
                        end case; -- case yy_get_next_buffer()
                when others =>
                    text_io.put( "action # " );
                    text_io.put( INTEGER'IMAGE(yy_act) );
                    text_io.new_line;
                    raise AFLEX_INTERNAL_ERROR;
            end case; -- case (yy_act)
        end loop; -- end of loop waiting for end of file
end YYLex;
--# line 93 "vole_lex.l"

end kv.avm.Vole_Lex;

