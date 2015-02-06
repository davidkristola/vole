
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
YY_END_OF_BUFFER : constant := 75;
subtype yy_state_type is integer;
yy_current_state : yy_state_type;
INITIAL : constant := 0;
yy_accept : constant array(0..242) of short :=
    (   0,
        0,    0,   75,   74,   73,   72,   47,   74,   71,   36,
       44,   67,   68,   34,   32,   63,   33,   62,   35,   59,
       61,   64,   42,   37,   39,   57,   57,   57,   57,   57,
       57,   57,   57,   69,   70,   57,   57,   57,   57,   57,
       57,   57,   57,   57,   57,   57,   57,   57,   57,   57,
       65,   45,   66,   73,   38,    0,   60,    0,   71,   31,
        0,   59,    0,   43,   29,   41,   37,   40,   30,   57,
        0,   57,   57,   57,   57,   57,   57,   57,   57,   57,
       57,   57,   57,   57,   57,   57,   57,   57,   57,   57,
       57,   12,   57,   57,   57,   57,   57,   57,   45,   57,

       57,   57,   57,   57,   57,   57,   57,   58,   57,   57,
       57,   57,   57,   57,   57,   57,   57,   57,   44,   57,
       57,   57,   57,   57,   57,   57,   57,   11,   57,   57,
       57,   57,   57,   36,   27,   47,   57,   57,   57,   57,
       57,   57,   57,   57,   57,   46,   58,    0,    0,   57,
       57,   57,   57,   57,   57,   55,   57,   57,   57,   57,
       57,    4,   57,    6,    8,   57,   57,   57,   57,   26,
       57,   57,   57,   57,   19,   20,   57,   22,   57,   24,
       57,    0,   58,   48,   57,   56,   52,   57,   57,   54,
       57,    1,   57,   57,   57,   57,    9,   57,   57,   16,

       57,   57,   57,   57,   21,   23,   25,   58,    0,   57,
       57,   53,   57,    2,   57,   57,    7,   57,   13,   57,
       18,   57,   14,   49,   50,   57,   57,   57,   10,   17,
       57,   15,   51,   57,   57,   57,    3,   57,   28,   57,
        5,    0
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

yy_base : constant array(0..245) of short :=
    (   0,
        0,    0,  456,  457,  453,  457,  433,   54,    0,  457,
      457,  457,  457,  442,  457,  457,  457,  457,  431,   45,
      430,  457,   43,  429,   44,   32,   26,  414,   33,   36,
       37,   38,   46,  457,  457,   47,   60,   55,   44,   68,
       49,   71,   72,   78,   80,   81,   82,   83,   88,   89,
      457,  457,  457,  446,  457,   71,  457,  444,    0,  457,
      429,  110,  428,  457,  457,  457,  457,  457,  457,  409,
      408,   94,   99,   97,  103,   98,  104,  105,  106,  108,
      109,  113,  114,  118,  121,  122,  125,   42,  127,  128,
      129,  407,  132,  136,  138,  143,  133,  140,  406,  148,

      142,  150,  151,  156,  158,  164,  163,  184,  167,  171,
      170,  176,  174,  182,  183,  185,  187,  190,  405,  192,
      193,  194,  198,  200,  201,  202,  204,  404,  206,  212,
      207,  210,  215,  403,  402,  401,  216,  217,  222,  225,
      227,  224,  230,  231,  234,  400,  251,  268,  417,  237,
      241,  248,  238,  254,  257,  398,  258,  259,  249,  260,
      262,  397,  265,  267,  396,  268,  272,  270,  273,  395,
      277,  279,  280,  281,  394,  393,  286,  392,  288,  391,
      290,  408,  305,  389,  297,  388,  387,  294,  300,  386,
      291,  385,  302,  304,  308,  309,  384,  310,  311,  383,

      313,  312,  318,  317,  382,  381,  380,  340,  397,  324,
      326,  378,  325,  376,  327,  331,  370,  332,  369,  333,
      366,  334,  337,  365,  364,  339,  342,  344,  362,  361,
      346,  360,  358,  350,  351,  352,  239,  354,  168,  356,
       70,  457,  406,  409,   72
    ) ;

yy_def : constant array(0..245) of short :=
    (   0,
      242,    1,  242,  242,  242,  242,  242,  243,  244,  242,
      242,  242,  242,  242,  242,  242,  242,  242,  242,  242,
      242,  242,  242,  242,  242,  245,  245,  245,  245,  245,
      245,  245,  245,  242,  242,  245,  245,  245,  245,  245,
      245,  245,  245,  245,  245,  245,  245,  245,  245,  245,
      242,  242,  242,  242,  242,  243,  242,  243,  244,  242,
      242,  242,  242,  242,  242,  242,  242,  242,  242,  245,
      245,  245,  245,  245,  245,  245,  245,  245,  245,  245,
      245,  245,  245,  245,  245,  245,  245,  245,  245,  245,
      245,  245,  245,  245,  245,  245,  245,  245,  245,  245,

      245,  245,  245,  245,  245,  245,  245,  242,  245,  245,
      245,  245,  245,  245,  245,  245,  245,  245,  245,  245,
      245,  245,  245,  245,  245,  245,  245,  245,  245,  245,
      245,  245,  245,  245,  245,  245,  245,  245,  245,  245,
      245,  245,  245,  245,  245,  245,  242,  242,  242,  245,
      245,  245,  245,  245,  245,  245,  245,  245,  245,  245,
      245,  245,  245,  245,  245,  245,  245,  245,  245,  245,
      245,  245,  245,  245,  245,  245,  245,  245,  245,  245,
      245,  242,  242,  245,  245,  245,  245,  245,  245,  245,
      245,  245,  245,  245,  245,  245,  245,  245,  245,  245,

      245,  245,  245,  245,  245,  245,  245,  242,  242,  245,
      245,  245,  245,  245,  245,  245,  245,  245,  245,  245,
      245,  245,  245,  245,  245,  245,  245,  245,  245,  245,
      245,  245,  245,  245,  245,  245,  245,  245,  245,  245,
      245,    0,  242,  242,  242
    ) ;

yy_nxt : constant array(0..515) of short :=
    (   0,
        4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
       14,   15,   16,   17,   18,   19,   20,   21,   22,   23,
       24,   25,   26,   27,   28,   28,   29,   30,   31,   32,
       33,   34,    4,   35,    4,   36,   28,   37,   28,   38,
       39,   28,   28,   40,   41,   42,   43,   44,   45,   46,
       47,   48,   28,   49,   50,   51,   52,   53,   57,   61,
       71,   62,   65,   66,   68,   69,   71,   71,   74,   72,
       71,   71,   71,   73,   70,   57,   71,   75,   71,   63,
       71,   71,   76,   71,   81,  125,   58,   78,   77,   71,
       79,   91,   80,   82,   71,   85,   94,   83,   84,   87,

       88,   89,   71,   58,   71,   71,   71,   86,   92,   90,
       95,   97,   71,   93,   71,   71,   71,   71,   96,   98,
      101,  102,   71,   71,   61,  104,   62,   99,   71,  100,
      106,   71,   71,   71,  103,  105,  107,   71,   71,   71,
       71,  111,   71,   71,   63,  109,  110,   71,   71,  113,
      112,  119,   71,  114,  116,   71,   71,  115,  117,   71,
      118,   71,   71,   71,  120,  126,   71,   71,  123,  121,
       71,  122,   71,  130,   71,  124,   71,   71,  128,  127,
      129,  134,   71,  131,   71,   71,  135,  137,  132,  133,
       71,  136,   71,  138,  139,  142,  140,   71,   71,  141,

      147,   71,   71,  144,   71,   71,  143,  145,   71,  148,
       71,  153,  146,  154,  150,  151,   71,   71,  149,   71,
      152,   71,  156,  148,   71,  155,   71,   71,   71,  157,
      158,  160,   71,  162,   71,   71,   71,  159,   71,  164,
       71,   71,  161,  167,   71,  166,   71,  169,  163,   71,
       71,   71,  165,  168,  173,  170,   71,  172,   71,   71,
      171,   71,  175,  176,   71,   71,  177,  147,   71,  174,
      178,   71,   71,   71,  179,   71,  148,  180,  181,  182,
      185,  182,   71,   71,  183,  149,  184,  186,   71,  187,
      148,   71,   71,   71,   71,  188,   71,  190,  192,   71,

      191,   71,   71,  189,   71,  194,   71,   71,  197,  193,
      196,   71,  201,   71,   71,   71,  195,  200,  198,  199,
       71,  208,   71,  203,   71,   71,  202,  206,   71,  207,
      204,   71,  210,  211,   71,  205,   71,  213,   71,  209,
      215,  212,   71,   71,   71,   71,   71,   71,  218,  217,
      221,   71,   71,  214,  220,  222,  208,  216,   71,   71,
       71,   71,  219,  223,  226,   71,   71,   71,   71,  231,
      224,   71,  230,   71,  209,  225,   71,  233,   71,  227,
       71,  235,  229,  228,   71,   71,   71,  232,   71,  237,
       71,  239,   71,  234,   71,   71,   71,  236,   71,   71,

       71,  240,  238,   71,   71,  241,   56,   56,   56,   59,
       71,   59,   71,  208,   71,   71,   71,   71,   71,   71,
       71,   71,   71,   71,  183,   71,   71,   71,   71,   71,
       71,   71,   71,  147,   71,   71,   71,   71,   71,   71,
       71,   71,  242,   71,   62,  108,  242,   54,   71,   67,
       64,   55,   60,   55,   54,  242,    3,  242,  242,  242,
      242,  242,  242,  242,  242,  242,  242,  242,  242,  242,
      242,  242,  242,  242,  242,  242,  242,  242,  242,  242,
      242,  242,  242,  242,  242,  242,  242,  242,  242,  242,
      242,  242,  242,  242,  242,  242,  242,  242,  242,  242,

      242,  242,  242,  242,  242,  242,  242,  242,  242,  242,
      242,  242,  242,  242,  242
    ) ;

yy_chk : constant array(0..515) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    8,   20,
       27,   20,   23,   23,   25,   25,   26,   29,   29,   26,
       30,   31,   32,   27,  245,   56,   88,   29,   39,   20,
       33,   36,   30,   41,   36,   88,    8,   32,   31,   38,
       32,   39,   33,   36,   37,   37,   41,   36,   36,   38,

       38,   38,   40,   56,  241,   42,   43,   37,   40,   38,
       42,   43,   44,   40,   45,   46,   47,   48,   42,   43,
       46,   47,   49,   50,   62,   48,   62,   44,   72,   45,
       49,   74,   76,   73,   47,   48,   50,   75,   77,   78,
       79,   74,   80,   81,   62,   72,   73,   82,   83,   76,
       75,   82,   84,   77,   79,   85,   86,   78,   80,   87,
       81,   89,   90,   91,   83,   89,   93,   97,   86,   84,
       94,   85,   95,   94,   98,   87,  101,   96,   91,   90,
       93,   96,  100,   94,  102,  103,   97,  100,   95,   95,
      104,   98,  105,  101,  102,  104,  102,  107,  106,  103,

      108,  109,  239,  106,  111,  110,  105,  106,  113,  108,
      112,  112,  107,  113,  109,  110,  114,  115,  108,  116,
      111,  117,  115,  108,  118,  114,  120,  121,  122,  116,
      117,  120,  123,  122,  124,  125,  126,  118,  127,  124,
      129,  131,  121,  127,  132,  126,  130,  130,  123,  133,
      137,  138,  125,  129,  137,  131,  139,  133,  142,  140,
      132,  141,  139,  140,  143,  144,  141,  147,  145,  138,
      142,  150,  153,  237,  143,  151,  147,  144,  145,  148,
      151,  148,  152,  159,  148,  147,  150,  152,  154,  153,
      147,  155,  157,  158,  160,  154,  161,  157,  159,  163,

      158,  164,  166,  155,  168,  161,  167,  169,  166,  160,
      164,  171,  171,  172,  173,  174,  163,  169,  167,  168,
      177,  183,  179,  173,  181,  191,  172,  179,  188,  181,
      174,  185,  185,  188,  189,  177,  193,  191,  194,  183,
      194,  189,  195,  196,  198,  199,  202,  201,  198,  196,
      202,  204,  203,  193,  201,  203,  208,  195,  210,  213,
      211,  215,  199,  204,  213,  216,  218,  220,  222,  222,
      210,  223,  220,  226,  208,  211,  227,  226,  228,  215,
      231,  228,  218,  216,  234,  235,  236,  223,  238,  234,
      240,  236,  233,  227,  232,  230,  229,  231,  225,  224,

      221,  238,  235,  219,  217,  240,  243,  243,  243,  244,
      214,  244,  212,  209,  207,  206,  205,  200,  197,  192,
      190,  187,  186,  184,  182,  180,  178,  176,  175,  170,
      165,  162,  156,  149,  146,  136,  135,  134,  128,  119,
       99,   92,   71,   70,   63,   61,   58,   54,   28,   24,
       21,   19,   14,    7,    5,    3,  242,  242,  242,  242,
      242,  242,  242,  242,  242,  242,  242,  242,  242,  242,
      242,  242,  242,  242,  242,  242,  242,  242,  242,  242,
      242,  242,  242,  242,  242,  242,  242,  242,  242,  242,
      242,  242,  242,  242,  242,  242,  242,  242,  242,  242,

      242,  242,  242,  242,  242,  242,  242,  242,  242,  242,
      242,  242,  242,  242,  242
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
	    if ( yy_current_state >= 243 ) then
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
		    if ( yy_current_state >= 243 ) then
			yy_c := yy_meta(yy_c);
		    end if;
		end loop;
		yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
	    yy_cp := yy_cp + 1;
if ( yy_current_state = 242 ) then
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
--# line 14 "vole_lex.l"
 return Key_Actor; 

when 2 => 
--# line 15 "vole_lex.l"
 return Key_Assert; 

when 3 => 
--# line 16 "vole_lex.l"
 return Key_Attribute; 

when 4 => 
--# line 17 "vole_lex.l"
 return Key_Case; 

when 5 => 
--# line 18 "vole_lex.l"
 return Key_Constructor; 

when 6 => 
--# line 19 "vole_lex.l"
 return Key_Else; 

when 7 => 
--# line 20 "vole_lex.l"
 return Key_Elseif; 

when 8 => 
--# line 21 "vole_lex.l"
 return Key_Emit; 

when 9 => 
--# line 22 "vole_lex.l"
 return Key_Endif; 

when 10 => 
--# line 23 "vole_lex.l"
 return Key_Extends; 

when 11 => 
--# line 24 "vole_lex.l"
 return Key_For; 

when 12 => 
--# line 25 "vole_lex.l"
 return Key_If; 

when 13 => 
--# line 26 "vole_lex.l"
 return Key_Import; 

when 14 => 
--# line 27 "vole_lex.l"
 return Key_Return; 

when 15 => 
--# line 28 "vole_lex.l"
 return Key_Returns; 

when 16 => 
--# line 29 "vole_lex.l"
 return Key_Local; 

when 17 => 
--# line 30 "vole_lex.l"
 return Key_Message; 

when 18 => 
--# line 31 "vole_lex.l"
 return Key_Method; 

when 19 => 
--# line 32 "vole_lex.l"
 return Key_Self; 

when 20 => 
--# line 33 "vole_lex.l"
 return Key_Send; 

when 21 => 
--# line 34 "vole_lex.l"
 return Key_Super; 

when 22 => 
--# line 35 "vole_lex.l"
 return Key_Then; 

when 23 => 
--# line 36 "vole_lex.l"
 return Key_Tuple; 

when 24 => 
--# line 37 "vole_lex.l"
 return Key_When; 

when 25 => 
--# line 38 "vole_lex.l"
 return Key_While; 

when 26 => 
--# line 39 "vole_lex.l"
 return Key_Loop; 

when 27 => 
--# line 40 "vole_lex.l"
 return Key_New; 

when 28 => 
--# line 41 "vole_lex.l"
 return Key_Predicate; 

when 29 => 
--# line 43 "vole_lex.l"
 return Op_Shift_Left; 

when 30 => 
--# line 44 "vole_lex.l"
 return Op_Shift_Right; 

when 31 => 
--# line 45 "vole_lex.l"
 return Op_Exp; 

when 32 => 
--# line 46 "vole_lex.l"
 return Op_Add; 

when 33 => 
--# line 47 "vole_lex.l"
 return Op_Sub; 

when 34 => 
--# line 48 "vole_lex.l"
 return Op_Mul; 

when 35 => 
--# line 49 "vole_lex.l"
 return Op_Div; 

when 36 => 
--# line 50 "vole_lex.l"
 return Op_Mod; 

when 37 => 
--# line 51 "vole_lex.l"
 return Op_Eq; 

when 38 => 
--# line 52 "vole_lex.l"
 return Op_Not_Eq; 

when 39 => 
--# line 53 "vole_lex.l"
 return Op_Gt; 

when 40 => 
--# line 54 "vole_lex.l"
 return Op_Gt_Eq; 

when 41 => 
--# line 55 "vole_lex.l"
 return Op_Lt_Eq; 

when 42 => 
--# line 56 "vole_lex.l"
 return Op_Lt; 

when 43 => 
--# line 57 "vole_lex.l"
 return Op_Assign; 

when 44 => 
--# line 58 "vole_lex.l"
 return Op_And; 

when 45 => 
--# line 59 "vole_lex.l"
 return Op_Or; 

when 46 => 
--# line 60 "vole_lex.l"
 return Op_Xor; 

when 47 => 
--# line 61 "vole_lex.l"
 return Op_Not; 

when 48 => 
--# line 63 "vole_lex.l"
 return Actor_Type; 

when 49 => 
--# line 64 "vole_lex.l"
 return Boolean_Type; 

when 50 => 
--# line 65 "vole_lex.l"
 return Integer_Type; 

when 51 => 
--# line 66 "vole_lex.l"
 return Unsigned_Type; 

when 52 => 
--# line 67 "vole_lex.l"
 return Float_Type; 

when 53 => 
--# line 68 "vole_lex.l"
 return String_Type; 

when 54 => 
--# line 69 "vole_lex.l"
 return Tuple_Type; 

when 55 => 
--# line 71 "vole_lex.l"
 return True_Literal; 

when 56 => 
--# line 72 "vole_lex.l"
 return False_Literal; 

when 57 => 
--# line 74 "vole_lex.l"
 return Id_Token; 

when 58 => 
--# line 76 "vole_lex.l"
 return Float_Literal; 

when 59 => 
--# line 78 "vole_lex.l"
 return Integer_Literal; 

when 60 => 
--# line 80 "vole_lex.l"
 return String_Literal; 

when 61 => 
--# line 82 "vole_lex.l"
 return Colon_Token; 

when 62 => 
--# line 83 "vole_lex.l"
 return Dot_Token; 

when 63 => 
--# line 84 "vole_lex.l"
 return Comma_Token; 

when 64 => 
--# line 85 "vole_lex.l"
 return Eos_Token; 

when 65 => 
--# line 86 "vole_lex.l"
 return Block_Begin; 

when 66 => 
--# line 87 "vole_lex.l"
 return Block_End; 

when 67 => 
--# line 88 "vole_lex.l"
 return Paren_Begin; 

when 68 => 
--# line 89 "vole_lex.l"
 return Paren_End; 

when 69 => 
--# line 90 "vole_lex.l"
 return Tuple_Begin; 

when 70 => 
--# line 91 "vole_lex.l"
 return Tuple_End; 

when 71 => 
--# line 93 "vole_lex.l"
 null; 

when 72 => 
--# line 94 "vole_lex.l"
 Inc_Line; 

when 73 => 
--# line 96 "vole_lex.l"
 null; 

when 74 => 
--# line 99 "vole_lex.l"
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
--# line 99 "vole_lex.l"

end kv.avm.Vole_Lex;

