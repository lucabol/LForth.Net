: ( [char] ) parse drop drop ; immediate
: \ 0 word drop ; immediate
: variable create 0 , ;
: constant create , does> @ ;

\ Arithmetic
: 1+ 1 + ;
: 2+ 2 + ;
: 1- 1 - ;
: 2- 2 - ;
: min ( n1 n2 -- n3 )  over over > if swap then drop ;
: max ( n1 n2 -- n3 )  over over < if swap then drop ;
: mod ( n n -- n )  1 swap */mod drop ;
: dec 10 base ! ;
: hex 16 base ! ;
: 2* 2 * ;
: negate -1 * ;

\ Stack
: rot ( x1 x2 x3 -- x2 x3 x1 )  >r  swap r> swap ;
: nip ( x1 x2 -- x2 )  swap drop ;
: tuck ( x1 x2 -- x2 x1 x2 )  swap over ;
: -rot ( x1 x2 x3 -- x3 x2 x1 )  rot rot ;
: ?dup dup 0 <> if dup then ;

\ Boolean
0 constant false
false invert constant true
: 0= 0 = ;
: 0< 0 < ;
: 0> 0 > ;
: or ( x x -- x )  invert swap invert and invert ; ( do morgan )
: xor ( x x -- x )  over over invert and >r swap invert and r> or ;
: lshift ( x1 u -- x2 )  begin dup while >r  2*  r> 1 - repeat drop ;
: endif postpone then ; immediate

\ Memory
: ? @ . ;
: +! ( x addr -- )  swap over @ + swap ! ;
: chars ;
: char+ ( c-addr1 -- c-addr2 )  1 chars + ;
: cell+ ( addr1 -- addr2 )  1 cells + ;
: aligned ( addr -- a-addr )  cell+ 1 -   1 cells 1 - invert  and ;

\ Compiler
: ' bl word find drop ;
: ['] ' postpone literal ; immediate
