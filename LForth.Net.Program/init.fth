: ( [char] ) word drop ; immediate
: \ 0 word drop ; immediate
: 1+ 1 + ;
: ? @ . ;
: +! ( x addr -- )  SWAP OVER @ + SWAP ! ;
: dec 10 base ! ;
: hex 16 base ! ;
: variable create 0 , ;
: constant create , does> @ ;
: 2* 2 * ;
: negate -1 * ;
: 0= 0 = ;
: 0< 0 < ;
: 0> 0 > ;
: ?dup dup 0 <> if dup then ;
