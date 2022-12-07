init the memory with counter buffer and four first bytes
{ 0 4 0 0 A B C @D@ }
>++++>>> ,>,>,>,

while current input isn't eof
[

now: { x x 0 0 A B C @D@ }
first duplicate everything
[-   >+ >+>>+>>+<<<<<                  <]    > [-   <+   >] <<
[-  >>+ >>>>>>+>+>>+<<<<<<<<<         <<]   >> [-  <<+  >>] <<<
[- >>>+ >>>>+>>>>>>+>+<<<<<<<<<<<    <<<]  >>> [- <<<+ >>>] <<<<
[->>>>+ >>+>>>>>>+>>>>+<<<<<<<<<<<< <<<<] >>>> [-<<<<+>>>>] >>>>>>>>>>>>

now: { x x 0 0 A B C D 0  D A  D B  D C  C A  C B  B <A> }
perform all six comparisons; increase counter by one for each match
[-<->]+<[[-]>-<]> [-<<<<<<<<<<<<+>>>>>>>>>>>>] <<
[-<->]+<[[-]>-<]> [-  <<<<<<<<<<+>>>>>>>>>>  ] <<
[-<->]+<[[-]>-<]> [-    <<<<<<<<+>>>>>>>>    ] <<
[-<->]+<[[-]>-<]> [-      <<<<<<+>>>>>>      ] <<
[-<->]+<[[-]>-<]> [-        <<<<+>>>>        ] <<
[-<->]+<[[-]>-<]> [-          <<+>>          ] <<

now: { x x 0 0 A B C D @n@ }
if n == 0 then all bytes were different and we found it

create a copy of not n for later
[->+>+<<]>[-<+>]+>[[-]<->]<<

if n non zero we must continue
[ [-]

  shift all three previous values (copy each to previous cell)
  <<<<[-]
  >[-<+>]
  >[-<+>]
  >[-<+>]

  go back to counter then inc by one
  <<<<<<+

  check for overflow
  [->+>+<<]>  duplicate to both buffer cells
  [-<+>]+>    copy back then set to 1
  [[-]<->]<   set previous cell to 0 if non 0
  [-<<+>>]    if 1 then counter is 0: inc first cell by one

  go back to cell for last byte; read input; go back to n
  >>>>>,>
]

if n was zero we must break out of the loop
> [ -<[-]<[-]<[-]<[-]<[-] ] <<

if n was non zero: now: { x x 0 0 A B C @D@ }
if n was     zero: now: { x x @0@ }
]

we must transform our two bytes of index into digits
increase last digit by 1 for each in lower byte
<[>>>>+
then loop back to carry values greater than 9
[-  >+>+<<  ]  >[-   <+>   ]>+ >>>++++++++++
[-<<<-[->+>+<<]>>[-<<+>>]+<[[-]>-<]>[->[-]<]>]<<<[[-]<<----------<+>>>]<<<
[- >>+>+<<< ] >>[-  <<+>>  ]>+ >>>++++++++++
[-<<<-[->+>+<<]>>[-<<+>>]+<[[-]>-<]>[->[-]<]>]<<<[[-]<<<----------<+>>>>]<<<<
[->>>+>+<<<<]>>>[- <<<+>>> ]>+ >>>++++++++++
[-<<<-[->+>+<<]>>[-<<+>>]+<[[-]>-<]>[->[-]<]>]<<<[[-]<<<<----------<+>>>>>]<<<<<
<-]

increase digits by 2 5 6 for each in higher byte
<[>>>++>+++++>++++++
loop back to carry values greater than 9
[-  >+>+<<  ]  >[-   <+>   ]>+ >>>++++++++++
[-<<<-[->+>+<<]>>[-<<+>>]+<[[-]>-<]>[->[-]<]>]<<<[[-]<<----------<+>>>]<<<
[- >>+>+<<< ] >>[-  <<+>>  ]>+ >>>++++++++++
[-<<<-[->+>+<<]>>[-<<+>>]+<[[-]>-<]>[->[-]<]>]<<<[[-]<<<----------<+>>>>]<<<<
[->>>+>+<<<<]>>>[- <<<+>>> ]>+ >>>++++++++++
[-<<<-[->+>+<<]>>[-<<+>>]+<[[-]>-<]>[->[-]<]>]<<<[[-]<<<<----------<+>>>>>]<<<<<
<<-]>

print the result, including leading zero digits
>++++++++++++++++++++++++++++++++++++++++++++++++.
>++++++++++++++++++++++++++++++++++++++++++++++++.
>++++++++++++++++++++++++++++++++++++++++++++++++.
>++++++++++++++++++++++++++++++++++++++++++++++++.

print newline
[-]++++++++++.
