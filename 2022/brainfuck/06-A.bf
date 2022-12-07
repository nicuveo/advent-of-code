init the memory with a two bytes counter; a buffer; and the three first bytes
{ 0 3 ' 0 0 ' 0 A B @C@ }
>+++>>>  >,>,>,

// while we have't found a match
>+ [[-]
  now: { x x ' 0 0 ' A B C @D@ }
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
  >>>>>,

  now: { x x ' 0 0 ' B C D @E@ }
  first: duplicate everything
  [-   >+ >+>>+>>+<<<<<                  <]    > [-   <+   >] <<
  [-  >>+ >>>>>>+>+>>+<<<<<<<<<         <<]   >> [-  <<+  >>] <<<
  [- >>>+ >>>>+>>>>>>+>+<<<<<<<<<<<    <<<]  >>> [- <<<+ >>>] <<<<
  [->>>>+ >>+>>>>>>+>>>>+<<<<<<<<<<<< <<<<] >>>> [-<<<<+>>>>] >>>>>>>>>>>>

  now: { x x ' 0 0 ' B C D E ' 0 E B E C E D D B D C C @B@ }
  perform all six comparisons; increase accum by one for each match
  [-<->]+<[[-]>-<]> [-<<<<<<<<<<<<+>>>>>>>>>>>>] <<
  [-<->]+<[[-]>-<]> [-  <<<<<<<<<<+>>>>>>>>>>  ] <<
  [-<->]+<[[-]>-<]> [-    <<<<<<<<+>>>>>>>>    ] <<
  [-<->]+<[[-]>-<]> [-      <<<<<<+>>>>>>      ] <<
  [-<->]+<[[-]>-<]> [-        <<<<+>>>>        ] <<
  [-<->]+<[[-]>-<]> [-          <<+>>          ] <<

  now: { x x ' 0 0 ' B C D E @n@ }
  if n == 0 then all bytes were different and we found it
  otherwise the loop continues
]

go back to the index counter
<[-]<[-]<[-]<[-]<<

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

print the result including leading zero digits
>++++++++++++++++++++++++++++++++++++++++++++++++.
>++++++++++++++++++++++++++++++++++++++++++++++++.
>++++++++++++++++++++++++++++++++++++++++++++++++.
>++++++++++++++++++++++++++++++++++++++++++++++++.

print newline
[-]++++++++++.
