init

>>>>>>>>>>>>>>>
+[<+>,-----------------------------------------------------------------------------------]
[,----------]
<<<<<<<<

main loop

,[----------[<+>------------------------------------[
[-]<

  copy the splitter
  [->+>>>>+<<<<<]>[-<+>]>>>>>>>

  iterate over set points
  [
    <<<[->+>+<<]>>[-<<+>>]>[-<+<->>]
    if non zero we must continue
    <<[[-]<[->>>>+<<<<]>>[->+<]>>>]>>
  ]

  we either deleted an existing point ot reached the end
  delete the splitter if it wasn't in the set
  <<+>[[-]<->]<[-<[-]>]>>

  attempt to contract the set
  >>+[[-]>>[
    [-<<<<+>>>>]>>+>>
  ]<<[-<<<<]]<<

  go back to the root and bring the splitter if it still exists
  <<<<[>[-<<<<+>>>>]<<<<<]

  if we did remove a splitter we must insert its neighbours in the set
  >[
    but start by increasing the counter then do the carry
    <<<<<<+
    [-<<+>>]<+<----------[>-<++++++++++[->>+<<]]>
    [-<<+>>]<+<----------[>-<++++++++++[->>+<<]]>
    [-<<+>>]<+<----------[>-<++++++++++[->>+<<]]>
    [-<<+>>]<+<----------[>-<++++++++++[->>+<<]]>
    >[-<<+>>]>[-<<+>>]>[-<<+>>]>[-<<+>>]
    >>>>>>

    make a copy of the splitter for the other neighbour
    [-<<+>>>+<]>[-<+>]<

    start with the right neighbour
    +
    >>>

    iterate over set points
    [
      <<<[->+>+<<]>>[-<<+>>]>[-<+<->>]
      if non zero we must continue
      <<[[-]<[->>>>+<<<<]>>[->+<]>>>]>>
    ]

    we either found the value or reached the end of the set
    <[-]<<[->>>+<<<]<

    go back up
    [<<<<]

    do the left neighbour
    <[->>+<<]>>
    -
    >>>

    iterate over set points
    [
      <<<[->+>+<<]>>[-<<+>>]>[-<+<->>]
      if non zero we must continue
      <<[[-]<[->>>>+<<<<]>>[->+<]>>>]>>
    ]

    we either found the value or reached the end of the set
    <[-]<<[->>>+<<<]<

    go back up
    [<<<<]
  >]<<<

<],----------]<[-]>,]

<<<<<<<
[++++++++++++++++++++++++++++++++++++++++++++++++.[-]]>
[++++++++++++++++++++++++++++++++++++++++++++++++.[-]]>
[++++++++++++++++++++++++++++++++++++++++++++++++.[-]]>
[++++++++++++++++++++++++++++++++++++++++++++++++.[-]]>
++++++++++.
