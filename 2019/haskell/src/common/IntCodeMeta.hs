{-# LANGUAGE QuasiQuotes #-}

module IntCodeMeta (makeInterpreter) where

import           Data.String.QQ
import           IntCodePlusPlus

makeInterpreter :: [Int]
makeInterpreter = either error id $ transpile "interpreter" [s|
base  = 2580
index = 0
read size
while (index < size) {
  read ${base + index}
  index = index + 1
}

index = base
[loop]

// ADD
if ($index ==    1) {
  ${base + ${index + 3}} = ${base + ${index + 1}} + ${base + ${index + 2}}
  index = index + 4
  goto loop
}
if ($index ==  101) {
  ${base + ${index + 3}} = ${index + 1} + ${base + ${index + 2}}
  index = index + 4
  goto loop
}
if ($index == 1001) {
  ${base + ${index + 3}} = ${base + ${index + 1}} + ${index + 2}
  index = index + 4
  goto loop
}
if ($index == 1101) {
  ${base + ${index + 3}} = ${index + 1} + ${index + 2}
  index = index + 4
  goto loop
}

// MUL
if ($index ==    2) {
  ${base + ${index + 3}} = ${base + ${index + 1}} * ${base + ${index + 2}}
  index = index + 4
  goto loop
}
if ($index ==  102) {
  ${base + ${index + 3}} = ${index + 1} * ${base + ${index + 2}}
  index = index + 4
  goto loop
}
if ($index == 1002) {
  ${base + ${index + 3}} = ${base + ${index + 1}} * ${index + 2}
  index = index + 4
  goto loop
}
if ($index == 1102) {
  ${base + ${index + 3}} = ${index + 1} * ${index + 2}
  index = index + 4
  goto loop
}

// INPUT
if ($index == 3) {
  read ${base + ${index + 1}}
  index = index + 2
  goto loop
}

// OUTPUT
if ($index ==   4) {
  print ${base + ${index + 1}}
  index = index + 2
  goto loop
}
if ($index == 104) {
  print ${index + 1}
  index = index + 2
  goto loop
}

// JUMP IF TRUE
if ($index ==    5) {
  if (${base + ${index + 1}} != 0) {
    index = base + ${base + ${index + 2}}
    goto loop
  }
  index = index + 3
  goto loop
}
if ($index ==  105) {
  if (${index + 1} != 0) {
    index = base + ${base + ${index + 2}}
    goto loop
  }
  index = index + 3
  goto loop
}
if ($index == 1005) {
  if (${base + ${index + 1}} != 0) {
    index = base + ${index + 2}
    goto loop
  }
  index = index + 3
  goto loop
}
if ($index == 1105) {
  if (${index + 1} != 0) {
    index = base + ${index + 2}
    goto loop
  }
  index = index + 3
  goto loop
}

// JUMP IF FALSE
if ($index ==    6) {
  if (${base + ${index + 1}} == 0) {
    index = base + ${base + ${index + 2}}
    goto loop
  }
  index = index + 3
  goto loop
}
if ($index ==  106) {
  if (${index + 1} == 0) {
    index = base + ${base + ${index + 2}}
    goto loop
  }
  index = index + 3
  goto loop
}
if ($index == 1006) {
  if (${base + ${index + 1}} == 0) {
    index = base + ${index + 2}
    goto loop
  }
  index = index + 3
  goto loop
}
if ($index == 1106) {
  if (${index + 1} == 0) {
    index = base + ${index + 2}
    goto loop
  }
  index = index + 3
  goto loop
}

// LESS THAN
if ($index ==    7) {
  ${base + ${index + 3}} = 0
  if (${base + ${index + 1}} < ${base + ${index + 2}}) {
    ${base + ${index + 3}} = 1
  }
  index = index + 4
  goto loop
}
if ($index ==  107) {
  ${base + ${index + 3}} = 0
  if (${index + 1} < ${base + ${index + 2}}) {
    ${base + ${index + 3}} = 1
  }
  index = index + 4
  goto loop
}
if ($index == 1007) {
  ${base + ${index + 3}} = 0
  if (${base + ${index + 1}} < ${index + 2}) {
    ${base + ${index + 3}} = 1
  }
  index = index + 4
  goto loop
}
if ($index == 1107) {
  ${base + ${index + 3}} = 0
  if (${index + 1} < ${index + 2}) {
    ${base + ${index + 3}} = 1
  }
  index = index + 4
  goto loop
}

// EQUALS
if ($index ==    8) {
  ${base + ${index + 3}} = 0
  if (${base + ${index + 1}} == ${base + ${index + 2}}) {
    ${base + ${index + 3}} = 1
  }
  index = index + 4
  goto loop
}
if ($index ==  108) {
  ${base + ${index + 3}} = 0
  if (${index + 1} == ${base + ${index + 2}}) {
    ${base + ${index + 3}} = 1
  }
  index = index + 4
  goto loop
}
if ($index == 1008) {
  ${base + ${index + 3}} = 0
  if (${base + ${index + 1}} == ${index + 2}) {
    ${base + ${index + 3}} = 1
  }
  index = index + 4
  goto loop
}
if ($index == 1108) {
  ${base + ${index + 3}} = 0
  if (${index + 1} == ${index + 2}) {
    ${base + ${index + 3}} = 1
  }
  index = index + 4
  goto loop
}

// END
if ($index == 99) {
  goto end
}

// UNKNOWN OPCODE
goto @-1

[end]
|]
