# import sugar
import strutils
import strformat


proc fuel(mass: int): int {. noSideEffect .} =
  result = mass div 3 - 2

proc total_fuel(mass: int): int {. noSideEffect .} =
  let f = fuel(mass)
  if f <= 0:
    result = 0
  else:
    result = f + total_fuel(f)


let input = readFile("../input/01.in")
var part1 = 0
var part2 = 0
for line in input.splitLines():
  if not line.isEmptyOrWhitespace():
    let mass = parseInt(line)
    part1 += fuel(mass)
    part2 += total_fuel(mass)

echo "### Day01"
echo &"part1: {part1}"
echo &"part2: {part2}"
