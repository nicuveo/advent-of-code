use "files"

actor Day01
  new create(env: Env val, input': FileLines iso) =>
    try
      let input: FileLines ref = consume input'
      var part1: I64 = 0
      var part2: I64 = 0
      for line in input do
        let value = line.i64() ?
        part1 = part1 + fuel(value)
        part2 = part2 + total_fuel(value)
      end
      env.out.print("Part1: " + part1.string())
      env.out.print("Part2: " + part2.string())
    else
      env.out.print("Day01: something went wrong")
    end

  fun fuel(mass: I64): I64 =>
    (mass / 3) - 2

  fun total_fuel(mass: I64): I64 =>
    let f = fuel(mass)
    if f <= 0 then 0 else f + total_fuel(f) end
