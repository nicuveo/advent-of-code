use "files"
use "collections"

actor Day01
  let _env: Env

  new create(env: Env val, input': FileLines iso) =>
    _env = env
    try
      let input: FileLines ref = consume input'
      var values = Set[I64].create()
      for line in input do
        let value = line.i64() ?
        values.set(value)
      end
      part1(values)
      part2(values)
    else
      env.out.print("Day01: something went wrong")
    end

  fun part1(input: Set[I64] box) =>
    for value in input.values() do
      if input.contains(2020-value) then
        _env.out.print("Day01 part1: " + (value * (2020-value)).string())
        return
      end
    end

  fun part2(input: Set[I64] box) =>
    for value1 in input.values() do
      for value2 in input.values() do
        let value3 = 2020 - (value1 + value2)
        if input.contains(value3) then
          _env.out.print("Day01 part2: " + (value1 * value2 * value3).string())
          return
        end
      end
    end
