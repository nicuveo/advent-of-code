use "files"
use "collections"


actor Day03
  fun trees(elems: Array[Set[USize]], n: USize, dx: USize, dy: USize): I64 =>
    var x: USize = 0
    var y: USize = 0
    var t: I64 = 0
    try
      while y < elems.size() do
        if elems(y)?.contains(x) then t = t + 1 end
        y = y + dy
        x = (x + dx) % n
      end
      t
    else
      0
    end

  new create(env: Env val, input': FileLines iso) =>
    let rule = Password.make_rule()
    let input: FileLines ref = consume input'
    var elems: Array[Set[USize]] = []
    var n: USize = 0

    for line' in input do
      var set = Set[USize]
      let line: String val = consume line'
      n = line.size()
      for (index, char) in line.array().pairs() do
        if char == '#' then
          set.set(index)
        end
      end
      elems.push(set)
    end
    let t11 = trees(elems, n, 1, 1)
    let t31 = trees(elems, n, 3, 1)
    let t51 = trees(elems, n, 5, 1)
    let t71 = trees(elems, n, 7, 1)
    let t12 = trees(elems, n, 1, 2)
    let part2 = t11 * t31 * t51 * t71 * t12    
    env.out.print("part1: " + t31.string())
    env.out.print("part2: " + part2.string())
