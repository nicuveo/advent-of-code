use "files"
use "collections"
use "kiuatan"
use "promises"


type Step is (I64,I64)


primitive Password
  fun make_rule(): Rule[U8] val =>
    """
    Builds the parsing rule for each line of the input.
    """
    recover
      let digit = Rule[U8]("digit", Single[U8]("0123456789"))
      let dash  = Rule[U8]("dash",  Single[U8]("-"))
      let space = Rule[U8]("space", Single[U8](" "))
      let colon = Rule[U8]("colon", Single[U8](":"))
      let any   = Rule[U8]("any",   Single[U8](""))
      Rule[U8]("input", Conj[U8]([
        Star[U8](digit, 1)
        dash
        Star[U8](digit, 1)
        space
        any
        colon
        space
        Star[U8](any, 1)
      ]))
    end

  fun get_string(res: Success[U8]): String =>
    """
    Extracts the entirety of the matched string from a parse success.
    """
    recover
      var s = String
      for char in res.start.values(res.next) do
        s.push(char)
      end
      s
    end

  fun process(env: Env, res: Success[U8]): Step? =>
    """
    Processes one parsed line of input.
    """
    let elems    = res.children(0)?.children
    let fstInt   = get_string(elems(0)?).i64()?
    let sndInt   = get_string(elems(2)?).i64()?
    let char     = get_string(elems(4)?)(0)?
    let password = get_string(elems(7)?)

    // part 1
    var amount: I64 = 0
    for c in password.values() do
      if c == char then
        amount = amount + 1
      end
    end
    let part1: I64 = if (amount >= fstInt) and (amount <= sndInt) then 1 else 0 end

    // part 2
    var part2: I64 = 0
    try
      let c1 = password(fstInt.usize() - 1)? == char
      let c2 = password(sndInt.usize() - 1)? == char
      if c1 != c2 then
        part2 = 1
      end
    end

    (part1, part2)


actor Day02
  new create(env: Env val, input': FileLines iso) =>
    let rule = Password.make_rule()
    let input: FileLines ref = consume input'
    let elems: Array[Promise[Step] tag] = []

    for line in input do
      let parser = Parser[U8]([consume line])
      let promise = Promise[Step]
      parser.parse(rule, {(result: Result[U8]) =>
        match result
        | let success: Success[U8] =>
          try
            promise.apply(Password.process(env, success)?)
          else
            env.out.print("error while processing step")
            promise.reject()
          end
        | let failure: Failure[U8] =>
          env.out.print(failure.get_message())
          promise.reject()
        end
        })
      elems.push(promise)
    end

    Promises[Step].join(elems.values()).next[None]({(result: Array[Step] val) =>
      var part1: I64 = 0
      var part2: I64 = 0
      for (a,b) in result.values() do
        part1 = part1 + a
        part2 = part2 + b
      end
      env.out.print("part1: " + part1.string())
      env.out.print("part2: " + part2.string())
    })
