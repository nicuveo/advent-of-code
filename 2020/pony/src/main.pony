use "files"

actor Main
  let _env: Env

  fun _get_day(): String ? =>
    try
      _env.args(1)?
    else
      _env.out.print("usage: aoc day\n    day: double digit")
      error
    end

  fun _get_input(day: String): FileLines iso^ ? =>
    let caps = recover val FileCaps.>set(FileRead).>set(FileStat) end
    try
      recover
        let file = OpenFile(
          FilePath(_env.root as AmbientAuth, "../input/" + day + ".in", caps)?) as File
        file.lines()
      end
    else
      _env.out.print("error: could not read input for day " + day)
      error
    end

  new create(env: Env) =>
    _env = env
    try
      let day   = _get_day()?
      let input = _get_input(day)?
      match day
      | "01" => Day01(env, consume input)
      | "02" => Day02(env, consume input)
      | "03" => Day03(env, consume input)
      else
        env.out.print("error: day " + day + " not found")
      end
    end
