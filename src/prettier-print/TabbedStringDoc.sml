structure TabbedStringDoc =
  PrettyTabbedDoc(struct
    open TerminalColorString

    val compaction = CommandLineArgs.parseReal "s-compact" 1.1 (* must be >= 1 *)
    val maxSat = CommandLineArgs.parseReal "s-max" 0.6

    val hues = Seq.fromList
      [ 0
      , 30
      , 55
      , 90
      , 140
      , 180
      , 210
      , 250
      , 290
      , 320
      ]

    fun niceRed depth =
      let
        val s = (compaction-1.0 +
          (1.0 / (1.0 + (Real.fromInt depth / compaction)))) / compaction
        val s = s * maxSat

        (* val d = if depth mod 2 = 0 then 2*(depth div 2)+1 else 2*(depth div 2) *)
        val d = 3*(depth-1)
        val h = Real.fromInt (Seq.nth hues (d mod Seq.length hues))
      in
        TerminalColors.hsv {h=h, s=s, v=0.9}
      end

    fun emphasize depth s = backgroundIfNone (niceRed depth) s

    fun toString t = TerminalColorString.toString {colors=false} t
  end)