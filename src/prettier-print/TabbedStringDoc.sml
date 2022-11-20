structure TabbedStringDoc =
  PrettyTabbedDoc(struct
    open TerminalColorString

    val compaction = CommandLineArgs.parseReal "s-compact" 1.05 (* must be >= 1 *)
    
    fun niceRed depth =
      let
        val s = (compaction-1.0 +
          (1.0 / (1.0 + (Real.fromInt depth / compaction)))) / compaction

        val d = if depth mod 2 = 0 then 2*(depth div 2)+1 else 2*(depth div 2)
        val h = Real.fromInt ((30 * d) mod 360)
      in
        TerminalColors.hsv {h=h, s=s, v=0.9}
      end

    fun emphasize depth s = background (niceRed depth) s

    fun toString t = TerminalColorString.toString {colors=false} t 
  end)