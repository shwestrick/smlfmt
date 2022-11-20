structure TabbedStringDoc =
  PrettyTabbedDoc(struct
    open TerminalColorString
    val niceRed = TerminalColors.hsv {h=0.0, s=0.5, v=0.9}
    fun emphasize s = background niceRed s
    fun toString t = TerminalColorString.toString {colors=false} t 
  end)