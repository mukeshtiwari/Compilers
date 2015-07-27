type prog = command list
and command =
   | Incv | Decv
   | Incp | Decp
   | Input  | Output
   | Loop of command list
   [@@deriving show]
