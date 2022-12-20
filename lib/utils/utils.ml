include StringUtils
module StringUtils = StringUtils

include ListUtils
module ListUtils = ListUtils

include FunOps
module FunOps = FunOps

include RefUtils
module RefUtils = RefUtils

module Stack = struct
  include Stack
  let pop_n s i = 
      let rec helper s i acc =
          if i <= 0
          then acc
          else 
              let x = Stack.pop s in
              helper s (i - 1) (x::acc)
      in
      helper s i []
end