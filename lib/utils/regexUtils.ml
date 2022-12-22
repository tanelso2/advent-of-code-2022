let get_first_group re s =
  let res = Re.exec re s in
  Re.Group.get res 1