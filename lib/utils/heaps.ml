module IntMinHeap = Heap.Make(struct
  include Base.Int
end)

module IntMaxHeap = Heap.Make(struct
  include Base.Int
  let compare x y = Base.Int.compare x y |> Int.neg

end)