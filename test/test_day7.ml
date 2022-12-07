open! Base
open! Lib.Day7

open Utils.StringUtils

let test_input = Stdio.In_channel.read_all "../inputs/day7.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let example = box_trim {|
    $ cd /
    $ ls
    dir a
    14848514 b.txt
    8504156 c.dat
    dir d
    $ cd a
    $ ls
    dir e
    29116 f
    2557 g
    62596 h.lst
    $ cd e
    $ ls
    584 i
    $ cd ..
    $ cd ..
    $ cd d
    $ ls
    4060174 j
    8033020 d.log
    5626152 d.ext
    7214296 k
|}

let%expect_test "parse" =
    let res = parse example in
    Stdio.printf "%d\n" (List.length res);
    [%expect {| 10 |}];
    List.nth_exn res 1 |> (fun {output;_} -> output) |> List.iter ~f:Stdio.print_endline;
    [%expect {|
      dir a
      14848514 b.txt
      8504156 c.dat
      dir d 
    |}];
    List.nth_exn res 1 |> (fun {output;_} -> output) |> List.length |> Stdio.printf "%d\n";
    [%expect {| 4 |}]

let%expect_test "dir_tree" =
    let root_dir = construct_dir_tree @@ parse example in
    Stdio.printf "%d\n" (List.length @@ get_children root_dir);
    [%expect {| 4 |}];
    let a = get_child root_dir "a" |> Option.value_exn in
    Stdio.printf "%d\n" (List.length @@ get_children a);
    [%expect {| 4 |}];
    let b = get_child root_dir "b.txt" |> Option.value_exn in
    Stdio.printf "%d\n" (get_file_size b);
    [%expect {| 14848514 |}];
    let d = get_child root_dir "d" |> Option.value_exn in
    Stdio.printf "%d\n" (List.length @@ get_children d);
    [%expect {| 4 |}]

let%expect_test "part1" =
    Stdio.printf "%d\n" (part1 example);
    [%expect {| 95437 |}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 1077191 |}]

let%expect_test "part2" =
    Stdio.printf "%d\n" (part2 example);
    [%expect {| 24933642 |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 5649896 |}]
