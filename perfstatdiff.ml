open Core

let verbose = ref false
let transpose = ref false
let split = ref false

type t = {
  count : float;
  variance : string; (* from perf -r *)
} [@@deriving compare, sexp]

type v = {
  abs : float;
  rel : float
}

let get_v ~base t =
  {
    abs = t.count -. base.count;
    rel = t.count /. base.count;
  }


let pr_int_or_float x =
  let n = Float.iround_towards_zero_exn x in
    if float_of_int n = x then
      Int.to_string_hum ~delimiter:',' n
    else
      Float.to_string_hum ~delimiter:',' x


let pr_t t =
  [
    pr_int_or_float t.count;
    t.variance
  ]

let pr_v v =
  [
    pr_int_or_float v.abs;
    Float.to_string_hum v.rel
  ]

let pr base t =
  match base with
  | None -> pr_t t
  | Some base ->
    let v = get_v ~base t
    in (pr_t t)@(pr_v v)

let pr_filename filename =
  let open Filename in
  basename filename |> chop_extension

let v_header = ["Difference";"%"]

let t_header title = [title;"+-  "]

let add_headers_transposed titles csv =
  let headers =
  List.map titles
    ~f:(fun title -> (t_header title) @ v_header)
  |> List.concat
  (* for the title column *)
  |> List.cons " "
  in
  headers::csv

let add_headers titles csv =
  let headers =
  List.map (List.tl_exn titles)
    ~f:(fun title -> (t_header title) @ v_header)
  |> List.concat
  (* for baseline *)
  |> List.append (t_header (List.hd_exn titles))
  (* for the title column *)
  |> List.cons " "
  in
  headers::csv

let row_to_t = function
  | "<not supported>"::_ -> None
  | c::_::name::variance::_ ->
    Some (name, { count=float_of_string c; variance })
  | s -> failwith (sprintf !"Unexpected format %{sexp:string list}" s)

(* reads csv file output of perf stat and produces a map
   from event names to measurements of the event: counter and variance. *)
let load file =
  let csv = Csvlib.Csv.load file in
  (* chop off the first line which is a timestamp,
     and any empty lines around the data. *)
  let timestamp = List.hd_exn (List.hd_exn csv) in
  let csv =  List.tl_exn csv in
  let csv = Csvlib.Csv.trim csv in
  if !verbose then printf "Reading data from %s\nPerf run %s\n" file timestamp;
  if !verbose then Csvlib.Csv.print_readable csv;
  let data = List.filter_map csv ~f:row_to_t in
  if !verbose then
    List.iter data ~f:(fun (name,x) -> printf !"%s %{sexp:t}\n" name x);
  Map.of_alist_exn (module String) data

(* Csvlib.Csv.print_readable that right-adjusts the cells,
   except the header row and column which are left adjusted. *)
let print_readable csv =
  let output_string = Out_channel.output_string stdout in
  (* Escape all the strings in the CSV file first. *)
  let csv = List.map ~f:(fun x -> List.map x ~f:String.escaped) csv in

  (* let csv = square csv in *)

  (* Find the width of each column. *)
  let widths =
    match csv with
    | [] -> []
    | r :: _ ->
      let n = List.length r in
      let lengths = List.map ~f:(fun x -> (List.map x ~f:String.length)) csv in
      let max2rows r1 r2 =
        let rp = List.zip_exn r1 r2 in
        List.map ~f:(fun ((a : int), (b : int)) -> max a b) rp
      in
      let rec repeat x = function
        | 0 -> []
        | i -> x :: repeat x (i-1)
      in
      List.fold_left ~f:max2rows ~init:(repeat 0 n) lengths in

  (* let tablewidth = List.fold ~init:0 ~f:( + ) widths in *)
  (* Print out each cell at the correct width. *)
  let rec repeat f = function
    | 0 -> ()
    | i -> f (); repeat f (i-1)
  in
  List.iteri ~f:(
    fun _i row ->
      let row = List.zip_exn widths row in
      List.iteri ~f:(
        fun _j (width, cell) ->
          let output_padding () =
            let n = String.length cell in
            repeat (fun () -> output_string " ") (width - n + 1);
          in
          (* right justified *)
          (* output_string "|"; *)
          output_padding ();
          output_string cell
      ) row;
      (* if i = 0 then begin
       *   output_string "\n";
       *   repeat (fun () -> output_string "_") tablewidth
       * end; *)
      output_string "\n"
  ) csv

let main files outfile =
  (* parse the files and create a map from filename to data
     where data is a map from event name to measurements for that event. *)
  let data = List.fold files
               ~init:(Map.empty (module String))
               ~f:(fun map file -> Map.add_exn map ~key:file ~data:(load file))
  in
  (* collect names of events *)
  let event_names = List.fold (Map.data data)
                      ~init:[]
                      ~f:(fun acc events -> acc@(Map.keys events))
                    |> List.dedup_and_sort ~compare:String.compare in

  (* compare to baseline, which the first file in the list of input filenames *)
  let base_filename = List.hd_exn files in
  let baseline_data = Map.find_exn data base_filename in

  let create () =
    (* events are rows, files are columns *)
    List.map event_names ~f:(fun eventname ->
      (* CR gyorsh:  handle base that doesn't have all measurements  *)
      let base = Map.find_exn baseline_data eventname in
      List.fold files ~init:[eventname]
        ~f:(fun row filename ->
          (* CR gyorsh: handle files with different events *)
          let filedata = Map.find_exn data filename in
          let counter = Map.find_exn filedata eventname in
          if filename = base_filename  then
            row@(pr None counter)
          else
            row@(pr (Some base) counter)
        ))
    |> add_headers (List.map files ~f:pr_filename)
  in
  let create_transposed () =
    List.map files ~f:(fun filename ->
      let filedata = Map.find_exn data filename in
      List.map event_names ~f:(fun event_name ->
        let base = Map.find_exn baseline_data event_name in
        let counter = Map.find_exn filedata event_name in
        (pr (Some base) counter)
      )
      |> List.concat
      |> List.cons (pr_filename filename)
    )
    |> add_headers_transposed event_names
  in
  let create_split () =
    (* events are rows, files are columns *)
    let basetitle = pr_filename base_filename in
    List.map (List.tl_exn files)
      ~f:(fun filename ->
        let filedata = Map.find_exn data filename in
        List.map event_names ~f:(fun eventname ->
          let base = Map.find_exn baseline_data eventname in
          let counter = Map.find_exn filedata eventname in
            [eventname]@(pr None base)@(pr (Some base) counter))
        |> add_headers [basetitle; pr_filename filename])
    |> List.concat
  in
  let csv_out =
    if !transpose then
      create_transposed ()
    else if !split then
      create_split ()
    else
      create ()
  in
  if !verbose then Csvlib.Csv.print_readable csv_out;
  print_readable csv_out;
  match outfile with
  | None -> ()
  | Some f  -> Csvlib.Csv.save f csv_out


let command =
  Command.basic
    ~summary:"Diff outputs of perf stat in csv format"
    ~readme:(fun () ->
      "Compare output of perf stat. For example, run:
      $ perf stat -o FILE1 -x , -r 5 -ddd <cmd1>
      $ perf stat -o FILE2 -x , -r 5 -ddd <cmd2>
      $ perfstatdiff FILE1 FILE2

FILE1 and FILE2 are in CSV format.

Default output format follows that of perf stat: events are rows.
This works well when there are many recorded events
and two input files (or at most 3 input files).
With more than 3 input files, it is easier to read a transposed table,
produced using -t option: files are rows and events are columns.
If there are more than 2 or 3 events, and more than 3 files,
it is easier to split into multiple tables, one for each input file
in comparison to the baseline, using the option -s.
The first file in the arguments list is assumed to be the baseline.
"
    )
    Command.Let_syntax.(
      let%map_open files = anon (non_empty_sequence_as_list ("FILE" %: file))
      and outfile = flag "-o" (optional file) ~doc:"output csv filename"
      and v = flag "-v" no_arg ~doc:" verbose"
      and t = flag "-t" no_arg ~doc:" events are columns"
      and s = flag "-s" no_arg ~doc:" separate tables"
      in
      if v then verbose := true;
      if t then transpose := true;
      if s then split := true;
      fun () -> main files outfile)


let () = Command.run command

