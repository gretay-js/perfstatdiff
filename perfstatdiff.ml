open Core

let verbose = ref false

type t = {
  name : string;
  count : float;
  variance : string;
} [@@deriving compare, sexp]

type u = { name: string; base: t; change: t; abs : float; rel : float }

let mk (v1:t) (v2:t) =
  {
    name=v1.name;
    base=v1;
    change=v2;
    abs=v2.count -. v1.count;
    rel = (v2.count /. v1.count)
  }

let pr_int_or_float x =
  let n = Float.iround_towards_zero_exn x in begin
    if float_of_int n = x then
      Int.to_string_hum ~delimiter:',' n
    else
      Float.to_string_hum ~delimiter:',' x
  end

let pr { name; base; change; abs; rel } =
  [
    pr_int_or_float base.count;
    base.variance;
    pr_int_or_float change.count;
    change.variance;
    pr_int_or_float abs;
    Float.to_string_hum rel;
    name
  ]

let add_headers file1 file2 csv =
  let headers =
    [
      file1;
      "+-";
      file2;
      "+-";
      "Difference";
      "";
      ""
    ]
  in
  headers::csv

let row_to_t = function
  | "<not supported>"::_ -> None
  | c::_::name::variance::_ ->
    Some {
      name;
      count=float_of_string c;
      variance
    }
  | s -> failwith (sprintf !"Unexpected format %{sexp:string list}" s)

let load file =
  let csv = Csvlib.Csv.load file in
  let timestamp = List.hd_exn (List.hd_exn csv) in
  let csv =  List.tl_exn csv in
  let csv = Csvlib.Csv.trim csv in
  if !verbose then printf "Reading data from %s\nPerf run %s\n" file timestamp;
  if !verbose then Csvlib.Csv.print_readable csv;
  let data = List.filter_map csv ~f:row_to_t in
  if !verbose then List.iter data ~f:(fun x -> printf !"%{sexp:t}\n" x);
  List.map data ~f:(fun t -> (t.name, t))
  |> Map.of_alist_exn (module String)

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
    fun i row ->
      let row = List.zip_exn widths row in
      List.iteri ~f:(
        fun j (width, cell) ->
          let output_padding () =
            let n = String.length cell in
            repeat (fun () -> output_string " ") (width - n + 1);
          in
          if (i = 0 (* header row *)
              ||  (j = (List.length row)-1) (* header column *)) then begin
            (* left justified *)
            output_string " ";
            output_string cell;
            output_padding ()
            end
          else begin (* right justified *)
            (* output_string "|"; *)
            output_padding ();
            output_string cell
          end
      ) row;
      (* if i = 0 then begin
       *   output_string "\n";
       *   repeat (fun () -> output_string "_") tablewidth
       * end; *)
      output_string "\n"
  ) csv

let main file1 file2 outfile v =
  if v then verbose := true;
  let merge ~key = function
    | `Right _ -> failwith (sprintf "Missing entry for %s in %s" key file1)
    | `Left _  -> failwith (sprintf "Missing entry for %s in %s" key file2)
    | `Both (v1, v2) -> Some (mk v1 v2)
  in
  let d1 = load file1 in
  let d2 = load file2 in
  let data = Map.merge d1 d2 ~f:merge in
  let list = List.rev (Map.data data) in
  let csv_out = add_headers file1 file2 (List.map ~f:pr list) in
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

FILE1 and FILE2 are in CSV format."
    )
    Command.Let_syntax.(
      let%map_open file1 = anon ("FILE1" %: file)
      and file2 = anon ("FILE2" %: file)
      and outfile = flag "-o" (optional file) ~doc:"output csv filename"
      and verbose = flag "-v" no_arg ~doc:" turns on verbose"
      in
      fun () -> main file1 file2 outfile verbose)


let () = Command.run command

