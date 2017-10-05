Require Import Coq.Lists.List.
Require Import Io.All.
Require Import Io.System.All.
Require Import ListString.All.

Import ListNotations.
Import C.Notations.
Require Import String.

Fixpoint dec (n : nat) (line : LString.t) : LString.t :=
  match (n, LString.to_string line) with
    | (O, str)    => LString.s str
    | (S n', str) =>
      dec n' (LString.s ((fix dec' line :=
                             match line with
                               | String " " s              => String " " (dec' s)
                               | String "#" (String "#" s) => String "#" s
                               | s                         => s
                             end) str))
  end.

Fixpoint inc (n:nat) (line : LString.t) : LString.t :=
  match (n, LString.to_string line) with
    | (O, str)    => LString.s str
    | (S n', str) =>
      inc n' (LString.s ((fix inc' st :=
                            match st with
                              | String " " s => String " " (inc' s)
                              | String "#" s => String "#" (String "#" s)
                              | s            => s
                            end) str))
  end.

Import Ascii.
Definition new_line : Ascii.ascii := "010".

Definition map_line f (code : LString.t) : LString.t :=
  (LString.join (LString.s (String new_line ""))) (map f (LString.split code new_line)).

Definition is_minus (num : LString.t) : bool :=
  match LString.to_string num with
    | String "-" _ => true
    | _            => false
  end.

Definition lstr2nat (num : LString.t) : option nat :=
  let abs_num := match LString.to_string num with
                  | String "-" n => LString.s n
                  | n            => LString.s n
                end in
  match (LString.to_N 10 abs_num) with
    | Some n => Some (BinNat.N.to_nat n)
    | None   => None
  end.           

Definition shift (argv : list LString.t) : C.t System.effect unit :=
  match argv with
  | [_; num; file_name] =>
    let! content := System.read_file file_name in
    match content with
    | None         => System.log (LString.s "Cannot read the file.")
    | Some content =>
      let shifter := if is_minus num then dec else inc in
      match lstr2nat num with
        | Some n => System.log (map_line (shifter n) content)
        | None   => System.log (LString.s "Wrong number spec.")
      end
    end
  | _                   => System.log (LString.s "Exact 2 parameters expected.")
  end.

(* Definition foo (arg : ascii) (s:string) : bool := *)
(*   match s with *)
(*     | String arg  _ => true *)
(*     | String c s => false *)
(*     | _   => false *)
(*   end. *)
(* Compute foo "b" "a b". *)

Fixpoint replace (str : string) : string :=
  match str with
    | String "-" s => String " " (replace s)
    | String c   s => String c   (replace s)
    | EmptyString  => EmptyString
  end.

Definition drop_dir (path : string) : string :=
  (fix drop path file :=  match path with
                           | String "/" s => drop s s
                           | String c   s => drop s file
                           | EmptyString  => file
                         end) path path.

Fixpoint combi (argv : list LString.t) : C.t System.effect unit :=
  match argv with
    | cons _ file_names =>
      (fix combi' file_names' := 
         match file_names' with
           | nil             => ret tt
           | cons name names => 
             let! content := System.read_file name in
             let! _ := match content with
                        | None         =>
                          System.log (LString.s "Cannot read the file.")
                        | Some content =>
                          let name' := drop_dir (replace (LString.to_string name)) in
                          let title := substring 0 ((length name')-3) name' in
                          let! _ := System.log (LString.s "# " ++ LString.s title) in
                          System.log (map_line (inc 1) content)
                      end in
             combi' names
         end) file_names
    | _                  => System.log (LString.s "Only 1 parameter expected.")
  end.

Definition main := Extraction.launch combi.
Extraction "./extraction/main" main.
