module Unsigned = struct
  let encode (x : Int64.t) : char list =
      if Int64.equal x 0L
      then [ '\x00' ]
      else (
        let rec aux acc n =
            if Int64.equal n 0L
            then acc |> List.rev |> List.map Char.chr
            else (
              let next = Int64.shift_right_logical n 7 in
              let curr = n |> Int64.to_int |> ( land ) 0x7f in
              let i = if Int64.equal next 0L then curr else curr lor 0x80 in
              aux (i :: acc) next
            )
        in
        aux [] x
      )

  let decode (s : char list) : Int64.t =
      let cs = s |> List.map Char.code |> List.rev in
      let rec aux acc = function
          | [] -> acc
          | h :: t ->
              let a2 = Int64.shift_left acc 7 in
              let a3 = h land 0x7f |> Int64.of_int |> Int64.add a2 in
              aux a3 t
      in
      aux 0L cs
end

module Signed = struct
  let encode (x : Int64.t) : char list =
      let xx =
          if Int64.compare x 0L >= 0
          then x
          else (
            let rec aux k n =
                let l = Int64.logand n 0x8000_0000_0000_0000L in
                if Int64.equal l 0L
                then aux (k + 1) (Int64.shift_left n 1)
                else (
                  let bits = 64 - k in
                  let rs = 7 - (bits mod 7) in
                  let n2 = Int64.shift_right_logical n rs in
                  let n3 = Int64.lognot n2 in
                  let n4 = Int64.shift_right_logical n3 (k - rs) in
                  Int64.add n4 1L
                )
            in
            aux 0 (Int64.abs x)
          )
      in
      Unsigned.encode xx

  let decode (s : char list) : Int64.t =
      let cs = s |> List.map Char.code |> List.rev in
      let is_neg =
          let h = List.hd cs in
          h land 0x40 = 0x40
      in
      let rec aux acc = function
          | [] -> acc
          | h :: t ->
              let a2 = Int64.shift_left acc 7 in
              let a3 = h land 0x7f |> Int64.of_int |> Int64.add a2 in
              aux a3 t
      in
      let n = aux 0L cs in
      if is_neg
      then (
        let k = 64 - (7 * List.length cs) in
        let n2 = Int64.sub n 1L in
        let n3 = Int64.shift_left n2 k in
        let n4 = Int64.lognot n3 in
        Int64.shift_right_logical n4 k
      )
      else n
end
