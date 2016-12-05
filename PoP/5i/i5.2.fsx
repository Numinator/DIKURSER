let gennemsnit (fx : float list)  =
  let fxLen = (float) fx.Length
  match fx with
  | [] -> None
  | _  -> Some (List.fold (fun (f : float) (e : float) -> f / fxLen + e) 0.0 fx)

gennemsnit []
gennemsnit [2.0; 0.0]
