let bin = List.fold_left (fun b c -> b * 2 + c) 0

let string_to_int_seq str =
  str
  |> String.to_seq
  |> Seq.map (fun c -> int_of_char c - if c < 'A' then 48 else 55)
  |> Seq.concat_map (fun i -> (Seq.cons ((i / 8) mod 2) (Seq.cons ((i / 4) mod 2) (Seq.cons ((i / 2) mod 2) (Seq.cons (i mod 2) Seq.empty)))))

let a = "D2FE28" (* literal 2021 *)
let b = "38006F45291200"
let c = "EE00D40C823060"
let d = "8A004A801A8002F478"
let e = "620080001611562C8802118E34"
let f = "C0015000016115A2E0802F182340"
let g = "A0016C880162017C3686B18A3D4780"


let return x str = Some (x, str)
let fail _ = None
let (>>=) p f str = Option.bind (p str) (fun (x, str) -> f x str)
let (<|>) p q str = (function None -> Fun.id | x -> Fun.const x) (p str) (q str)
let (<*>) p_f p_x = p_f >>= fun f -> p_x >>= fun x -> return (f x)
let rec kleene p str = str |> (return List.cons <*> p <*> kleene p <|> return [])
let zero seq = match seq () with Seq.Cons (0, seq) -> Some (0, seq) | _ -> None
let one seq = match seq () with Seq.Cons (1, seq) -> Some (1, seq) | _ -> None
let rec bits n = if n > 0 then return List.cons <*> (zero <|> one) <*> bits (n - 1) else return []
let rec iter p n = if n > 0 then return List.cons <*> p <*> iter p (n - 1) else return []

let trailer = kleene zero

type packet = Lit of int * int | Op of int * int * packet list
let rec sum = function
| Lit (v, _) -> v
| Op (v, _, u) -> List.fold_left (fun n p -> n + sum p) v u

let rec eval = function  
| Lit (_, x) -> x
| Op (_, 0, u) -> List.fold_left (fun n p -> n + eval p) 0 u
| Op (_, 1, u) -> List.fold_left (fun n p -> n * eval p) 1 u
| Op (_, 2, u) -> List.fold_left (fun n p -> min n (eval p)) max_int u
| Op (_, 3, u) -> List.fold_left (fun n p -> max n (eval p)) min_int u
| Op (_, 5, u) -> if eval (List.nth u 0) > eval (List.nth u 1) then 1 else 0  
| Op (_, 6, u) -> if eval (List.nth u 0) < eval (List.nth u 1) then 1 else 0
| Op (_, 7, u) -> if eval (List.nth u 0) = eval (List.nth u 1) then 1 else 0
| _ -> failwith "eval"

let lit_header = bits 3 >>= fun v -> bits 3 >>= function [1; 0; 0] -> return v | _ -> fail
let op15_header = bits 3 >>= fun v -> bits 3 >>= fun t -> zero >>= fun _ -> bits 15 >>= fun l -> return (bin v, bin t, bin l)
let op11_header = bits 3 >>= fun v -> bits 3 >>= fun t -> one >>= fun _ -> bits 11 >>= fun n -> return (bin v, bin t, bin n)

let group1 = one >>= fun _ -> bits 4
let group0 = zero >>= fun _ -> bits 4
let literal = lit_header >>= fun v -> kleene group1 >>= fun u -> group0 >>= fun x -> return (Lit (bin v, bin (List.concat (u @ [x]))))

let parse p u = u |> List.to_seq |> (p >>= fun x -> return x) |> Option.get |> fst

let rec packet seq = seq |> (literal <|> op15 <|> op11)
    and op15 seq = seq |> (op15_header >>= fun (v, t, l) -> bits l >>= fun u -> return (Op (v, t, parse (kleene packet) u)))
    and op11 seq = seq |> (op11_header >>= fun (v, t, n) -> iter packet n >>= fun u -> return (Op (v, t, u)))

let data = "020D790050D26C13EC1348326D336ACE00EC299E6A8B929ED59C880502E00A526B969F62BF35CB4FB15B93A6311F67F813300470037500043E2D4218FA000864538E905A39CAF77386E35AB01802FC01BA00021118617C1F00043A3F4748A53CF66008D00481272D73308334EDB0ED304E200D4E94CF612A49B40036C98A7CF24A53CA94C6370FBDCC9018029600ACD529CA9A4F62ACD2B5F928802F0D2665CA7D6CC013919E78A3800D3CF7794A8FC938280473057394AFF15099BA23CDD37A08400E2A5F7297F916C9F97F82D2DFA734BC600D4E3BC89CCBABCBE2B77D200412599244D4C0138C780120CC67E9D351C5AB4E1D4C981802980080CDB84E034C5767C60124F3BC984CD1E479201232C016100662D45089A00087C1084F12A724752BEFEA9C51500566759BF9BE6C5080217910CD00525B6350E8C00E9272200DCE4EF4C1DD003952F7059BCF675443005680103976997699795E830C02E4CBCE72EFC6A6218C88C9DF2F3351FCEF2D83CADB779F59A052801F2BAACDAE7F52A8190073937FE1D700439234DBB4F7374DC0CC804CF1006A0D47B8A4200F445865170401F8251662D100909401AB8803313217C680004320D43F871308D140C010E0069E7EDD1796AFC8255800052E20043E0F42A8B6400864258E51088010B85910A0F4ECE1DFE069C0229AE63D0B8DC6F82529403203305C00E1002C80AF5602908400A20240100852401E98400830021400D30029004B6100294008400B9D0023240061C000D19CACCD9005F694AEF6493D3F9948DEB3B4CC273FFD5E9AD85CFDFF6978B80050392AC3D98D796449BE304FE7F0C13CD716656BD0A6002A67E61A400F6E8029300B300B11480463D004C401889B1CA31800211162204679621200FCAC01791CF6B1AFCF2473DAC6BF3A9F1700016A3D90064D359B35D003430727A7DC464E6401594A57C93A0084CC56A662B8C00AA424989F2A9112
"

let w = [
"C200B40A82";
"04005AC33890";
"880086C3E88112";
"CE00C43D881120";
"D8005AC2A8F0";
"F600BC2D8F";
"9C005AC2F8F0";
"9C0141080250320F1802104A08";
]