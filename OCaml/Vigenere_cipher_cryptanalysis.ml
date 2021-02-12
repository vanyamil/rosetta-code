(* Task : Vigenere_cipher/Cryptanalysis *)

(*  
    Given a ciphertext encrypted with a Vigenere cipher,
    extracts the key and the resulting decrypted text.
    Uses the Friedman test to find best key length.
*)

(*** Included code (remove before submission) ***)

type key = string

(* Rotate a single uppercase letter *)
let ascii_caesar_shift : bool -> char -> char -> char = 
    let min_range = Char.code 'A' in
    let max_range = Char.code 'Z' in
    (* aka 26 but this code can be adapted to larger ranges, such as the ASCII printable range (codes 32 to 126). *)
    let range_len = max_range - min_range + 1 in 
    let actual_fun (dir : bool) (c1 : char) (c2 : char) : char =
        let n1 = Char.code c1 in
        let n2 = Char.code c2 - min_range in
        let sum = (if dir then (+) else (-)) n1 n2 in
        ( (* Effectively mod function, but simplified and works on negatives. *)
            if sum > max_range
            then sum - range_len
            else if sum < min_range
            then sum + range_len
            else sum
        ) |> Char.chr
    in
    actual_fun

(* Remove non-letters and uppercase *)
let ascii_upper_letters_only (s : string) : string = 
    let slen = String.length s in
    (* Make a list of escaped uppercase letter chars *)
    let rec toLetterList sidx acc = 
        if sidx >= slen 
        then List.rev acc
        else
        let c = s.[sidx] in
        if c >= 'A' && c <= 'Z' 
        then toLetterList (sidx + 1) ((c |> Char.escaped) :: acc)
        else if c >= 'a' && c <= 'z'
        then toLetterList (sidx + 1) ((c |> Char.uppercase_ascii |> Char.escaped) :: acc)
        else toLetterList (sidx + 1) acc
    in
    toLetterList 0 [] |> String.concat ""

(*** Actual task at hand ***)

let vig_crypt (dir : bool) (key : key) (message : string) : string =
    let message = ascii_upper_letters_only message in
    let key = ascii_upper_letters_only key in
    let klen = String.length key in
    let aux idx c = 
        let kidx = idx mod klen in
        let e = ascii_caesar_shift dir c key.[kidx] in
        e
    in
    String.mapi aux message

let encrypt : key -> string -> string = vig_crypt true
let decrypt : key -> string -> string = vig_crypt false
;;

(*** Helpers ***)

(* Implementation of Float.round to avoid v4.08 *)
let round (x : float) : float = 
    let rem = mod_float x 1. in
    if rem >= 0.5 
    then ceil x
    else floor x

(* A function that updates array element at a position *)
let array_update (arr : 'a array) (idx : int) (update : 'a -> 'a) : unit =
    let curr = Array.get arr idx in
    Array.set arr idx (update curr)

(*** Actual task at hand ***)

(* the n'th element of array is how often the n'th letter was found *)
let observe_coincidences ?(step : int = 1) ?(offset : int = 0) (text : string) : int array =
    let arr = Array.make 26 0 in
    let a_code = Char.code 'A' in
    String.iteri (fun idx c -> if idx mod step = offset then array_update arr (Char.code c - a_code) succ) text;
    arr

(* Obtain correlation factor for the observed coincidences *)
let correlation_factor ?(sort : bool = true) (coincidences : int array) (freqs : float list) : float =
    let clist = Array.to_list coincidences in
    let clist = (if sort then List.sort compare clist else clist) in
    List.fold_left2 (fun acc c f -> acc +. (float_of_int c *. f)) 0. clist freqs

(* Friedman correlation test - turned out to not be good enough for the test input *)
let friedman_test (freqs : float list) (text : string) : int = 
    let n = String.length text in
    let kappa_p = List.fold_left (fun acc el -> acc +. el *. el) 0. freqs in
    let kappa_r = 1. /. (freqs |> List.length |> float_of_int) in
    let observed_coincidences = observe_coincidences text in
    let observed_rate = Array.fold_left (fun acc el -> acc + el * (el - 1)) 0 observed_coincidences |> float_of_int in
    let kappa_o = observed_rate /. (n * (n - 1) |> float_of_int) in
    ((kappa_p -. kappa_r) /. (kappa_o -. kappa_r)) |> round |> int_of_float

(* Translation of the test used in other Rosetta Code solutions *)
let shifted_coincidences_test (freqs : float list) (text : string) : int = 
    let sorted_freqs = List.sort compare freqs in
    let bestCorr = -100. in 
    let max_keylen = String.length text / 20 in
    let rec helper idx (cur_len, cur_corr) (best_len, best_corr) = 
        if cur_len = max_keylen then (* Finished testing everything *)
            best_len
        else if idx = cur_len then (* Finished testing this key length *)
            let (best_len, best_corr) = if cur_corr > best_corr then (cur_len, cur_corr) else (best_len, best_corr) in
            helper 0 (cur_len + 1,  ~-.0.5 *. float_of_int (cur_len + 1)) (best_len, best_corr)
        else
            let coincidences = observe_coincidences ~step:cur_len ~offset:idx text in
            let factor = correlation_factor coincidences sorted_freqs in
            helper (succ idx) (cur_len, cur_corr +. factor) (best_len, best_corr)
    in
    helper 0 (2, ~-.1.) (1, ~-.100.)

(* Returns the most likely shift value for this set *)
let break_caesar ?(step : int = 1) ?(offset : int = 0) (text : string) (freqs : float list) : int =
    let c_arr = observe_coincidences ~step ~offset text in
    let rec helper l curShift (maxShift, maxCorr) = 
        if curShift = 26 
        then maxShift
        else
            let corr = correlation_factor ~sort:false c_arr l in
            let l' = List.tl l @ [List.hd l] in
            if corr > maxCorr 
            then helper l' (curShift + 1) (curShift, corr)
            else helper l' (curShift + 1) (maxShift, maxCorr)
    in
    helper freqs 0 (-1, -100.)

let break (keylen : int) (text : string) (freqs : float list) : key = 
    let rec getCaesars idx acc = 
        if idx >= keylen then acc else
        let shift = break_caesar ~step:keylen ~offset:idx text freqs in
        let new_code = if shift = 0 then Char.code 'A' else Char.code 'Z' + 1 - shift in
        getCaesars (succ idx) (acc ^ Char.(new_code |> chr |> escaped))
    in
    getCaesars 0 ""

let cryptanalyze (freqs : float list) (text : string) : key * string = 
    let text = ascii_upper_letters_only text in
    let keylen = shifted_coincidences_test freqs text in
    let key = break keylen text freqs in
    let pt = decrypt key text in
    (key, pt)

(*** Output ***)

let _ =
    let long_text = "\
MOMUD EKAPV TQEFM OEVHP AJMII CDCTI FGYAG JSPXY ALUYM NSMYH \
VUXJE LEPXJ FXGCM JHKDZ RYICU HYPUS PGIGM OIYHF WHTCQ KMLRD \
ITLXZ LJFVQ GHOLW CUHLO MDSOE KTALU VYLNZ RFGBX PHVGA LWQIS \
FGRPH JOOFW GUBYI LAPLA LCAFA AMKLG CETDW VOELJ IKGJB XPHVG \
ALWQC SNWBU BYHCU HKOCE XJEYK BQKVY KIIEH GRLGH XEOLW AWFOJ \
ILOVV RHPKD WIHKN ATUHN VRYAQ DIVHX FHRZV QWMWV LGSHN NLVZS \
JLAKI FHXUF XJLXM TBLQV RXXHR FZXGV LRAJI EXPRV OSMNP KEPDT \
LPRWM JAZPK LQUZA ALGZX GVLKL GJTUI ITDSU REZXJ ERXZS HMPST \
MTEOE PAPJH SMFNB YVQUZ AALGA YDNMP AQOWT UHDBV TSMUE UIMVH \
QGVRW AEFSP EMPVE PKXZY WLKJA GWALT VYYOB YIXOK IHPDS EVLEV \
RVSGB JOGYW FHKBL GLXYA MVKIS KIEHY IMAPX UOISK PVAGN MZHPW \
TTZPV XFCCD TUHJH WLAPF YULTB UXJLN SIJVV YOVDJ SOLXG TGRVO \
SFRII CTMKO JFCQF KTINQ BWVHG TENLH HOGCS PSFPV GJOKM SIFPR \
ZPAAS ATPTZ FTPPD PORRF TAXZP KALQA WMIUD BWNCT LEFKO ZQDLX \
BUXJL ASIMR PNMBF ZCYLV WAPVF QRHZV ZGZEF KBYIO OFXYE VOWGB \
BXVCB XBAWG LQKCM ICRRX MACUO IKHQU AJEGL OIJHH XPVZW JEWBA \
FWAML ZZRXJ EKAHV FASMU LVVUT TGK"
    in
    let english_freqs = [
        0.08167; 0.01492; 0.02782; 0.04253; 0.12702; 0.02228; 0.02015;
        0.06094; 0.06966; 0.00153; 0.00772; 0.04025; 0.02406; 0.06749;
        0.07507; 0.01929; 0.00095; 0.05987; 0.06327; 0.09056; 0.02758;
        0.00978; 0.02360; 0.00150; 0.01974; 0.00074
    ] 
    in
    let (key, pt) = cryptanalyze english_freqs long_text in
    Printf.printf "Key:  %s\n\nText: %s" key pt
;;