
let rec fact n m =
    match m with
    | 0 -> 1
    | m -> n * fact (n-1) (m-1)


let choose n m =
    match n < 2*m with
    | true -> (fact n (n-m)) / (fact (n-m) (n-m))
    | false -> (fact n m) / (fact m m)


type Reactant = Reactant of string


type Reaction = {
    inputs: (int*Reactant) list;
    outputs: (int*Reactant) list;
    rate: float;
}


type Threshold = {
    reactant: Reactant;
    conditional: int -> int -> bool;
    threshold: int;
}


let prob_helper x c =
    match x < c with
    | true -> 0.0
    | false -> (choose x c) |> float


let calc_prob (reaction: Reaction) (quantities: Map<Reactant, int>) =
    // calculate the probability of the given reaction happening given the available quantities
    [for c,r in reaction.inputs -> prob_helper (match quantities.TryFind r with | Some x -> x | None -> 0) c] |> List.reduce (*)


let rec select_helper probs total_prob target sum =
    match probs with
    | (p,r)::li ->
        let new_sum = sum + p/total_prob
        match new_sum >= target with
        | true -> Some r
        | false -> select_helper li total_prob target new_sum
    | [] -> None


let random = System.Random()


let select_reaction (probs: (float*Reaction) list) (total_prob: float) =
    let target = random.NextDouble()
    select_helper probs total_prob target 0.0


let rec update_helper (inputs: (int*Reactant) list) (outputs: (int*Reactant) list) (quantities: Map<Reactant, int>) =
    match inputs with 
    | (i,r)::li ->
        let new_quantity = (quantities.Item r) - i
        update_helper li outputs (Map.add r new_quantity quantities)
    | [] ->
        match outputs with 
        | (o,r)::li ->
            let new_quantity =
                match quantities.TryFind r with
                | Some x -> x + o
                | None -> o
            update_helper [] li (Map.add r new_quantity quantities)
        | [] -> quantities


let update_quantities (reaction: Reaction) (quantities: Map<Reactant, int>) = 
    update_helper reaction.inputs reaction.outputs quantities


let react reactions quantities =
    let probs = [for r in reactions -> (calc_prob r quantities),r]
    let total_prob = [for p,_ in probs -> p] |> List.reduce (+)
    let reaction = select_reaction probs total_prob
    match reaction with
    | Some r -> update_quantities r quantities
    | None -> quantities


let rec thresholds_met thresholds (quantities: Map<Reactant, int>) =
    // returns true once a threshold is met
    match thresholds with
    | [] -> false
    | t::li ->
        match quantities.TryFind t.reactant with
        | None -> thresholds_met li quantities
        | Some q ->
            match t.conditional q t.threshold with
            | true -> true
            | false -> thresholds_met li quantities


let rec simulate (reactions: Reaction list) thresholds (quantities: Map<Reactant, int>) (iter: int) =
    match iter >= 10000, thresholds_met thresholds quantities with
    | true, _ | _, true -> quantities
    | _, _ ->
        let updated_quantities = react reactions quantities
        //printfn "%A" updated_quantities
        simulate reactions thresholds updated_quantities (iter+1) 


//let a = Reactant "a"
//let b = Reactant "b"
//let c = Reactant "c"

//let reactions = [
//    { inputs=[1,a; 1,b]; outputs=[1,c]; rate=1.0; };
//    { inputs=[1,c]; outputs=[1,a; 1,b]; rate=0.1; };
//]

//let thresholds = [
//    { reactant=c; conditional=(>=); threshold=90; }
//]

//let quantities = 
//    Map.empty.
//        Add(a, 100).
//        Add(b, 100).
//        Add(c, 0)

//simulate reactions thresholds quantities 0 |> ignore


// Simulate log function
// Reactants
let b = Reactant "b"
let a = Reactant "a"
let Y = Reactant "Y"
let c = Reactant "c"
let Yp = Reactant "Yp"
let W = Reactant "W"
let m = Reactant "m"

let reactions = [
    { inputs=[1,b]; outputs=[1,a; 1,b]; rate=1.0; };
    { inputs=[1,a; 2,Y]; outputs=[1,c; 1,Yp; 1,a]; rate=1000.0; };
    { inputs=[2,c]; outputs=[1,c]; rate=1000.0; };
    { inputs=[1,a]; outputs=[1,W]; rate=100.0; };
    { inputs=[1,Yp]; outputs=[1,Y]; rate=10.0; };
    { inputs=[1,c]; outputs=[1,m]; rate=10.0 };
]

let thresholds = [
    { reactant=Y; conditional=(<=); threshold=1; }
]

let quantities = 
    Map.empty.
        Add(b, 1).
        Add(a, 1).
        Add(Y, 32).
        Add(c, 0).
        Add(Yp, 0).
        Add(W, 0).
        Add(m, 0)

let ending_quantities = simulate reactions thresholds quantities 0

printfn "%A" ending_quantities
