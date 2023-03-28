
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
    // TODO: choose the next reaction for real
    //let p, r = probs[0]
    //match p > 0.0 with 
    //| true -> Some r
    //| false -> None
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


let rec simulate (reactions: Reaction list) (iter: int) (quantities: Map<Reactant, int>) =
    match iter < 10 with
    | false -> quantities
    | true ->
        let updated_quantities = react reactions quantities
        printfn "%A" updated_quantities
        simulate reactions (iter+1) updated_quantities


let a = Reactant "a"
let b = Reactant "b"
let c = Reactant "c"

let reactions = [
    { inputs=[1,a; 1,b]; outputs=[1,c]; rate=1.0; };
]

let quantities = 
    Map.empty.
        Add(a, 10).
        Add(b, 10).
        Add(c, 0)

simulate reactions 0 quantities |> ignore
