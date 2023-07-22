
/// <summary>
/// Global random object used for the generation of random numbers.
/// </summary>
let random = System.Random()

/// <summary>
/// Generalized factorial function.
/// </summary>
let rec fact n m =
    match m with
    | 0u -> 1u
    | m -> n * fact (n-1u) (m-1u)


/// <summary>
/// Calculate the binomial coefficient (n choose k)
/// </summary>
let choose n m =
    match n < 2u*m with
    | true -> (fact n (n-m)) / (fact (n-m) (n-m))
    | false -> (fact n m) / (fact m m)


/// <summary>
/// A type defining a reactant for use in a reaction.
/// </summary>
type Reactant = Reactant of string


/// <summary>
/// A type defining a reaction that has inputs, outputs, and a rate.
/// </summary>
type Reaction = {
    inputs: (uint*Reactant) list;
    outputs: (uint*Reactant) list;
    rate: float;
}


type Threshold = {
    reactant: Reactant;
    conditional: uint -> uint -> bool;
    threshold: uint;
}


/// <summary>
/// Calculate the binomial coefficient unless x < c in which case return 0.
/// </summary>
let prob_helper x c =
    match x < c with
    | true -> 0.0
    | false -> (choose x c) |> float


/// <summary>
/// Calculate the probability of a reaction occuring given the quantities already present.
/// </summary>
/// <param name="probs"></param>
/// <param name="total_prob"></param>
/// <param name="target"></param>
/// <param name="sum"></param>
let calc_prob (reaction: Reaction) (quantities: Map<Reactant, uint>) =
    // calculate the probability of the given reaction happening given the available quantities
    [for c,r in reaction.inputs -> prob_helper (match quantities.TryFind r with | Some x -> x | None -> 0u) c]
    |> List.reduce (*)


/// <summary>
/// Help select which reaction to run next.
/// </summary>
/// <param name="probs"></param>
/// <param name="total_prob"></param>
let rec select_helper probs total_prob target sum =
    match probs with
    | (p,r)::li ->
        let new_sum = sum + p/total_prob
        // Select the reaction if the threshold for selecting the given reaction is met.
        match new_sum >= target with
        | true -> Some r
        | false -> select_helper li total_prob target new_sum
    | [] -> None


/// <summary>
/// Randomly select which reaction to run next.
/// </summary>
let select_reaction (probs: (float*Reaction) list) (total_prob: float) =
    let target = random.NextDouble()
    select_helper probs total_prob target 0.0


/// <summary>
///
/// </summary>
let rec update_helper (inputs: (uint*Reactant) list) (outputs: (uint*Reactant) list) (quantities: Map<Reactant, uint>) =
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


/// <summary>
/// Update the reactant quantities as the result of a single reaction taking place.
/// </summary>
let update_quantities (reaction: Reaction) (quantities: Map<Reactant, uint>) = 
    update_helper reaction.inputs reaction.outputs quantities


/// <summary>
/// A single step of the simulation: calculate probabilities, choose a reaction, and update quantities.
/// </summary>
let react reactions quantities =
    // calculate the probabilities for each reaction
    let probs = [for r in reactions -> (r.rate * calc_prob r quantities),r]
    // sum the probabilities
    let total_prob = [for p,_ in probs -> p] |> List.reduce (+)
    // select a reaction to run
    let reaction = select_reaction probs total_prob
    // update quantities if a reaction was selected
    match reaction with
    | Some r -> update_quantities r quantities
    | None -> quantities


/// <summary>
/// Check if any threshold has been met.
/// </summary>
/// <param name="thresholds"></param>
/// <param name="quantities"></param>
let rec thresholds_met thresholds (quantities: Map<Reactant, uint>) =
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


/// <summary>
/// Run a simulation of the reaction.
/// </summary>
/// <param name="reactions"></param>
/// <param name="thresholds"></param>
/// <param name="quantities"></param>
/// <param name="iter"></param>
let rec simulate (reactions: Reaction list) thresholds (quantities: Map<Reactant, uint>) (iter: uint) =
    // Run for a maximum of 10000 iterations or until a threshold is met.
    match iter >= 10000u, thresholds_met thresholds quantities with
    | true, _ | _, true -> quantities
    | _, _ ->
        let updated_quantities = react reactions quantities
        //printfn "%A" updated_quantities
        simulate reactions thresholds updated_quantities (iter+1u) 


[<EntryPoint>]
let main argv =

    //let H = Reactant "H"
    //let O = Reactant "O"
    //let N = Reactant "N"
    //let H2 = Reactant "H2"
    //let O2 = Reactant "O2"
    //let N2 = Reactant "N2"

    //let reactions = [
    //    { inputs=[2u, H]; outputs=[1u, H2]; rate=10.0; };
    //    { inputs=[2u, O]; outputs=[1u, O2]; rate=1.0; };
    //    //{ inputs=[2u, N]; outputs=[1u, N2]; rate=1.0; };
    //]

    //let thresholds = []

    //let quantities =
    //    Map.empty.
    //        Add(H, 1000u).
    //        Add(O, 1000u).
    //        Add(N, 1000u).
    //        Add(H2, 0u).
    //        Add(O2, 0u).
    //        Add(N2, 0u)

    //let iterations = 10000

    //// TODO: this could be parallized (but be careful to handle random number generation in a thread-safe way)
    //let ending_quantities = [
    //    for _ in 1..iterations ->
    //        let q = (simulate reactions thresholds quantities 0u)
    //        q.Item(H), q.Item(O), q.Item(N), q.Item(H2), q.Item(O2), q.Item(N2)
    //]

    //let count_quantities (a,b,c,d,e,f) (aa, bb, cc, dd, ee, ff) =
    //    a+aa, b+bb, c+cc, d+dd, e+ee, f+ff

    //let fiterations = float iterations

    ////printfn "%A" ending_quantities
    //let h, o, n, h2, o2, n2 = List.reduce count_quantities ending_quantities
    //(h |> float) / fiterations |> printfn "%f"
    //(o |> float) / fiterations |> printfn "%f"
    //(n |> float) / fiterations |> printfn "%f"
    //(h2 |> float) / fiterations |> printfn "%f"
    //(o2 |> float) / fiterations |> printfn "%f"
    //(n2 |> float) / fiterations |> printfn "%f"


    // Simulate multiplication.

    // Reactants
    let X = Reactant "X"
    let a = Reactant "a"
    let Y = Reactant "Y"
    let Yp = Reactant "Yp"
    let Z = Reactant "Z"
    let W = Reactant "W"

    let reactions = [
        { inputs=[1u,X]; outputs=[1u,a]; rate=1.0; };
        { inputs=[1u,a; 1u,Y]; outputs=[1u,a; 1u,Yp; 1u,Z]; rate=100000.0; };
        { inputs=[1u,a]; outputs=[1u,W]; rate=10000.0; };
        { inputs=[1u,Yp]; outputs=[1u,Y]; rate=1000.0; };
    ]

    let thresholds = []

    let quantities = 
        Map.empty.
            Add(X, 2u).
            Add(a, 0u).
            Add(Y, 5u).
            Add(Yp, 0u).
            Add(Z, 0u).
            Add(W, 0u)

    //let ending_quantities = simulate reactions thresholds quantities 0u

    let ending_quantities = [for _ in 1..100 -> (simulate reactions thresholds quantities 0u).Item(Z)]

    //printfn "%A" ending_quantities
    ((List.reduce (+) ending_quantities) |> float) / 100.0 |> printfn "%f"


    0