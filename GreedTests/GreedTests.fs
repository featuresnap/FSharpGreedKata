namespace GreedTests

module GreedTests =
    open FsUnit
    open Xunit
    open GreedLib.Greed

    let [<Fact>] ``Removing first occurrence of a die value leaves other occurrences with that value`` () =
        [1;2;1] |> removeFirstOccurrence 1 |> should equal [2;1]

    let [<Fact>] ``Single one rule scores 100`` () =
        matchSingleOne [1] |> score |> should equal 100

    let [<Fact>] ``Single one rule removes first one`` () =
        matchSingleOne [1;2] |> remainder |> should equal [2]

    let [<Fact>] ``Single five rule scores 50`` () =
        matchSingleFive [5] |> score |> should equal 50

    let [<Fact>] ``Single five rule removes first five`` () =
        matchSingleFive [2;5] |> remainder |> should equal [2]

    let [<Fact>] ``Single five rule does not match two ones`` () = 
        matchSingleFive [1;1] |> Option.isNone |> should be True

    let [<Fact>] ``Triple 1 rule scores 1000`` () = 
        matchThreeOnes [1; 1; 1] |> score |> should equal 1000

    let [<Fact>] ``Triple 1 rule removes first three ones`` () =
        matchThreeOnes [1;2;1;5;1] |> remainder |> should equal [2;5]
    
    let [<Fact>] ``Triple 1 rule does not match two ones`` () = 
        matchThreeOnes [1;1] |> Option.isNone |> should be True

    let [<Fact>] ``Find matching rules finds all rules that match`` () = 
        let rules = [matchSingleOne; matchSingleFive; matchThreeOnes]
        let matchingRules = evaluateRules [1;1;5;1] rules
        matchingRules |> List.exists((=) (1000,[5])) |> should be True
        matchingRules |> List.exists((=) (100, [1; 5; 1])) |> should be True
        matchingRules |> List.exists((=) (50, [1; 1; 1])) |> should be True

    let [<Fact>] ``Generate score sets scores three ones as singles and a triple`` () =
        let dice = [1; 1; 1]
        let rules = [matchSingleOne; matchThreeOnes]
        let waysToScore = generateScoreSets dice rules
        waysToScore |> List.length |> should equal 2
        waysToScore |> List.exists (fun matches -> matches |> List.sumBy fst = 300) |> should be True
        waysToScore |> List.exists (fun matches -> matches |> List.sumBy fst = 1000) |> should be True
    
    let [<Fact>] ``Highest score finds highest score`` () =
        let dice = [1; 1; 1]
        let rules = [matchSingleOne; matchThreeOnes]
        let bestScore = findBestScore dice rules
        bestScore |> should equal 1000

    let [<Fact>] ``Standard Scenarios`` () = 
        standardRules |> findBestScore  [1; 1; 1; 5; 1] |> should equal  1150
        standardRules |> findBestScore  [2; 3; 4; 6; 2] |> should equal  0   
        standardRules |> findBestScore  [3; 4; 5; 3; 3] |> should equal  350 
        standardRules |> findBestScore  [1; 5; 1; 2; 4] |> should equal  250 
        standardRules |> findBestScore  [5; 5; 5; 5; 5] |> should equal  600








