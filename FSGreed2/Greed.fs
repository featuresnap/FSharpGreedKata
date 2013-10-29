namespace GreedLib

module Greed =
    
    type Die = int
    type Score = int
    type Dice = Die list
    type MatchResult = (Score * Dice)
    type ScoringRule = Dice -> MatchResult option 

    let score = function |Some(score, dice) -> score |_ -> 0
    let remainder = function |Some( _, dice) -> dice |_ -> []

    let rec removeFirstOccurrence value list = 
         match list with
         |x :: xs when x = value ->  xs
         |x :: xs -> x :: removeFirstOccurrence value xs
         |_ -> []

    let rec removeFirstNOccurrences value list n =
        match list with 
        |x::xs when x = value -> removeFirstNOccurrences value xs (n-1)
        |x::xs -> x :: removeFirstNOccurrences value xs n
        |_ -> []

    let makeSingleDieMatchRule dieValue points : ScoringRule = fun dice ->
        let found = dice |> List.exists ((=) dieValue)
        match found with 
        |true -> Some(points, dice |> removeFirstOccurrence dieValue)
        |_ -> None

    let makeMultiDiceMatchRule (dieValue, number) points : ScoringRule = fun dice -> 
        let inThisGroup = 
            dice 
            |> Seq.groupBy id 
            |> Seq.tryFind (fst >> ((=) dieValue)) 
            |> Option.map (snd >> Seq.length)
        match inThisGroup with 
        |Some(n) when n = number -> 
            Some(points, removeFirstNOccurrences(dieValue)(dice)(number))
        |_ -> None

    let evaluateRules dice rules = 
        rules |> List.choose (fun rule -> rule dice) 

    let rec generateScoreSets (dice:Dice) (rules:ScoringRule list) : MatchResult list list= 
        [
            let matches = evaluateRules dice rules
            if matches |> List.length = 0 then yield []
            for (score, remainder) in matches do
            match remainder with 
            |[] -> yield [(score, remainder)]
            |_ -> for nextMove in generateScoreSets remainder rules do
                  yield (score, remainder)::nextMove
        ]     

    let findBestScore dice rules = 
        generateScoreSets dice rules
        |> List.map(List.sumBy fst)
        |> List.max
        
          
    let matchSingleOne : ScoringRule = makeSingleDieMatchRule 1 100
    let matchSingleFive : ScoringRule = makeSingleDieMatchRule 5 50
    let matchThreeOnes : ScoringRule = makeMultiDiceMatchRule (1,3) 1000
    let matchThreeTwos : ScoringRule = makeMultiDiceMatchRule (2,3) 200
    let matchThreeThrees : ScoringRule = makeMultiDiceMatchRule (3,3) 300
    let matchThreeFours : ScoringRule = makeMultiDiceMatchRule (4,3) 400
    let matchThreeFives : ScoringRule = makeMultiDiceMatchRule (5,3) 500
    let matchThreeSixes : ScoringRule = makeMultiDiceMatchRule (6,3) 600

    let standardRules = [matchSingleOne; matchSingleFive; matchThreeOnes;
                    matchThreeTwos; matchThreeThrees; matchThreeFours ;
                    matchThreeFives; matchThreeSixes ]