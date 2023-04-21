module constants{

    pure val MAX_BID = 2^31 - 1
    pure val EMPTY_BID = -1
    pure val NO_WINNER = "none"
    
}

module helpers{
    def require(cond : bool) : bool = cond

    pure def max(a : int, b : int) : int = 
        if (a > b) 
            a 
        else 
            b
    }

// This is an alternative implementation.
// It uses a formulaic approach of maintainig only one state variable state,
// which is a mapping from all original variables to their values.
// Each action looks like this:
//  action A = all {precondition, s' = stateUpdateFunction()}
// 
// The name `bidOO` comes from the fact that the style of writing reminds of object-oriented
// programming style.
// 
// Some advantages of this approach:
//  1) Avoiding problems of "not updating all variables in a single state"
//  2) Easier to debug actions because all update functions can be inspected using REPL
// 
// Some drawbacks of this approach:
//  1) the maps update syntax state.with("field_name", x) is not very readable and uses
//  strings (instead of field variables) to refer to fields.

module bidOO{
    import constants.*
    import helpers.*

    type STATE_TYPE = {
        deadline : bool,
        bid_A : int,
        bid_B : int,
        winner : str}

    var state : STATE_TYPE
    

    action init = all {
        state' = {
            deadline: false,
            bid_A: EMPTY_BID,
            bid_B: EMPTY_BID,
            winner: NO_WINNER}
    }

    def bidAUpdate(s : STATE_TYPE) : STATE_TYPE =
        nondet bidAmount = oneOf(1.to(MAX_BID)) 
        s.with("bid_A", bidAmount)
        

    action bidByA = all {
        require(not(state.deadline)),
        state' = state.bidAUpdate()
        
    }

    def bidBUpdate(s : STATE_TYPE) : STATE_TYPE =
        nondet bidAmount = oneOf(1.to(MAX_BID)) 
        s.with("bid_B", bidAmount)
    

    action bidByB = all {
        require(not(state.deadline)),
        state' = state.bidAUpdate()
    }

    def deadlineStateUpdate(s : STATE_TYPE) : STATE_TYPE =
        val winner = 
            if (max(s.bid_A, s.bid_B) == EMPTY_BID or s.bid_A == s.bid_B)
                NO_WINNER
            else
                if (s.bid_A > s.bid_B) "A" else "B"        
        
        s.with("winner", winner).with("deadline", true)
        
        

    
    action deadlineHit = all {        
        require(not(state.deadline)),
        state' = state.deadlineStateUpdate()
        
    }

    action step = any {
        bidByA,
        bidByB,
        deadlineHit
    }

    }