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

module bid{
    import constants.*
    import helpers.*

    var deadline : bool
    var bid_A : int
    var bid_B : int
    var winner : str

    action init = all {
        deadline' = false,
        bid_A' = EMPTY_BID,
        bid_B' = EMPTY_BID,
        winner' = NO_WINNER
    }

    action bidByA = all {
        require( not(deadline) ),
        nondet bidAmount = oneOf(1.to(MAX_BID))
        bid_A' = bidAmount,
        bid_B' = bid_B,
        winner' = winner,
        deadline' = deadline
    }

    action bidByB = all {
        require( not(deadline) ),
        deadline' = deadline,        
        nondet bidAmount = oneOf(1.to(MAX_BID))        
        bid_B' = bidAmount,
        bid_A' = bid_A,
        winner' = winner
    }
    
    action deadlineHit = all {
        require( not(deadline) ),
        deadline' = true,        
        winner' = 
            if (max(bid_A, bid_B) == EMPTY_BID or bid_A == bid_B)
                NO_WINNER
            else
                if (bid_A > bid_B) "A" else "B",
        bid_A' = bid_A,
        bid_B' = bid_B
        
        
    }

    action step = any {
        bidByA,
        bidByB,
        deadlineHit
    }

    }