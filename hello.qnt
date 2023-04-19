module hello {
    var consoleOutput : str
    var readByUser : str
    var actionTaken : str

    def require(cond : bool) : bool = cond

    val ALL_POSSIBLE_GREETINGS = Set("Hello", "Zdravo", "Hola")

    action write = all {
        
        nondet chosenGreeting = oneOf(ALL_POSSIBLE_GREETINGS)
        consoleOutput' = chosenGreeting,
        readByUser' = readByUser,
        actionTaken' = "write",
        require(consoleOutput == ""),
    }

    action read = all {
        require(not(consoleOutput == "")),
        readByUser' = consoleOutput,
        consoleOutput' = "",
        actionTaken' = "read"
    }

    action init = all {
        consoleOutput' = "",
        readByUser' = "",
        actionTaken' = ""
    }

    action step = any {
        write,        
        read 
    }

    run normalTest = init.then(write).then(read)
    run multipleWriteTest = init.then(write).then(write).then(read)


}
