module constants {
    val ALL_POSSIBLE_GREETINGS = Set("Hello, world!", "Hello, Quint!", "Hello, world! Hello, Quint!", "Hola", "Zdravo", "Mrhban", "Zdravstvuj")
    }

module hello {
    import constants.*
    


    // we declare a variable for the output
    // All declared variables comprise a state of the system
    var consoleOutput: str 
    var readByUser: str
    // this variable is only for our internal purposes 
    var actionTaken: str

    // no order is implied here
    // prime (x') stands for: next state value
    action init2 = all{
        consoleOutput' = "",
        readByUser' = "",
        actionTaken' = "init"
    }


    pure def require(b: bool) : bool = b

    // what do we mean when we say "all"?
    // we meant that all described relations must be true
    action write = all {
        require(consoleOutput == ""),

        nondet outputText = oneOf(ALL_POSSIBLE_GREETINGS)
        consoleOutput' = outputText,

        // Quint will likely be able to make this inference in the future
        readByUser' = readByUser,
        actionTaken' = "write"
    }

    action read = all {
        require(consoleOutput != ""),
        readByUser' = consoleOutput,
        // consoleOutput' = consoleOutput,
        consoleOutput' = "",
        actionTaken' = "read"

    }

    // action step: either a write happens, or a read happens
    action step2 = any {
        write,
        read
    }

    // run writeReadTest = init.then(write).then(read)

    // run multipleWriteReadTest = init.then(write).then(read).then(write).then(read)


    



    }