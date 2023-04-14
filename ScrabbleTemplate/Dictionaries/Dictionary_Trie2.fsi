module Dictionary
    type Dictionary
        val empty:
            unit -> Dictionary
        val insert:
            string -> Dictionary -> Dictionary
        val lookup:
            string -> Dictionary -> bool
        val step:
            char -> Dictionary -> (bool * Dictionary) option