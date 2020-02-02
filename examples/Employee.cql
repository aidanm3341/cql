options
    //prover = completion
    program_allow_nontermination_unsafe = true
    allow_empty_sorts_unsafe = true
    //timeout = "1"

typeside T = literal {
    types
        string
        nat
    constants
        Aidan Akin Bob Bo Carl Cork Dan Dunn Math CS : string
        zero : nat
    functions
        succ : nat -> nat
        plus : nat, nat -> nat
}

schema S = literal : T {
    entities
        Employee
        Department
    foreign_keys
        manager   : Employee -> Employee
        worksIn   : Employee -> Department
        secretary : Department -> Employee
    attributes
        first last : Employee -> string
        age : Employee -> nat
        name : Department -> string
}

instance I = literal : S {
    generators
        one two : Employee
    equations
        one.manager=one one.worksIn.secretary=one
        two.manager=one two.worksIn = one.worksIn
        last(two) = Bo

    multi_equations
        first -> {one Aidan, two Bob}
}

instance J = literal : S {
    generators
        one two : Employee
        three four : Department
        y   : nat
    equations
        one.manager = one one.worksIn = four   three.secretary = two
        two.manager = one two.worksIn = three   four.secretary = two
        first(one) = Aidan
        one.last = Dunn
        four.name = Math
        three.name = CS
        age(one) = zero
        age(two) = y
    options interpret_as_algebra = true
}
