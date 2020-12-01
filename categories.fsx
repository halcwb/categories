
type Minimum = | MinIncl of float | MinExcl of float
type Maximum = | MaxIncl of float | MaxExcl of float

type MinMax = { Min : Minimum option; Max : Maximum option}

type GenderCategory =
     | Male 
     | Female 
type AgeCategory = MinMax 
type GestationAgeCategory = MinMax 
type PostConceptionalAgeCategory = MinMax 
type WeightCategory = MinMax 

type PatientCategory =
    | RootCategory
    | Gender of GenderCategory 
    | Age of AgeCategory
    | GestationAge of GestationAgeCategory
    | PostConceptionalAge of PostConceptionalAgeCategory
    | Weight of WeightCategory


type Category =
    | Category of PatientCategory * Category list


let createCategory xs c = (c, xs |> List.distinct) |> Category 


let initCategory = RootCategory |> createCategory []


let findCategory pc cat =
    let rec find pc (Category(c, xs)) =
        if pc = c then Category(c, xs) |> Some
        else
            xs
            |> List.fold(fun acc cat ->
                if acc |> Option.isSome then acc
                else find pc cat
            ) None

    find pc cat


let getCategoryChildren (Category(_, xs)) = xs
let getCategoryPatient (Category(c, _)) = c


let findParent pc cat =
    let rec find pc (Category(parent, xs)) cat =
        if pc = (cat |> getCategoryPatient) then 
            Category(parent, xs) |> Some
        else
            cat
            |> getCategoryChildren
            |> List.fold(fun acc child ->
                if acc |> Option.isSome then acc
                else find pc cat child 
            ) None

    cat
    |> getCategoryChildren
    |> List.fold (fun acc child ->
        if acc |> Option.isSome then acc
        else find pc cat child
    ) None
    


let addCategories cats pc cat =
    let rec add cats pc (Category(x, xs)) =
        if pc = x then cats |> List.append xs else xs 
        |> fun xs ->
            if xs |> List.isEmpty then x |> createCategory xs
            else 
                xs
                |> List.map (add cats pc)
                |> fun xs -> x |> createCategory xs

    add cats pc cat


let replaceCategory newCat oldCat cat =
    let rec repl newCat oldCat (Category(x, xs)) =
        if oldCat = x then newCat else x 
        |> fun x ->
            if xs |> List.isEmpty then x |> createCategory xs
            else 
                xs
                |> List.map (repl newCat oldCat )
                |> fun xs -> x |> createCategory xs

    repl newCat oldCat cat


let addGenderCategory pc cat =
    [ 
        Male   |> Gender
        Female |> Gender  
    ] 
    |> List.map (createCategory [])
    |> fun cats -> cat |> addCategories cats pc


let minMax = { Min = None ; Max = None }

let createMin b f = if b then f |> MinIncl else f |> MinExcl
let createMax b f = if b then f |> MaxIncl else f |> MaxExcl

let setMin min mm = { mm with Min = Some min }
let setMax max mm = { mm with Max = Some max }


let inline setMinMaxValue c s b f mm = 
    f 
    |> c b
    |> fun x -> mm |> s x

let setMinIncl = setMinMaxValue createMin setMin true 
let setMinExcl = setMinMaxValue createMin setMin false 
let setMaxIncl = setMinMaxValue createMax setMax true 
let setMaxExcl = setMinMaxValue createMax setMax false 


let minToMax min =
    match min with
    | MinIncl v -> MaxExcl v
    | MinExcl v -> MaxIncl v


let maxToMin max =
    match max with
    | MaxIncl v -> MinExcl v
    | MaxExcl v -> MinIncl v


let evalMinMax mm =
    match mm.Min, mm.Max with
    | None, None     -> [ mm ]
    | Some min, None ->
        [
            { minMax with Max = minToMax min |> Some }
            mm
        ]
    | None, Some max ->
        [
            mm
            { minMax with Min = maxToMin max |> Some }
        ]
    | Some min, Some max ->
        [
            { minMax with Max = minToMax min |> Some }
            mm
            { minMax with Min = maxToMin max |> Some }
        ]


let splitMinMax f b mm =
    [
        if b then mm |> setMaxIncl f else mm |> setMaxExcl f
        if b then mm |> setMinExcl f else mm |> setMinIncl f
    ]


let addMinMaxCategory c mm pc cat =
    mm
    |> evalMinMax
    |> List.map (c >> (createCategory []))
    |> fun cats -> cat |> addCategories cats pc


let addAgeCategory = addMinMaxCategory Age
let addGestAgeCategory = addMinMaxCategory GestationAge
let addPostAgeCategory = addMinMaxCategory PostConceptionalAge

let splitMinMaxCategory c pc f b cat =
    cat 
    |> findCategory pc
    |> function
    | None -> cat
    | Some (Category(oldpc, _)) -> 
        match oldpc with
        | Age mm -> Age, mm
        |> fun (c, mm) ->
            mm
            |> splitMinMax f b
            |> List.map (c >> (createCategory []))
        

// testing
initCategory
|> addGenderCategory RootCategory
|> addAgeCategory (minMax |> setMinIncl 10.) (Male |> Gender)

initCategory
|> addAgeCategory (minMax |> setMinIncl 1. |> setMaxExcl 12.) RootCategory
|> addGenderCategory (minMax |> setMinIncl 1. |> setMaxExcl 12. |> Age)
|> findParent (Male |> Gender)

initCategory
|> addAgeCategory (minMax |> setMaxExcl 1.) RootCategory
|> addGestAgeCategory (minMax |> setMinIncl 28. |> setMaxIncl 37.) (minMax |> setMaxExcl 1. |> Age)
