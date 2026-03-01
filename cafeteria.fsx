//Drinks
type Size = Small | Medium | Large

type CoffeeType = Espresso | Latte | Cappuccino | Americano | Filter
type TeaType = Green | Black | Herbal | Oolong | Rooibus
type JuiceType = Orange | Apple | Multifruit | Exotic
type MilkType = Skummet | Mini | Let
type SodaType = Fanta | Pepsi | Sprite | FaxeKondi

type DrinkInfo = { 
    Size: Size 
}

type Drink = 
    | Coffee of CoffeeType * DrinkInfo
    | Tea of TeaType * DrinkInfo
    | Juice of JuiceType //No sizes as they come in bottles
    | Milk of MilkType //No sizes as they come in bottles
    | Soda of SodaType //No sizes as they come in bottles

//Food
type Food = Sandwich | Pastry | Salad
type Fruit = Grapes | Banana | Pear


let getDrinkBasePrice drink =
    match drink with
    | Coffee (variety, _) -> 
        match variety with
        | Espresso   -> 15.0m
        | Latte      -> 25.0m
        | Cappuccino -> 25.0m 
        | Americano  -> 20.0m 
        | Filter     -> 12.0m

    | Tea (variety, _) ->
        match variety with
        | Green    -> 15.0m
        | Black    -> 15.0m
        | Herbal   -> 14.0m
        | Oolong   -> 18.0m
        | Rooibus  -> 13.0m

    | Juice variety ->
        match variety with
        | Orange     -> 15.0m
        | Apple      -> 15.0m
        | Multifruit -> 18.0m
        | Exotic     -> 18.0m

    | Milk variety ->
        match variety with
        | Skummet -> 10.0m
        | Mini    -> 10.0m
        | Let     -> 10.0m

    | Soda variety ->
        match variety with
        | Fanta     -> 15.0m
        | Pepsi     -> 20.0m
        | Sprite    -> 18.0m
        | FaxeKondi -> 15.0m

let applyDrinkSize size price =
    match size with
    | Small  -> price
    | Medium -> price + 5.0m
    | Large  -> price + 10.0m

let calculateDrinkPrice (drink: Drink) =
    match drink with
    | Coffee (_, info)
    | Tea (_, info) ->
        getDrinkBasePrice drink
        |> applyDrinkSize info.Size

    | Juice _
    | Milk _
    | Soda _ ->
        getDrinkBasePrice drink


let calculateFoodPrice food =
    match food with
    | Sandwich -> 45.0m
    | Pastry   -> 20.0m
    | Salad    -> 35.0m

let calculateFruitPrice fruit =
    match fruit with
    | Grapes | Banana | Pear -> 5.0m



let myCoffeeOrder = Coffee(Latte, { Size = Large })
let myMilkOrder   = Milk(Skummet)

let coffeePrice = myCoffeeOrder |> calculateDrinkPrice
let milkPrice   = myMilkOrder   |> calculateDrinkPrice

printfn "Coffee: %A - Price: %M DKK" myCoffeeOrder coffeePrice
printfn "Milk: %A - Price: %M DKK" myMilkOrder milkPrice