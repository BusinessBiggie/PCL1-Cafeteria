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
        | Espresso   -> 15
        | Latte      -> 25
        | Cappuccino -> 25 
        | Americano  -> 20 
        | Filter     -> 12

    | Tea (variety, _) ->
        match variety with
        | Green    -> 15
        | Black    -> 15
        | Herbal   -> 14
        | Oolong   -> 18
        | Rooibus  -> 13

    | Juice variety ->
        match variety with
        | Orange     -> 15
        | Apple      -> 15
        | Multifruit -> 18
        | Exotic     -> 18

    | Milk variety ->
        match variety with
        | Skummet -> 10
        | Mini    -> 10
        | Let     -> 10

    | Soda variety ->
        match variety with
        | Fanta     -> 15
        | Pepsi     -> 20
        | Sprite    -> 18
        | FaxeKondi -> 15

let applyDrinkSize size price =
    match size with
    | Small  -> price
    | Medium -> price + 5
    | Large  -> price + 10

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
    | Sandwich -> 45
    | Pastry   -> 20
    | Salad    -> 35

let calculateFruitPrice fruit =
    match fruit with
    | Grapes | Banana | Pear -> 5



let myCoffeeOrder = Coffee(Latte, { Size = Large })
let myMilkOrder   = Milk(Skummet)

let coffeePrice = myCoffeeOrder |> calculateDrinkPrice
let milkPrice   = myMilkOrder   |> calculateDrinkPrice

printfn "Coffee: %A - Price: %int DKK" myCoffeeOrder coffeePrice
printfn "Milk: %A - Price: %int DKK" myMilkOrder milkPrice

//SPRINT 2
type PaymentType = Cash | Card | MobilePay | ApplePay | GooglePay

type Customer = {
    Id: int
    Name: string
    Email: string
    Phone: string
}

let gtgVAT (x: int) =
    let percentage = 0.25
    float x + (float x * percentage)

printfn "Price with VAT: %.2f DKK" (gtgVAT coffeePrice)

type OrderProductMsg = 
    | OrderDrink of Drink * qty:int 
    | OrderFood of Food * qty:int
    | OrderFruit of Fruit * qty:int
    | LeaveComment of string

let gtgAgent = MailboxProcessor<OrderProductMsg>.Start(fun inbox ->
    let rec loop () = async {
        //Part 3
        return ()
    }
    loop ()
)