//Drinks DU
type Size = Small | Medium | Large

type CoffeeType = Espresso | Latte | Cappuccino | Americano | Filter
type TeaType = Green | Black | Herbal | Oolong | Rooibus
type JuiceType = Orange | Apple | Multifruit | Exotic
type MilkType = Skummet | Mini | Let
type SodaType = Fanta | Pepsi | Sprite | FaxeKondi

//Record
type DrinkInfo = { 
    Size: Size 
}

//Drink DU
type Drink = 
    | Coffee of CoffeeType * DrinkInfo
    | Tea of TeaType * DrinkInfo
    | Juice of JuiceType //No sizes as they come in bottles
    | Milk of MilkType //No sizes as they come in bottles
    | Soda of SodaType //No sizes as they come in bottles

//Food DUs
type Food = Sandwich | Pastry | Salad
type Fruit = Grapes | Banana | Pear


let getDrinkBasePrice drink =
    match drink with
    | Coffee (variety, _) ->  //ignore size for base price, as it will be added later
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
//Payment DUs
type PaymentType = Cash | Card | MobilePay | ApplePay | GooglePay

//Record for customers
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
        let! msg = inbox.Receive()

        match msg with

        | OrderDrink (drink, qty) ->
            let basePrice  = calculateDrinkPrice drink
            let unitPrice  =
                match drink with
                | Coffee _ -> gtgVAT basePrice
                | _        -> float basePrice

            let totalPrice = unitPrice * float qty

            let drinkDesc =
                match drink with
                | Coffee (variety, info) ->
                    sprintf "%A coffee (%A)" variety info.Size
                | Tea (variety, info) ->
                    sprintf "%A tea (%A)" variety info.Size
                | Juice variety -> sprintf "%A juice"  variety
                | Milk  variety -> sprintf "%A milk"   variety
                | Soda  variety -> sprintf "%A soda"   variety

            printfn "Please pay DKK %.2f for your %d %s. Thanks!"
                totalPrice qty drinkDesc

        | OrderFood (food, qty) ->
            let totalPrice = calculateFoodPrice food * qty
            printfn "Please pay DKK %d for your %d %A. Enjoy your meal!"
                totalPrice qty food

        | OrderFruit (fruit, qty) ->
            let totalPrice = calculateFruitPrice fruit * qty
            printfn "Please pay DKK %d for your %d %A. Stay healthy!"
                totalPrice qty fruit

        | LeaveComment comment ->
            printfn "Thanks for your comment: \"%s\" — We really appreciate your feedback!" comment

        return! loop ()
    }
    loop ()
)


gtgAgent.Post(OrderDrink(Coffee(Latte,  { Size = Large  }), 2))

gtgAgent.Post(OrderDrink(Coffee(Espresso, { Size = Small }), 1))

gtgAgent.Post(OrderDrink(Tea(Green, { Size = Medium }), 3))

gtgAgent.Post(OrderDrink(Soda Pepsi, 1))

gtgAgent.Post(OrderFood(Sandwich, 2))

gtgAgent.Post(OrderFruit(Grapes, 3))

gtgAgent.Post(LeaveComment "The coffee was absolutely amazing today!")

System.Threading.Thread.Sleep(500)
