open System

type Size = Small | Medium | Large
type CoffeeVariety = Espresso | Latte | Cappuccino
type MilkType = Skummet | Mini | Let | OatMilk | SoyMilk

type PaymentMethod = 
    | ViaCard 
    | CreditCard 
    | MobilePay 
    | Cash

type Customer = {
    Id: int
    Name: string
    Email: string
}

type CoffeeRecord = { 
    Variety: CoffeeVariety 
    Size: Size 
    Milk: MilkType 
}

type JuiceRecord = { 
    Flavor: string 
    IsIceCold: bool 
}

type MilkRecord = {
    Type: MilkType
    Size: Size
}

type Drink = 
    | Coffee of CoffeeRecord
    | Juice of JuiceRecord
    | Milk of MilkRecord 

let getBasePrice drink =
    match drink with
    | Coffee c ->
        match c.Variety with
        | Espresso -> 15.0
        | Latte | Cappuccino -> 25.0
    | Juice _ -> 18.0
    | Milk m -> 
        match m.Type with
        | OatMilk | SoyMilk -> 15.0
        | _ -> 10.0

let applySizePremium size price =
    match size with
    | Small -> price
    | Medium -> price + 5.0
    | Large -> price + 10.0

let applyMilkPremium milk price =
    match milk with
    | OatMilk | SoyMilk -> price + 7.0
    | _ -> price

let calculateDrinkPrice drink =
    match drink with
    | Coffee c -> 
        getBasePrice drink 
        |> applySizePremium c.Size 
        |> applyMilkPremium c.Milk
    | Juice _ -> 
        getBasePrice drink
    | Milk m -> 
        getBasePrice drink 
        |> applySizePremium m.Size 

let gtgVAT n x = x + (x * float n / 100.0)

type Order = {
    Customer: Customer
    Drink: Drink
    Quantity: int
    Payment: PaymentMethod
}

type OrderMsg = 
    | ProcessOrder of Order
    | LeaveComment of string

let gtgAgent = MailboxProcessor<OrderMsg>.Start(fun inbox ->
    let rec loop () = async {
        let! msg = inbox.Receive()
        
        match msg with
        | ProcessOrder order ->
            let pricePerUnit = calculateDrinkPrice order.Drink
            let finalPrice = gtgVAT 25 pricePerUnit
            let total = finalPrice * float order.Quantity
            
            printfn "--- New Order ---"
            printfn "Customer: %s (ID: %d)" order.Customer.Name order.Customer.Id
            
            match order.Drink with
            | Coffee c -> 
                printfn "Item: Coffee { Variety = %A; Size = %A; Milk = %A }" c.Variety c.Size c.Milk
            | Juice j -> 
                printfn "Item: Juice { Flavor = %s; IsIceCold = %b }" j.Flavor j.IsIceCold
            | Milk m -> 
                printfn "Item: Milk { Type = %A; Size = %A }" m.Type m.Size

            printfn "Payment Method: %A" order.Payment
            printfn "Total Amount: DKK %.2f" total
            printfn "-----------------"

        | LeaveComment c -> 
            printfn "Feedback received: %s" c
            
        return! loop ()
    }
    loop ()
)

let studentUser = { Id = 101; Name = "Anders"; Email = "and@via.dk" }

let oatLatte = {
    Customer = studentUser
    Drink = Coffee { Variety = Latte; Size = Large; Milk = OatMilk }
    Quantity = 1
    Payment = MobilePay
}

let plainOatMilk = {
    Customer = studentUser
    Drink = Milk { Type = OatMilk; Size = Large }
    Quantity = 1
    Payment = ViaCard
}

gtgAgent.Post(ProcessOrder(oatLatte))
gtgAgent.Post(ProcessOrder(plainOatMilk))

System.Threading.Thread.Sleep(1000)