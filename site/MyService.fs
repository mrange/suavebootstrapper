module MyService

module Data =
  open MiniJson.JsonModule

  open System

  type CustomerId = CustomerId  of int64
  type OrderId    = OrderId     of int64

  let ReadCustomer (CustomerId id) : Async<Json> =
    async {
      return
        JsonObject
          [|
            "customerId"  , JsonString  (sprintf "%d" id)
            "isActive"    , JsonBoolean true
            "firstName"   , JsonString  "Mårten"
            "lastName"    , JsonString  "Rånge"
            "birthDate"   , JsonString  "1974"
            "openOrders"  , JsonArray
              [|
                JsonString "123"
                JsonString "456"
              |]
          |]
    }

  let ReadOrder (CustomerId customerId) (OrderId orderId) : Async<Json> =
    async {
      return
        JsonObject
          [|
            "orderId"     , JsonString  (sprintf "%d" orderId)
            "customerId"  , JsonString  (sprintf "%d" customerId)
            "isClosed"    , JsonBoolean false
            "payDate"     , JsonString  "2015-10-01"
          |]
    }

  let SetOrderAsPaid (CustomerId customerId) (OrderId orderId) (date : DateTime) : Async<unit> =
    async {
      printfn "SetOrderAsPaid: %A, %A, %A" customerId orderId date
      return ()
    }

module Parsers =
  open MiniJson.DynamicJsonModule
  open System

  let (|ParseId|_|) (jsonPath : JsonPath) =
    if jsonPath.HasValue then
      let r,v = Int64.TryParse jsonPath.AsString
      if r then Some v
      else None
    else
      None

  let (|ParseDate|_|) (jsonPath : JsonPath) =
    if jsonPath.HasValue then
      let r,v = DateTime.TryParse jsonPath.AsString
      if r then Some v
      else None
    else
      None

module WebParts =
  open Data
  open Parsers
  open System
  open WebPartT

  open MiniJson.JsonModule

  open Suave.Http.RequestErrors

  let RespondWithJsonData (a : Async<Json>) =
    FromAsync a
      >>= RespondWithJson true
    |> ToWebPart

  let GetCustomer customerId =
    ReadCustomer (CustomerId customerId)
    |> RespondWithJsonData

  let GetOrderStatus (customerId,orderId) =
    ReadOrder (CustomerId customerId) (OrderId orderId)
    |> RespondWithJsonData

  let PostOrderStatus =
    wpt {
      let! _, q = ReceiveJson true
      match q?customerId, q?orderId, q?paidDate with
      | (ParseId customerId, ParseId orderId, ParseDate date) ->
        do! FromAsync (SetOrderAsPaid (CustomerId customerId) (OrderId orderId) date)
        return! RespondWithText "text/plain" "Successfully updated OrderStatus"
      | _ ->
        return! FailWith (BAD_REQUEST "Invalid OrderStatus data")
    } |> ToWebPart

open Suave.Http
open Suave.Http.Applicatives
open Suave.Http.RequestErrors
open Suave.Types

let App : WebPart =
  choose
    [
      GET   >>= pathScan  "/Customer/%u"        WebParts.GetCustomer
      GET   >>= pathScan  "/OrderStatus/%u/%u"  WebParts.GetOrderStatus
      POST  >>= path      "/OrderStatus"        >>= WebParts.PostOrderStatus
      NOT_FOUND "Invalid request"
    ]
