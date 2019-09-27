## Strategy/Market Interface Model

A Strategy sees markets through an Event interface, where the market outputs new events, and an Action interface where it can place and cancel orders (inputs to the market). Markets are typed according to the currencies traded in them

The events are:

```
data TradingEv
    = PlaceEv
    | CancelEv
    | DoneEv
    | FillsEv
    | BookEv
```

The Actions are:

```
data Action
    = PlaceLimit
    | PlaceMarket
    | CancelLimit
```

**Actions are assumed to eventually succeed.** They need not execute immediately, but the exchange will eventually get the request. Hopefully, in a timely manner, but the is no hard guarantee of timeliness (best effort).

Strategies can place and cancel orders by using the corresponding Actions.

All Events and Actions that require or return an identifier for orders placed use a ClientOID rather than an exchange specific OrderID. The ClientOID is an opaque identifier outside the strategy (except for being `Hashable`, `Show` and `Eq`).

The framework *requires* the strategy to provide order placements with `ClientOID`s that are *unique within each market*. If a `ClientOID` is reused within the same market (for example: BTC-USD) of a given exchange, the behavior is undefined. A new `ClientOID` should be used for each new order placement.


### Market Event Description

1. **PlaceEv** - Notifies the strategy that an order has been placed in the market. At a minimum, specifies the `ClientOID` of the Action that succeeded.

2. **CancelEv** - Notifies the strategy that an order has been cancelled. At a minimum, specifies the `ClientOID` of the cancelled order. No more events for this order will occur.

3. **DoneEv** - Notifies the strategy that an order has been completely executed and is now closed. At a minimum, specifies the `ClientOID` of the executed order. No more events for this order will occur. DoneEv and CancelEv events are mutually exclusive and cannot both happen for the same order.

4. **FillsEv** - Notifies the strategy that an order has been filled by one of more fills. Each fill provides at least: the volume executed, the price, the `ClientOID` and the side of the order that got filled. Each event provides a (possibly empty) list of fills.

5. **BookEv** - Notifies the strategy that the market's orderboook has been updated. Although the updates may be incremental, the event returns the whole QuoteBook each time.

#### Event Sequencing

**BookEv**

`BookEv` provide no synchronization guarantee. These events may happen at any time and are not necessarily syncronized with our own order placements and cancellations. In orders words, a `BookEv` may show an order for which a `CancelEv` has already happened. It may also already show an order for which we have not yet received a `PlaceEv`.

(Ideally, `BookEv` should provide a "filtered" orderbook where all our own orders have been removed, but this is not and may never be impletemented due to exchange API limitations. Note that filtering by itself is *not* sufficient to avoid all the synchrony crazyness these events bring. The order of orderbooks may simply be flipped in time, no amount of filtering can make sense of that...)

**PlaceEv, CancelEv, DoneEv and FillsEv**

The framework provides the following guarantee of ordering:

A `PlaceEv` comes before any `FillsEv`. `FillsEv`s can happen multiple times. These, in turn, all come before either a `CancelEv` or a `DoneEv`.

That is:

1. No `FillsEv`, `CancelEv` or `DoneEv` will happen before the corresponding `PlaceEv`
2. The framework guarantees that all `FillsEv` events that happened to a cancelled order will be sent to the strategy *before* the final `CancelEv`.
3. Similarly, after all `FillsEv` for a fully executed order are sent, the strategy will *eventually* get a `DoneEv`. (There's not timeliness guaranteed here, the `DoneEv` may be delayed for hours)

If an order is fully executed `CancelEv` will never happen.
If an order is    cancelled    `DoneEv`  will never happen.

**The strategy will only receive events corresponding to orders for which the framework received a `ClientOID`**. The framework receives notifications from the exchange in terms of OrderIDs and it must know which OrderID relates to which ClientOID to be able to generate events for the strategy. This means, that if no corresponding `ClientOID` is found for a given OrderID, the notification from the exchange is discarded and no event generated.

To avoid space leaks, the framework will discard the mapping between `ClientOID`s and the exchanges own `OrderID`s once it dispatches a `CancelEv` or an `DoneEv`. So, all cancellation requests made to those `ClientOID` will be ignored from then on.


**Seemingly Inconsistent Events**

The framework tries to present to trading strategies a view of the world that makes sense, but this only goes as far as the guarantees mentioned above. Otherwise, the strategy needs to do its best to make sense of the information provided by the exchanges. There are many circumstances that will seem weird.

For example: The frameworks makes no assurance that every single change to the orderbook will be shown to the trading strategy or that those will be shown in order. If the strategy places an order, but the order only executes at a much worse price than expected, the framework will not generate orderbook events to try to justify the difference in price for the executed order. The strategy just has to figure out by itself that the market slipped before its order was executed.


### Market Action Description

1. **PlaceLimit** - Requests that a limit order be placed on the market. At a minimum, specifies a `ClientOID`, side, price and volume.
2. **PlaceMarket** - Requests that a market order be placed on the market. At a minimum, specifies a `ClientOID`, side and volume.
2. **CancelLimit** - Requests that an order be cancelled. At a minimum, specifies the `ClientOID`.

#### Action Sequencing

The strategy is free to place actions in any order. However, if the strategy requests cancellation of an order for a ClientOID that the framework doesn't yet know, the results are undefined. It is guaranteed that the framework knows about a `ClientOID` after a corresponding `PlaceEv` has been issued.


## Change Log

(UPDATE 2019-01-29: I decided to add the `DoneEv` because either framework had to keep track of how much was executed in each order and then delete
orderID <-> ClientOID mappings once the orders were fully executed or it had to be informed the order was done by somebody. If we didn't track this, we would have a memory leak due to keeping track of mappings for fully executed orders forever. So, either the strategy had to keep track of this and tell the framework - which seems backwards - or the framework could do it itself. If the framework is going to keep track of it, then it might as well provide this event, so that the strategy doesn't have to do it also! In short, someone has to do it, so it is best if the framework does it and frees up the strategy. Furthermore, for other exchanges this may be provided without any work.)

(UPDATE 2019-05-23: I removed `Nothing` from the space of valid `ClientOID`s because:

- *It simplifies the interface description*
- There were many valid interpretations for what a connector should do with a `Nothing` as a `ClientOID`. And those interpretations would be different in connectors for different exchanges.
- It will simplify some already implemented connectors
- It will simplify the implementation of some new connectors
- It will make strategy implementation only slightly more complicated
- It will actually slighty simplify some already implemented strategies

)

(UPDATE 2019-09-27: Finally had to add them. Market order are in! Yay! :-D )