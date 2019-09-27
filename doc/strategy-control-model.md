### Strategy Control Interface

We now define the control interface for the strategies.

We need an `Error` Action on this interface. If there is failure from which a strategy cannot recover, it should send this Action to let a human know the situation it found itself in. This message does **not** cause a shutdown. It just signals the problem. 

We also need a new `ShutdownDone` message to let the framework know the strategy has completed its shutdown. The framework should not expect anything else from the strategy after receiving the `ShutdownDone` control action. From the point of view of the strategy this action is fail-proof and will *eventually* succeed.

Here's the definition:

```
data ControlAction
    = ShutdownDone Int
    | Error Int String
```

The extra parameters needed allow the strategy to pass extra information.

There is also a new mechanism to signal a shutdown *to* the strategy:

```
data ControlEv = ShutdownEv
```

All strategies that need to be notified of shutdown events should have an extra `ControlEv` input interface and an `ControlAction` output interface.

Currently, the strategies will receive a single new event on the `ControlEv` interface: A `ShutdownEv` and they can output two types of `ControlAction`s: `Error` and `ShutdownDone`.

The framework signals termination on the `ControlEv` interface. The strategy will send a single final `ShutdownDone` once it is done and it may issue multiple `Error` messages, as required, before then. The framework itself will timeout the strategy. It may start shutting down the connectors before the time out if it receives the `ShutdownDone` message.

Another way to look at this extra "control" interface, is that it receives auxiliary information from outside any market. A similar interface would be necessary for any strategy that depends on "off market" events. In this case, we are using it for the shutdown event, but we might consider using similar interfaces for other changes, such as an update on the exchange rate or maybe a change in our risk tolerance, etc.

#### Pre/Post conditions for connector shutdown signalling

A strategy should only issue a `ShutdownDone` action *after* all the necessary event notifications from all markets have been received.

**A strategy may still receive events _after_ it sends the `ShutdownDone` action.** This is because those events maybe processed by different threads and already be queued up. The strategy should make sure this is not a problem. 

