﻿module Utilities

open OrchestrationCE
open Coordination

type EventAndState<'TState, 'TEvent> = 
    { Event : 'TEvent
      State : 'TState }

let private stateAccumulator updateState eventAndState = function
    | Some e -> { State = updateState eventAndState.State e; Event = Some e }
    | None -> { State = eventAndState.State; Event = None }

let baseStoryWithState updateState initState =
    event Some
    |> Coordination.scan (stateAccumulator updateState) ({ Event = None; State = initState })
    |> Coordination.skip 1
    |> Coordination.map (function | { Event = Some e; State = state } -> Some { Event = e; State = state } | _ -> None)

type Action<'TEvent, 'TExposition, 'TInteractive> =
    | Exposition of 'TExposition * ('TEvent option)
    | Interactive of 'TInteractive * 'TEvent

module Action =
    let mapEvent f = function
        | Exposition (msg, event) -> Exposition (msg, event |> Option.map f)
        | Interactive (msg, event) -> Interactive (msg, f event)

    let mapExposition f = function
        | Exposition (msg, event) -> Exposition (f msg, event)
        | Interactive (msg, event) -> Interactive (msg, event)

    let mapInteractive f = function
        | Interactive (msg, event) -> Interactive (f msg, event)
        | Exposition (msg, event) -> Exposition (msg, event)