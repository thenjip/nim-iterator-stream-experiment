type
  EmptyState* = object
    discard

  SingleItemState* = object
    consumed: bool



func emptyState* (): EmptyState =
  EmptyState()



func singleItemState* (consumed: bool): SingleItemState =
  SingleItemState(consumed: consumed)


func isConsumed* (state: SingleItemState): bool =
  state.consumed
