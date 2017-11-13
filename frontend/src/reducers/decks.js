import { RECEIVE_DECK } from '../actions/decks'
import { Map } from 'immutable'

export default function (state=Map(), action) {
  switch (action.type) {
    case RECEIVE_DECK:
      return state.set(action.payload.id, action.payload);
    default:
      return state;
  }
}
