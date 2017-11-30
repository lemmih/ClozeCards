import _ from "lodash";
import { RECEIVE_DECK, FETCH_DECK, UNUSED_SLUG } from "../actions/decks";
import { Map } from "immutable";

export default function(state = Map(), action) {
  switch (action.type) {
    // associate by slug and by id.
    case RECEIVE_DECK:
      var s = state;
      _.forEach(action.payload.slugs, slug => {
        s = s.set(slug, action.payload.id);
      });
      return s.set(action.payload.id, action.payload.id);
    case FETCH_DECK:
      return state.set(action.payload, "fetching");
    case UNUSED_SLUG:
      return state.set(action.payload, "failed");
    default:
      return state;
  }
}
