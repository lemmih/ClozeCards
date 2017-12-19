import { FETCH_CARDS, RECEIVE_CARDS } from "../actions/cards";

export default (cards = null, action) => {
  switch (action.type) {
    case RECEIVE_CARDS:
      return action.payload.cards;
    case FETCH_CARDS:
      return "fetching";
    default:
      return cards;
  }
};
