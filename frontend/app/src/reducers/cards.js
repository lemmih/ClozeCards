import { FETCH_CARDS, RECEIVE_CARDS } from '../actions/cards'

export default (cards=null, action) => {
  switch (action.type) {
    case RECEIVE_CARDS:
      if( action.payload.cards.length > 0)
        return action.payload.cards;
      return 'fetching';
    case FETCH_CARDS:
      return 'fetching';
    default:
      return cards;
  }
}
