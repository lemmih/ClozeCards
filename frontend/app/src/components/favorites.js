import _ from "lodash";
import React, { PureComponent } from "react";
import { connect } from "react-redux";
import { Label, Icon } from "semantic-ui-react";

import { setFavorite, unsetFavorite } from "../actions/user";

import backend from "../backend";

const favoriteStyle = {
  position: "relative",
  paddingLeft: "1em"
};

// user
// nFavorites
// deckId
class Favorites extends PureComponent {
  handleClick = () => {
    const { user, deckId } = this.props;
    const isFavorite = user.favorites.has(deckId);
    const action = isFavorite ? unsetFavorite(deckId) : setFavorite(deckId);
    backend.relay(action);
  };
  render = () => {
    const { user, deckId, nFavorites } = this.props;
    const isFavorite = user.favorites.has(deckId);
    const name = isFavorite ? "heart" : "empty heart";

    return (
      <a onClick={this.handleClick}>
        <span style={favoriteStyle}>
          <Icon name={name} size="big" />
          <Label circular color="grey" floating>
            {nFavorites}
          </Label>
        </span>
      </a>
    );
  };
}
export default connect(store => {
  return { user: store.user };
})(Favorites);
