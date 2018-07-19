import _ from "lodash";
import React, { Component } from "react";
import { connect } from "react-redux";
import {
  Input,
  Item,
  Label,
  Icon,
  Segment,
  Popup,
  Dropdown,
  Grid
} from "semantic-ui-react";
import { Link } from "react-router-dom";
import moment from "moment";

import { setVisibility } from "../actions/decks";
import Favorites from "./favorites";

import backend from "../backend";

const opt = val => {
  return { key: val, text: val, value: val };
};

const defaultState = props => {
  return {
    options: [
      opt("Audio"),
      opt("Comprehensive"),
      opt("Exam"),
      opt("Audio Book"),
      opt("Free"),
      opt("Non-Free"),
      opt("Mandarin Companion"),
      opt("Graded Reader"),
      opt("Slow Chinese")
    ]
  };
};
// editable
// mayEdit
class DeckHeader extends Component {
  constructor(props) {
    super(props);
    this.state = defaultState(props);
  }

  getDeck = () => {
    return Object.assign(
      { type: "word-list", title: "", tags: [] },
      this.props.deck
    );
  };

  handleAddition = (e, { value }) => {
    this.setState({
      options: [{ text: value, value }, ...this.state.options]
    });
  };
  handleChange = (e, { value }) => {
    this.props.onChange({
      ...this.getDeck(),
      tags: value
    });
  };
  handleChangeTitle = (e, { value }) => {
    this.props.onChange({
      ...this.getDeck(),
      title: value
    });
  };

  toggleVisibility = () => {
    const { deck } = this.props;
    backend.relay(setVisibility(deck.id, !deck.hidden));
  };

  render = () => {
    const { editable, user, deck } = this.props;
    const { title, tags } = this.props.deck;
    const slug =
      _.isArray(deck.slugs) && deck.slugs.length > 0 ? deck.slugs[0] : null;
    const deckLink = "/decks/" + slug;

    const mayEdit = deck.owner === user.id && this.props.mayEdit;

    const studyBase = (
      <Popup
        content="Study"
        position="top center"
        trigger={<Icon name="student" size="big" />}
      />
    );
    const studyActive = <Link to={deckLink + "/study"}>{studyBase}</Link>;

    // const notesBase = (
    //   <Popup
    //     content="Notes"
    //     position="top center"
    //     trigger={<Icon name="edit" size="big" />}
    //   />
    // );
    // const notesActive = <Link to={deckLink + "/notes"}>{notesBase}</Link>;

    const diagnoseBase = (
      <Popup
        content="Diagnostic"
        position="top center"
        trigger={<Icon name="list alternate outline" size="big" />}
      />
    );
    const diagnoseActive = <Link to={deckLink + "/diagnose"}>{diagnoseBase}</Link>;

    const editBase = (
      <Popup
        content="Edit deck"
        position="top center"
        trigger={<Icon name="write" size="big" />}
      />
    );
    const editActive = <a onClick={this.props.onEdit}>{editBase}</a>;

    const hideBase = (
      <Popup
        content="Hide deck"
        position="top center"
        trigger={<Icon style={{ marginLeft: "1em" }} name="hide" size="big" />}
      />
    );
    const unhideBase = (
      <Popup
        content="Unhide deck"
        position="top center"
        trigger={
          <Icon style={{ marginLeft: "1em" }} name="unhide" size="big" />
        }
      />
    );
    const visibilityActive = (
      <a onClick={this.toggleVisibility}>
        {deck.hidden ? unhideBase : hideBase}
      </a>
    );

    return (
      <Item className="deck-item">
        <Item.Image className="ui">
          <Segment style={{ background: "cornsilk" }}>
            <Label style={{ textAlign: "center" }} attached="bottom">
              Word List
            </Label>
            <Icon
              name="file word outline"
              size="huge"
              style={{
                display: "block",
                marginLeft: "auto",
                marginRight: "auto"
              }}
            />
          </Segment>
        </Item.Image>

        <Item.Content verticalAlign="middle">
          {editable && (
            <Item.Header
              style={{ display: "flex" }}
              as={Input}
              onChange={this.handleChangeTitle}
              value={title}
              transparent={true}
              fluid={true}
              placeholder="Title..."
            />
          )}
          {!editable && (
            <Item.Header>
              <Link to={deckLink}>{title}</Link>
            </Item.Header>
          )}
          <Item.Meta>
            {editable ? (
              <Dropdown
                options={this.state.options}
                placeholder="Add tags"
                search
                selection
                fluid
                allowAdditions
                multiple
                value={tags}
                onAddItem={this.handleAddition}
                onChange={this.handleChange}
              />
            ) : (
              <div>
                <span>Tags</span>
                {deck.tags.map(tag => <span key={tag}>{tag}</span>)}
              </div>
            )}
            {moment(deck.createdAt).calendar()}
          </Item.Meta>
          <Item.Extra>
            <Grid columns={3}>
              <Grid.Row>
                <Grid.Column>
                  {editable ? studyBase : studyActive}
                  {editable ? diagnoseBase : diagnoseActive}
                </Grid.Column>

                {mayEdit ? (
                  <Grid.Column textAlign="center">
                    {editActive}
                    {visibilityActive}
                  </Grid.Column>
                ) : (
                  <Grid.Column textAlign="center" />
                )}

                <Grid.Column textAlign="right">
                  {!editable && (
                    <Favorites nFavorites={deck.nLikes} deckId={deck.id} />
                  )}
                  {/*!editable && (
                    <a>
                      <span
                        style={{ position: "relative", paddingLeft: "1em" }}
                      >
                        <Icon name="comments" size="big" />
                        <Label circular color="grey" floating>
                          {deck.nComments}
                        </Label>
                      </span>
                    </a>
                  )*/}

                  {editable && (
                    <span style={{ position: "relative", paddingLeft: "1em" }}>
                      <Icon name="empty heart" size="big" />
                    </span>
                  )}
                  {/*editable && (
                    <span style={{ position: "relative", paddingLeft: "1em" }}>
                      <Icon name="comments" size="big" />
                    </span>
                  )*/}
                </Grid.Column>
              </Grid.Row>
            </Grid>
          </Item.Extra>
        </Item.Content>
      </Item>
    );
  };
}

export default connect(store => {
  return { user: store.user };
})(DeckHeader);
