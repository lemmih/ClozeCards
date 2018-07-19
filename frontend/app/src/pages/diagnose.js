import _ from "lodash";
import React, {Component} from "react";
import {withRouter, Redirect} from "react-router-dom";
import {connect} from "react-redux";
import {Container, Item, Loader, Table, Grid} from "semantic-ui-react";
import uuid from "uuid/v4";
import moment from "moment";

import DeckHeader from "../components/deck-header";
import DeckBody from "../components/deck-body";
import DeckComments from "../components/deck-comments";
import Study from "../components/deck-study";

import {receiveNotes} from "../actions/notes";
import {receiveDeck, fetchDeck} from "../actions/decks";
import {receiveContent, fetchContent} from "../actions/content";

import backend from "../backend";
import {getUser} from "../common";

// states:
//   not fetched => doesn't have key
//   fetching    => true
//   fetched     => obj
//   missing     => false
function toDiagnoseDeckProps(store, ownProps) {
  const slug = ownProps.slug;
  const deckId = store.decksBySlug.get(slug);
  const deck = deckId
    ? store.decks.get(deckId)
    : null;
  const content = deck
    ? store.content.get(deck.contentId)
    : null;
  return {bySlug: deckId, deck: deck, content: content, cards: store.cards, diagnose: store.diagnose};
}
export const DiagnoseDeck = connect(toDiagnoseDeckProps)(class DiagnoseDeck extends Component {
  constructor(props) {
    super(props);
    this.state = {
      selection: getUser().id
    };
  }

  componentDidMount() {
    if (_.isUndefined(this.props.bySlug)) {
      backend.relay(fetchDeck(this.props.slug));
    }
    if (_.isPlainObject(this.props.deck)) {
      if (_.isUndefined(this.props.content)) {
        backend.relay(fetchContent(this.props.deck.contentId));
      }
    }
  }
  componentDidUpdate() {
    // console.log('update', this.props.deck, this.props.content);
    if (this.props.bySlug === undefined) {
      backend.relay(fetchDeck(this.props.slug));
    }
    // console.log('update', this.props.deck, this.props.content);
    if (_.isPlainObject(this.props.deck)) {
      if (_.isUndefined(this.props.content)) {
        backend.relay(fetchContent(this.props.deck.contentId));
      }
    }
  }

  studyQuitLink = () => {
    const deck = this.state.deck || this.props.deck;
    return "/decks/" + deck.slugs[0] + "/";
  };

  render() {
    const {content, diagnose} = this.props;
    var { cards } = this.props;
    const deck = this.state.deck || this.props.deck;
    const hasDeck = _.isPlainObject(this.props.deck);
    const hasContent = _.isPlainObject(content);
    if (this.props.bySlug === "failed")
      return <Redirect to="/"/>;
    if (!hasDeck)
      return <Loader active="active"/>;

    if(_.isArray(cards) && cards.length > 0) {
      if(_.isArray(diagnose) && diagnose.length > 0) {
        if(cards[0].word === diagnose[0].word) {
          cards = _.clone(cards);
          cards.shift();
        }
      }
    }
    var totalDelta = 0;
    if(_.isArray(diagnose)) {
      for(var i=0;i<diagnose.length;i++) {
        totalDelta += diagnose[i].delta;
      }
    }

    return (<div className="studying">
      <h1>
        <center>{deck.title}</center>
      </h1>
      <h3>
        <center>Session delta: {moment.duration(totalDelta, 'seconds').humanize()}</center>
      </h3>

      <Grid centered="centered" columns={2}>
        <Grid.Column>
          <Table basic='very' celled="celled">
            <Table.Header>
              <Table.Row>
                <Table.HeaderCell>Word</Table.HeaderCell>
                <Table.HeaderCell>Recall delay</Table.HeaderCell>
                <Table.HeaderCell>Next review</Table.HeaderCell>
                <Table.HeaderCell>Stability</Table.HeaderCell>
              </Table.Row>
            </Table.Header>

            <Table.Body>
              {/*_.isArray(cards) && _.clone(cards).reverse().map(card => <DiagnoseCard card={card}/>) */}
              {_.isArray(diagnose) && diagnose.map(entry => <DiagnoseEntry entry={entry}/>)}

            </Table.Body>
          </Table>
        </Grid.Column>
      </Grid>

      <Study studyQuitLink={this.studyQuitLink()} deckId={deck.id}/>
    </div>);
  }
});

class DiagnoseEntry extends Component {
  render() {
    const {entry} = this.props;
    const interval = entry.interval
      ? moment.duration(entry.interval, 'seconds').humanize()
      : 'new';
    //const next = moment.duration(moment(entry.review).diff(entry.seen)).humanize();
    const next = moment(entry.review).fromNow();
    const stability = moment.duration(entry.stability, 'seconds').humanize();
    const delta = moment.duration(entry.delta, 'seconds').humanize();
    const deltaTxt = entry.delta > 0
      ? '+' + delta
      : '-' + delta;
    return (<Table.Row positive={entry.delta > 0} negative={entry.delta < 0} key={entry.word}>
      <Table.Cell textAlign='center'>{entry.word}</Table.Cell>
      <Table.Cell>{interval}</Table.Cell>
      <Table.Cell>{next}</Table.Cell>
      <Table.Cell>{stability} ({deltaTxt})</Table.Cell>
    </Table.Row>);
  }
}

class DiagnoseCard extends Component {
  render() {
    const {card} = this.props;
    return (<Table.Row key={card.word}>
      <Table.Cell>{card.word}</Table.Cell>
      <Table.Cell>{card.seen && moment.duration(moment(card.seen).diff()).humanize()}</Table.Cell>
      <Table.Cell></Table.Cell>
      <Table.Cell></Table.Cell>
    </Table.Row>);
  }
}
