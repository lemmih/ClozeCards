import _ from "lodash";
import React, { Component } from "react";
import { connect } from "react-redux";
import { Button, Grid, Loader, Modal } from "semantic-ui-react";

import { fetchCards, receiveCards, receiveResponse } from "../actions/cards";
import backend from "../backend";

import ClozeSentence from "./cloze-sentence";

const containerStyle = {
  position: "fixed",
  bottom: 0,
  width: "100%",
  background: "aliceblue"
};
const headerRow = {
  background: "#ccc",
  borderTop: "1px solid black",
  padding: 0
};
const controlRow = {};
const exerciseRow = {
  borderTop: "1px solid black",
  minHeight: "8em",
  fontSize: "150%",
  paddingTop: "0"
};

function toStudyProps(store) {
  return {
    cards: store.cards
  };
}
export default connect(toStudyProps)(
  class Study extends Component {
    constructor(props) {
      super(props);
      this.state = {
        active: 0,
        style: "study",
        showStatus: false,
        showPinyin: false,
        showEnglish: false
      };
    }

    componentDidMount = () => {
      if (_.isNull(this.props.cards)) {
        backend.relay(fetchCards(this.props.deckId, this.state.style));
      }
    };
    componentDidUpdate = (prevProps, prevState) => {
      if (_.isNull(this.props.cards) || this.state.style !== prevState.style) {
        backend.relay(fetchCards(this.props.deckId, this.state.style));
      }

      const active = this.getActive();

      const ready = _.isArray(this.props.cards);
      const done = ready && this.props.cards[0].chinese.length <= active;
      if (done && this.continueRef) {
        this.continueRef.focus();
      }
      if (_.isArray(this.props.cards) && !_.isArray(prevProps.cards)) {
        this.setState({ showPinyin: false, showEnglish: false, active: 0 });
      }
    };

    onSpace = value => {
      const { cards } = this.props;
      const block = cards[0].chinese[this.getActive()];
      const isCorrect = _.includes(
        block.answers,
        value.replace(" ", "").toLowerCase()
      );
      if (isCorrect) this.onAnswer(value);
      return isCorrect;
    };
    onAnswer = value => {
      const { cards } = this.props;
      const card = cards[0];
      const block = card.chinese[this.getActive()];
      const isCorrect = _.includes(
        block.answers,
        value.replace(" ", "").toLowerCase()
      );
      const response = {
        word: block.simplified,
        sentenceId: card.sentenceId,
        completed: isCorrect,
        value: value,
        shownAnswer: this.state.showEnglish,
        factor: 3
      };
      backend.relay(receiveResponse(response));
      if (isCorrect) {
        let i = this.getActive() + 1;
        this.setState({ active: i, showPinyin: false });
        i = this.getActive(i);
        if (i >= card.chinese.length) {
          this.playAudio();
        }
      }
      return isCorrect;
    };
    onEscape = () => {
      console.log("Set shown answer");
      this.setState(state => {
        return { showEnglish: true, showPinyin: state.showEnglish };
      });
      return false;
    };
    onSwitchStyle = style => () => {
      this.setState({ style });
    };
    onCloseStatus = () => {
      this.setState({ showStatus: false });
    };
    onContinue = () => {
      const { cards } = this.props;
      if (cards.length === 1) {
        this.setState({ showStatus: false, active: 0 }); // Show Status FIXME
        backend.relay(fetchCards(this.props.deckId, this.state.style));
      } else {
        const nextCard = cards[1];
        let i = 0;
        while (
          i < nextCard.chinese.length &&
          (_.isString(nextCard.chinese[i]) || !nextCard.chinese[i].isGap)
        )
          i++;
        this.setState({ active: i });
        this.props.dispatch(receiveCards(_.slice(cards, 1)));
      }
    };
    getActive = i => {
      const { cards } = this.props;
      const valid = i =>
        i < cards[0].chinese.length &&
        _.isPlainObject(cards[0].chinese[i]) &&
        cards[0].chinese[i].isGap;
      i = i || this.state.active;
      if (_.isArray(cards) && cards.length > 0 && !valid(i)) {
        const nextCard = cards[0];
        while (i < nextCard.chinese.length && !valid(i)) i++;
        return i;
      } else {
        return i;
      }
    };
    handleContinueRef = continueRef => {
      this.continueRef = continueRef;
    };
    playAudio = () => {
      const a = new Audio();
      const { cards } = this.props;
      const sid = cards[0].sentenceId;
      a.src = "/static/audio/sentences/" + sid + ".mp3";
      a.play();
    };
    render = () => {
      const { style, showStatus, showPinyin, showEnglish } = this.state;
      const { cards } = this.props;
      const active = this.getActive();

      const ready = _.isArray(cards);
      const done = ready && cards[0].chinese.length <= active;
      const sid = ready && cards[0].sentenceId;

      return (
        <div style={containerStyle}>
          <Modal open={showStatus} onClose={this.onCloseStatus}>
            <Modal.Content>Content</Modal.Content>
          </Modal>
          <Grid relaxed>
            <Grid.Row columns={1} textAlign="center" style={headerRow}>
              <Grid.Column>
                High score | When word will be reviewed.
              </Grid.Column>
            </Grid.Row>
            <Grid.Row columns={1} textAlign="center" style={exerciseRow}>
              <Grid.Column>
                {ready ? (
                  <ClozeSentence
                    onAnswer={this.onAnswer}
                    onSpace={this.onSpace}
                    onEscape={this.onEscape}
                    active={active}
                    showPinyin={showPinyin}
                    showEnglish={showEnglish || done}
                    blocks={cards[0].chinese}
                    english={cards[0].english}
                  />
                ) : (
                  <Loader active />
                )}
              </Grid.Column>
            </Grid.Row>
            <Grid.Row columns={3} style={controlRow}>
              <Grid.Column>
                <Button negative circular icon="power" />
                <Button circular icon="settings" />
                <Button
                  circular
                  disabled={!ready}
                  icon="external"
                  target="_blank"
                  as={"a"}
                  href={"https://tatoeba.org/eng/sentences/show/" + sid}
                />
                <Button
                  circular
                  disabled={!done}
                  icon="play"
                  onClick={this.playAudio}
                />
              </Grid.Column>
              <Grid.Column textAlign="center">
                <Button.Group toggle>
                  <Button
                    onClick={this.onSwitchStyle("study")}
                    active={style === "study"}
                  >
                    Study
                  </Button>
                  <Button.Or />
                  <Button
                    onClick={this.onSwitchStyle("review")}
                    active={style === "review"}
                  >
                    Review
                  </Button>
                </Button.Group>
              </Grid.Column>
              <Grid.Column textAlign="right">
                {done ? (
                  <Button
                    ref={this.handleContinueRef}
                    onClick={this.onContinue}
                    positive
                  >
                    Continue
                  </Button>
                ) : (
                  <Button disabled={!ready} color="yellow">
                    Show hint
                  </Button>
                )}
              </Grid.Column>
            </Grid.Row>
          </Grid>
        </div>
      );
    };
  }
);
