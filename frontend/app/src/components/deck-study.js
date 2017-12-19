import _ from "lodash";
import React, { Component, PureComponent } from "react";
import { connect } from "react-redux";
import {
  Button,
  Grid,
  Loader,
  Modal,
  Popup,
  Header,
  Icon
} from "semantic-ui-react";
import { Link } from "react-router-dom";

import {
  fetchCards,
  receiveCards,
  receiveResponse,
  clearCards
} from "../actions/cards";
import { setFavorite } from "../actions/user";
import backend from "../backend";

import ClozeSentence from "./cloze-sentence";

const containerStyle = {
  position: "fixed",
  bottom: 0,
  width: "100%",
  background: "aliceblue",
  zIndex: 1001
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

function toStudyProps(store, props) {
  const { deckId } = props;
  const isFavorite = store.user.favorites.has(deckId);
  return {
    isFavorite: isFavorite,
    isRegistered: !_.isNull(store.user.email),
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
        showEnglish: false,
        showPlaceholder: false,
        mode: "rocket",
        type: "keyboard"
      };
    }

    componentDidMount = () => {
      const { cards, isFavorite, isRegistered, deckId } = this.props;
      if (_.isNull(cards)) {
        backend.relay(fetchCards(deckId, this.state.style));
      }
      this.setAudioState();
      if (!isFavorite && isRegistered) backend.relay(setFavorite(deckId));
    };
    componentDidUpdate = (prevProps, prevState) => {
      if (_.isNull(this.props.cards) || this.state.style !== prevState.style) {
        backend.relay(fetchCards(this.props.deckId, this.state.style));
      }

      const active = this.getActive();

      const ready = _.isArray(this.props.cards);
      const haveCards = ready && this.props.cards.length !== 0;
      const done = haveCards && this.props.cards[0].chinese.length <= active;
      if (done && this.continueRef) {
        this.continueRef.focus();
      }
      if (_.isArray(this.props.cards) && !_.isArray(prevProps.cards)) {
        this.setState({ showPinyin: false, showEnglish: false, active: 0 });
      }
      this.setAudioState();
    };
    componentWillUnmount = () => {
      this.props.dispatch(clearCards());
    };

    setAudioState = () => {
      const { cards } = this.props;
      const isSound = this.state.type === "sound";
      if (_.isArray(cards) && cards.length > 0) {
        const sid = cards[0].sentenceId;
        const target = "/static/audio/sentences/" + sid + ".mp3";
        if (!this.audio || !this.audio.src.endsWith(target)) {
          this.audio = new Audio();
          this.audio.src = target;
          if (isSound) this.audio.play();
        }
      }
    };

    onSpace = value => {
      const { cards } = this.props;
      const block = cards[0].chinese[this.getActive()];
      const isCorrect = _.includes(
        block.answers,
        value.replace(/\s/g, "").toLowerCase()
      );
      if (isCorrect) this.onAnswer(value);
      return isCorrect;
    };
    onAnswer = value => {
      const isKeyboard = this.state.type === "keyboard";
      if (value === "") {
        if (!isKeyboard) this.playAudio();
        return;
      }
      const { cards } = this.props;
      const card = cards[0];
      const block = card.chinese[this.getActive()];
      const isCorrect = _.includes(
        block.answers,
        value.replace(/\s/g, "").toLowerCase()
      );
      const response = {
        word: block.simplified,
        sentenceId: card.sentenceId,
        completed: isCorrect,
        value: value,
        shownAnswer: this.state.showPinyin,
        factor: 3
      };
      backend.relay(receiveResponse(response));
      if (isCorrect) {
        let i = this.getActive() + 1;
        i = this.getActive(i);
        const allDone = i >= card.chinese.length;
        this.setState({
          active: i,
          showPlaceholder: false,
          showEnglish: !allDone && this.state.showEnglish,
          showPinyin: false
        });
        if (allDone) {
          if (isKeyboard) this.playAudio();
        }
      }
      return isCorrect;
    };
    onEscape = () => {
      this.setState(state => {
        if (state.type === "sound")
          return {
            showEnglish: true,
            showPlaceholder: state.showEnglish,
            showPinyin: state.showPlaceholder
          };
        else return { showEnglish: true, showPinyin: state.showEnglish };
      });
      return false;
    };
    onSwitchStyle = style => () => {
      this.setState({ style });
    };
    onCloseStatus = () => {
      this.setState({ showStatus: false });
    };
    onKeyUp = e => {
      // Hitting space (KeyDown) may finish the exercise and switch focus
      // to the 'continue' button. Then, when the user lifts his finger and
      // the KeyUp event is fired, that event triggers the 'continue' button
      // prematurely. We get around this by ignoring all KeyUp events for
      // the 'continue' button. Instead, we take action only on KeyDown.
      e.preventDefault();
    };
    onKeyDown = e => {
      var code = e.keyCode ? e.keyCode : e.which;
      if (code === 32 || code === 13) {
        this.onContinue(e);
        e.preventDefault();
      }
    };
    onContinue = e => {
      const { cards } = this.props;
      if (cards.length === 1) {
        this.setState({ showStatus: false, active: 0 }); // Show Status FIXME
        backend.relay(fetchCards(this.props.deckId, this.state.style));
      } else {
        this.setState({ active: 0 });
        this.props.dispatch(receiveCards(_.slice(cards, 1)));
      }
    };
    getActive = i => {
      const thoroughMode = this.state.mode === "ship";
      const { cards } = this.props;
      const valid = i =>
        i < cards[0].chinese.length &&
        _.isPlainObject(cards[0].chinese[i]) &&
        (cards[0].chinese[i].isGap || thoroughMode);
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
      this.audio.play();
    };
    switchMode = mode => {
      this.setState({ mode });
    };
    switchType = type => {
      this.setState({ type });
    };
    render = () => {
      const { style, showStatus, showPinyin, showEnglish } = this.state;
      const { cards, studyQuitLink } = this.props;
      const active = this.getActive();
      const keyboardStyle = this.state.type === "keyboard";
      const soundStyle = this.state.type === "sound";

      const ready = _.isArray(cards);
      const haveCards = ready && cards.length !== 0;
      const done = haveCards && cards[0].chinese.length <= active;
      const sid = haveCards && cards[0].sentenceId;

      return (
        <div style={containerStyle}>
          <Modal open={showStatus} onClose={this.onCloseStatus}>
            <Modal.Content>Content</Modal.Content>
          </Modal>
          <Grid stackable>
            <Grid.Row columns={1} textAlign="center" style={headerRow}>
              <Grid.Column>
                High score | When word will be reviewed.
              </Grid.Column>
            </Grid.Row>
            <Grid.Row columns={1} textAlign="center" style={exerciseRow}>
              <Grid.Column>
                {ready ? (
                  haveCards ? (
                    <ClozeSentence
                      onAnswer={this.onAnswer}
                      onShiftEnter={this.playAudio}
                      onSpace={this.onSpace}
                      onEscape={this.onEscape}
                      active={active}
                      showPinyin={showPinyin}
                      showPlaceholder={
                        this.state.showPlaceholder || keyboardStyle
                      }
                      mode={this.state.mode}
                      type={this.state.type}
                      showEnglish={showEnglish || done}
                      blocks={cards[0].chinese}
                      english={cards[0].english}
                    />
                  ) : (
                    "No exercises available"
                  )
                ) : (
                  <Loader active />
                )}
              </Grid.Column>
            </Grid.Row>
            <Grid.Row columns={3} style={controlRow}>
              <Grid.Column textAlign="center">
                <Button
                  negative
                  circular
                  icon="power"
                  as={Link}
                  to={studyQuitLink}
                />
                <Button circular icon="settings" disabled />
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
                  disabled={!done && !soundStyle}
                  icon="play"
                  onClick={this.playAudio}
                />
                <ModeButton mode={this.state.mode} onChange={this.switchMode} />
                <TypeButton type={this.state.type} onChange={this.switchType} />
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
              <Grid.Column textAlign="center">
                {done ? (
                  <Button
                    ref={this.handleContinueRef}
                    onClick={this.onContinue}
                    onKeyUp={this.onKeyUp}
                    onKeyDown={this.onKeyDown}
                    positive
                  >
                    Continue
                  </Button>
                ) : (
                  <Button
                    disabled={!ready}
                    color="yellow"
                    onClick={this.onEscape}
                  >
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

class ModeButton extends PureComponent {
  state = { open: false };

  handleOpen = () => {
    this.setState({ open: true });
  };
  handleClose = () => {
    this.setState({ open: false });
  };

  switchMode = mode => () => {
    this.props.onChange(mode);
    this.handleClose();
  };

  render = () => {
    const { mode } = this.props;
    const { open } = this.state;
    return (
      <Popup
        trigger={<Button circular icon={mode} />}
        flowing
        hideOnScroll
        open={open}
        onOpen={this.handleOpen}
        onClose={this.handleClose}
        on="click"
      >
        <Grid centered divided columns={2}>
          <Grid.Column
            as={"a"}
            textAlign="center"
            style={{ maxWidth: "25em" }}
            onClick={this.switchMode("rocket")}
          >
            <Header as="h4">
              <Icon name="rocket" />
            </Header>
            <center>
              <b>Speed mode</b>
            </center>
            <p>
              <b>
                Speed through exercises, focusing only on new or difficult
                words.
              </b>
            </p>
          </Grid.Column>
          <Grid.Column
            as="a"
            onClick={this.switchMode("ship")}
            textAlign="center"
            style={{ maxWidth: "25em" }}
          >
            <Header as="h4">
              <Icon name="ship" />
            </Header>
            <center>
              <b>Mastery mode</b>
            </center>
            <p>
              <b>
                Stimulate deep learning by focusing on the context in which
                words appear.
              </b>
            </p>
          </Grid.Column>
        </Grid>
      </Popup>
    );
  };
}

class TypeButton extends PureComponent {
  state = { open: false };

  handleOpen = () => {
    this.setState({ open: true });
  };
  handleClose = () => {
    this.setState({ open: false });
  };

  switchType = mode => () => {
    this.props.onChange(mode);
    this.handleClose();
  };

  render = () => {
    const { type } = this.props;
    const { open } = this.state;
    return (
      <Popup
        trigger={<Button circular icon={type} />}
        flowing
        hideOnScroll
        open={open}
        onOpen={this.handleOpen}
        onClose={this.handleClose}
        on="click"
      >
        <Grid centered divided columns={2}>
          <Grid.Column
            as={"a"}
            textAlign="center"
            style={{ maxWidth: "25em" }}
            onClick={this.switchType("keyboard")}
          >
            <Header as="h4">
              <Icon name="keyboard" />
            </Header>
            <center>
              <b>Reading exercises</b>
            </center>
            <p>
              <b>Learn to recognize written Chinese.</b>
            </p>
          </Grid.Column>
          <Grid.Column
            as="a"
            onClick={this.switchType("sound")}
            textAlign="center"
            style={{ maxWidth: "25em" }}
          >
            <Header as="h4">
              <Icon name="sound" />
            </Header>
            <center>
              <b>Listening exercises</b>
            </center>
            <p>
              <b>Learn to understand spoken Mandarin.</b>
            </p>
          </Grid.Column>
        </Grid>
      </Popup>
    );
  };
}
