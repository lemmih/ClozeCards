import _ from "lodash";
import React, { PureComponent } from "react";
import { Container, Button, Form } from "semantic-ui-react";
import { connect } from "react-redux";
import backend from "../backend";

import {
  fetchKnownWords,
  markWords,
  RECEIVE_KNOWN_WORDS
} from "../actions/mark";

class MarkWords extends PureComponent {
  state = {
    text: "",
    placeholder: "Add chinese words here...",
    loading: false
  };
  markKnown = () => {
    console.log("Mark known.");
    this.setState({
      text: "",
      placeholder: "Words marked as known.",
      loading: false
    });
    backend.relay(markWords(this.state.text, true));
  };
  markUnknown = () => {
    console.log("Mark unknown.");
    this.setState({
      text: "",
      placeholder: "Words marked as unknown.",
      loading: false
    });
    backend.relay(markWords(this.state.text, false));
  };
  clearField = () => {
    this.setState({ text: "", loading: false });
  };
  fetchKnownWords = () => {
    console.log("Fetch known.");
    this.setState({ loading: true });
    backend.callback(RECEIVE_KNOWN_WORDS, words => {
      this.setState({ text: words, loading: false });
    });
    backend.relay(fetchKnownWords());
  };
  handleChange = (e, { value }) => {
    this.setState({ text: value });
  };
  render = () => {
    const { text, loading, placeholder } = this.state;
    return (
      <Container text textAlign="center">
        <h2>Mark words</h2>
        <h3>
          Copy&paste known words into the text area and click the blue button to
          skip those words in future exercises. If you accidentally mark too
          many words, use the red button to schedule a review of the words.
        </h3>
        <Form loading={loading}>
          <Form.TextArea
            rows="10"
            value={text}
            onChange={this.handleChange}
            placeholder={placeholder}
          />
          <Button onClick={this.clearField}>Clear text field</Button>
          <Button onClick={this.fetchKnownWords}>Fetch known words</Button>
          <Button onClick={this.markKnown} primary>
            Mark words as known
          </Button>
          <Button onClick={this.markUnknown} negative>
            Mark words as unknown
          </Button>
        </Form>
      </Container>
    );
  };
}

export default MarkWords;
