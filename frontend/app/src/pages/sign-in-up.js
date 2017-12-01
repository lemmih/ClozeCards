import _ from "lodash";
import React, { Component } from "react";
import { Redirect } from "react-router-dom";
import { Container, Form } from "semantic-ui-react";
import { connect } from "react-redux";
import sha256 from "sha256";

import { login } from "../actions/user";
import backend from "../backend";

function toSignInProps(store) {
  return {
    user: store.user
  };
}

export const SignIn = connect(toSignInProps)(
  class SignIn extends Component {
    constructor(props) {
      super(props);
      this.state = { email: "", password: "" };
    }
    signin = () => {
      const { email, password } = this.state;
      backend.relay(login(email, sha256(password)));
    };
    loading = () => {
      return this.props.user.status === "logging-in";
    };
    failed = () => {
      return this.props.user.status === "failed";
    };
    setEmail = evt => {
      this.setState({ email: evt.target.value });
    };
    setPassword = evt => {
      this.setState({ password: evt.target.value });
    };
    render = () => {
      if (!_.isNull(this.props.user.email)) return <Redirect to="/" />;
      return (
        <Container text>
          <Form>
            <Form.Input
              onChange={this.setEmail}
              error={this.failed()}
              icon="checkmark"
              placeholder="Email address"
            />
            <Form.Input
              onChange={this.setPassword}
              error={this.failed()}
              type="password"
              placeholder="Password"
            />
            <Form.Button
              loading={this.loading()}
              onClick={this.signin}
              fluid
              primary
              content="Sign in"
            />
          </Form>
        </Container>
      );
    };
  }
);

export class SignUp extends Component {
  render = () => {
    return (
      <Container text>
        <Form>
          <Form.Input icon="checkmark" placeholder="Email address" />
          <Form.Input type="password" placeholder="Password" />
          <Form.Input type="password" placeholder="Repeat password" />
          <Form.Button fluid primary content="Sign up" />
        </Form>
      </Container>
    );
  };
}
