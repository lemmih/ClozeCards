import _ from "lodash";
import React, { Component } from "react";
import { Redirect, Link } from "react-router-dom";
import { Container, Form, Header, Grid, Message } from "semantic-ui-react";
import { connect } from "react-redux";
import sha256 from "sha256";

import { login, register } from "../actions/user";
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
      this.state = { email: "", password: "", attempted: false };
    }
    signin = () => {
      const { email, password } = this.state;
      backend.relay(login(email, sha256(password)));
      this.setState({ attempted: true });
    };
    loading = () => {
      return this.props.user.status === "logging-in";
    };
    failed = () => {
      return this.state.attempted && this.props.user.status === "failed";
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
        <Container text className="sign-in-up">
          <Header textAlign="center">Sign In</Header>
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
          <p>
            Don't have an account? <Link to={"/sign-up"}>Register</Link>
          </p>
          {this.failed() && (
            <Message warning content="Incorrect email/password." />
          )}
        </Container>
      );
    };
  }
);

export const SignUp = connect(toSignInProps)(
  class SignUp extends Component {
    constructor(props) {
      super(props);
      this.state = {
        email: "",
        password: "",
        repeatPassword: "",
        attempted: false
      };
    }
    register = () => {
      const { email, password } = this.state;
      backend.relay(register(email, sha256(password)));
      this.setState({ attempted: true });
    };
    loading = () => {
      return this.props.user.status === "logging-in";
    };
    failed = () => {
      return this.state.attempted && this.props.user.status === "failed";
    };
    setEmail = evt => {
      this.setState({ email: evt.target.value });
    };
    setPassword = evt => {
      this.setState({ password: evt.target.value });
    };
    setRepeatPassword = evt => {
      this.setState({ repeatPassword: evt.target.value });
    };
    render = () => {
      const { password, repeatPassword } = this.state;
      const passwordsMatch = password === repeatPassword;
      const emptyPassword = password.length === 0;
      if (!_.isNull(this.props.user.email)) return <Redirect to="/" />;
      return (
        <Container text className="sign-in-up">
          <Header textAlign="center">Register an Account</Header>
          <Form>
            <Form.Input
              onChange={this.setEmail}
              error={this.failed()}
              icon="checkmark"
              placeholder="Email address"
            />
            <Form.Input
              onChange={this.setPassword}
              error={this.failed() || !passwordsMatch}
              type="password"
              placeholder="Password"
            />
            <Form.Input
              onChange={this.setRepeatPassword}
              error={this.failed() || !passwordsMatch}
              type="password"
              placeholder="Password (again)"
            />
            <Form.Button
              loading={this.loading()}
              onClick={this.register}
              disabled={!passwordsMatch || emptyPassword}
              fluid
              primary
              content="Register account"
            />
          </Form>
          <p>
            Already have an account? <Link to={"/sign-in"}>Sign in</Link>
          </p>
          {!passwordsMatch && (
            <Message warning content="Passwords do not match." />
          )}
          {this.failed() && (
            <Message
              negative
              content="That email address is already registered."
            />
          )}
        </Container>
      );
    };
  }
);
