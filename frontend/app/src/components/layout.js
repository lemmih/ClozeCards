import _ from "lodash";
import React, { Component } from "react";
import { BrowserRouter as Router, Route, Link, Switch } from "react-router-dom";
import { Menu, Dropdown } from "semantic-ui-react";
import { connect } from "react-redux";

import LandingPage from "../pages/landing-page";
import { ViewDeck, NewDeck } from "../pages/decks";
import BrowseDecks from "../pages/browse-decks";
import { SignIn, SignUp } from "../pages/sign-in-up";
import Dictionary from "./dictionary";

import { logout } from "../actions/user";

import backend from "../backend";

class Layout extends Component {
  logout = () => {
    backend.relay(logout());
    return true;
  };
  matchViewDeck = ({ match }) => {
    return <ViewDeck slug={match.params.slug} />;
  };
  matchViewNotes = ({ match }) => {
    return <ViewDeck slug={match.params.slug} notes />;
  };
  matchStudy = ({ match }) => {
    return <ViewDeck slug={match.params.slug} study />;
  };
  render = () => {
    const { user } = this.props;
    const loggedIn = !_.isNull(user.email);
    return (
      <Router>
        <div>
          <Menu
            attached="top"
            inverted={true}
            color="blue"
            size="huge"
            stackable={true}
          >
            <Menu.Item as={Link} to={"/"} header>
              ClozeCards
            </Menu.Item>
            <Menu.Item as={Link} to={"/top-decks/"}>
              Top Decks
            </Menu.Item>
            <Menu.Item as={Link} to={"/recent-decks/"}>
              Recent Decks
            </Menu.Item>
            {/* <Menu.Item as={Link} to={"/trending-decks/"}>Trending Decks</Menu.Item> */}
            <Menu.Item as={Link} to={"/teach/"}>
              Teach
            </Menu.Item>
            <Menu.Menu position="right">
              <Menu.Item as={Link} to={"/news/"}>
                News
              </Menu.Item>
              {loggedIn ? (
                <Dropdown item text={user.email}>
                  <Dropdown.Menu>
                    <Dropdown.Item as={Link} to={"/new-deck/"}>
                      New Deck
                    </Dropdown.Item>
                    <Dropdown.Item onClick={this.logout} as={Link} to={"/"}>
                      Logout
                    </Dropdown.Item>
                  </Dropdown.Menu>
                </Dropdown>
              ) : (
                <Menu.Item as={Link} to={"/sign-in"} name="Sign in" />
              )}
              {/* <Menu.Item as={Link} to={"/sign-up"} name="Sign up"/> */}
            </Menu.Menu>
          </Menu>
          <Switch>
            <Route path="/" exact={true} component={LandingPage} />
            <Route
              path="/top-decks/"
              exact={true}
              render={() => <BrowseDecks order="ByLikes" />}
            />
            <Route
              path="/recent-decks/"
              exact={true}
              render={() => <BrowseDecks order="ByDate" />}
            />
            <Route
              path="/trending-decks/"
              exact={true}
              render={() => <BrowseDecks order="ByTrending" />}
            />
            <Route path="/new-deck/" exact={true} render={() => <NewDeck />} />
            <Route
              path="/decks/:slug"
              exact={true}
              render={this.matchViewDeck}
            />
            <Route
              path="/decks/:slug/notes"
              exact={true}
              render={this.matchViewNotes}
            />
            <Route
              path="/decks/:slug/study"
              exact={true}
              render={this.matchStudy}
            />
            <Route path="/sign-in" exact={true} render={() => <SignIn />} />
            <Route path="/sign-up" exact={true} render={() => <SignUp />} />
          </Switch>
          <Dictionary />
        </div>
      </Router>
    );
  };
}

export default connect(store => store)(Layout);
