// @flow
import _ from "lodash";
import React, { PureComponent } from "react";
import { Table } from "semantic-ui-react";
import { Map, Set } from "immutable";
// import Identicon from "identicon.js";
// import sha256 from "sha256";

import Identicon from "./identicon";

type HighscoreProps = {
  title: string,
  highscore: Map<string, number>,
  highlight: ?number,
  mark: Set<number>
};

export default class Highscore extends PureComponent<HighscoreProps> {
  static defaultProps = {
    highlight: null,
    mark: Set()
  };

  render = () => {
    const { title, highscore, highlight, mark } = this.props;
    const lst = highscore.sort((a, b) => b - a);
    var n = 1;
    return (
      <div>
        <center>{title} high score</center>
        <Table compact textAlign="center">
          <Table.Header>
            <Table.Row>
              <Table.HeaderCell />
              <Table.HeaderCell>User</Table.HeaderCell>
              <Table.HeaderCell>Score</Table.HeaderCell>
            </Table.Row>
          </Table.Header>
          <Table.Body>
            {lst
              .map((score, userId) => {
                const idx = n;
                n++;
                return (
                  <Table.Row
                    key={userId}
                    active={highlight && highlight.toString() === userId}
                    positive={mark.has(parseInt(userId, 10))}
                  >
                    <Table.Cell>#{idx}</Table.Cell>
                    <Table.Cell>
                      <Identicon id={userId} size={32} />
                    </Table.Cell>
                    <Table.Cell>{score}</Table.Cell>
                  </Table.Row>
                );
              })
              .toArray()}
          </Table.Body>
        </Table>
      </div>
    );
  };
}
