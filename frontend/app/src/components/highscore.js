// @flow
import _ from "lodash";
import React, { PureComponent } from "react";
import { Table } from "semantic-ui-react";
import { Map } from "immutable";
// import Identicon from "identicon.js";
// import sha256 from "sha256";

import Identicon from "./identicon";

type HighscoreProps = {
  title: string,
  highscore: Map<string, number>,
  highlight?: number
};

export default class Highscore extends PureComponent<HighscoreProps> {
  render = () => {
    const { title, highscore, highlight } = this.props;
    const lst = highscore.sort((a, b) => b - a);
    var n = 1;
    return (
      <div>
        <center>{title} high score</center>
        <Table compact textAlign="center" striped>
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
                  <Table.Row key={userId} active={highlight === userId}>
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
