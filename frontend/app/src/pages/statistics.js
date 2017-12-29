// @flow
import React, { PureComponent } from "react";
import { Container, Grid } from "semantic-ui-react";
import { connect } from "react-redux";

import Highscore from "../components/highscore";

type Props = {
  daily: { [string]: number },
  weekly: { [string]: number }
};

function toProps(store) {
  const { daily, weekly } = store.highscore;
  return {
    highlight: store.user.id,
    daily,
    weekly
  };
}

export default connect(toProps)(
  class Statistics extends PureComponent<Props> {
    render = () => {
      const { daily, weekly, highlight } = this.props;
      return (
        <Container text>
          <Grid stackable textAlign="center">
            <Grid.Row columns={2}>
              <Grid.Column>
                <Highscore
                  title="Weekly"
                  highscore={daily}
                  highlight={highlight}
                />
              </Grid.Column>
              <Grid.Column>
                <Highscore
                  title="Daily"
                  highscore={weekly}
                  highlight={highlight}
                />
              </Grid.Column>
            </Grid.Row>
          </Grid>
        </Container>
      );
    };
  }
);
