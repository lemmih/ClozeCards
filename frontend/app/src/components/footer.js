import _ from "lodash";
import React, { PureComponent } from "react";
import { Grid, List } from "semantic-ui-react";

import "./footer.css";

export class Footer extends PureComponent {
  render = () => {
    return (
      <div className="footer">
        <Grid>
          <Grid.Row columns={3}>
            <Grid.Column>
              <h4>About</h4>
              <p>
                ClozeCards aims at providing the most effective language
                learning tools to the largest number of people possible.
              </p>
            </Grid.Column>
            <Grid.Column>
              <h4>Thanks</h4>
              <p>
                ClozeCards chould not be here without the great work done by{" "}
                <a href="https://www.mdbg.net/chinese/dictionary">MDBG</a>,{" "}
                <a href="https://tatoeba.org/eng/">Tatoeba</a>,{" "}
                <a href="http://shtooka.net/">Project SHTOOKA</a> and{" "}
                <a href="http://justlearnchinese.com/">JustLearnChinese</a>.
              </p>
            </Grid.Column>
            <Grid.Column>
              <h4>Contact and Updates</h4>
              <List>
                <List.Item>
                  <List.Icon name="mail" />
                  <List.Content>
                    <a href="mailto:david.himmelstrup@clozecards.com">
                      david.himmelstrup@clozecards.com
                    </a>
                  </List.Content>
                </List.Item>
                <List.Item>
                  <List.Icon name="facebook square" />
                  <List.Content>
                    <a href="https://www.facebook.com/ClozeCards/">
                      Facebook Page
                    </a>
                  </List.Content>
                </List.Item>
                <List.Item>
                  <List.Icon name="github" />
                  <List.Content>
                    <a href="https://github.com/Lemmih/ClozeCards">
                      Source code
                    </a>
                  </List.Content>
                </List.Item>
                {/*<List.Item>
                  <List.Icon name="info circle" />
                  <List.Content>
                    <a href="https://clozecards.com/news">Site news</a>
                  </List.Content>
                </List.Item>*/}
              </List>
            </Grid.Column>
          </Grid.Row>
        </Grid>
      </div>
    );
  };
}

export class EmptyFooter extends PureComponent {
  render = () => {
    return <div className="empty-footer" />;
  };
}
