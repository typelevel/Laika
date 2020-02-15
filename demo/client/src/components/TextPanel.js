import React, { Component } from 'react';
import Panel from './Panel'

class TextPanel extends Component {
  render() {
    return (
      <Panel kind="text" title={this.props.title} bottom={this.props.bottom}>
        <pre>{this.props.content}</pre>
      </Panel>
    );
  }
}

export default TextPanel;
