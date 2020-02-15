import React, { Component } from 'react';
import Panel from './Panel'
import '../css/output.css'; 

class OutputPanel extends Component {
  render() {
    return (
      <Panel kind="output" title={this.props.title}>
        <div className="render" dangerouslySetInnerHTML={{__html: this.props.content}} />
      </Panel>
    );
  }
}

export default OutputPanel;
