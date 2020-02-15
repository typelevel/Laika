import React, { Component } from 'react';
import Panel from './Panel'
import '../css/output.css'; 

class HtmlPanel extends Component {
  render() {
    return (
      <Panel kind="output" title={this.props.title} bottom>
        <div dangerouslySetInnerHTML={{__html: this.props.content}} />
      </Panel>
    );
  }
}

export default HtmlPanel;
