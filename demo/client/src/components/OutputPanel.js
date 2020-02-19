import React, { Component } from 'react';
import Panel from './Panel'
import ButtonGroup from './ButtonGroup'
import '../css/output.css'; 

class OutputPanel extends Component {

  state = {
    selectedOutputFormat: "html-rendered"
  }

  formats = [
    { value: "html-rendered", display: "Rendered HTML" }, 
    { value: "html-source", display: "HTML Source" },
    { value: "ast-resolved", display: "Resolved AST" },
    { value: "ast-unresolved", display: "Unresolved AST" }
  ]

  fireEvent = () => { this.props.onChange(this.state.selectedOutputFormat) }

  handleFormatChange = newFormat => { this.setState({ selectedOutputFormat: newFormat }, this.fireEvent); }

  render() {
    return (
      <Panel kind="output" title={this.props.title}>
        <ButtonGroup items={this.formats} value={this.state.selectedOutputFormat} onChange={this.handleFormatChange}/>
        <div className={`render ${this.props.renderedOutputFormat}`} dangerouslySetInnerHTML={{__html: this.props.content}} />
      </Panel>
    );
  }
}

export default OutputPanel;
