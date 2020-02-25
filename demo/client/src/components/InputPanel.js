import React, { Component } from 'react';
import Panel from './Panel'
import ButtonGroup from './ButtonGroup'

class InputPanel extends Component {

  state = {
    selectedFormat: "md",
    selectedExecutionMode: "jvm",
    markupInput: ""
  }

  formats = [
    { value: "md", display: "Markdown" }, 
    { value: "rst", display: "reStructuredText" }
  ]

  executionMode = [
    { value: "jvm", display: "Run on JVM" }, 
    { value: "js", display: "Run with Scala.js" }
  ]

  eventDelay = 1500
  maxInputChars = 800

  inputTooLong = () => this.state.markupInput.length > this.maxInputChars

  fireEvent = () => { 
    if (!this.inputTooLong()) this.props.onChange(this.state.selectedFormat, this.state.selectedExecutionMode, this.state.markupInput) 
  }

  scheduleEvent = () => {
    if (this.timeout) clearTimeout(this.timeout);
    this.timeout = setTimeout(this.fireEvent, this.eventDelay);
  }

  handleFormatChange = newFormat => { this.setState({ selectedFormat: newFormat }, this.fireEvent); }

  handleModeChange = newMode => { this.setState({ selectedExecutionMode: newMode }, this.fireEvent); }

  handleInputChange = event => { this.setState({ markupInput: event.target.value }, this.scheduleEvent); }

  render() {
    const numInputChars = this.state.markupInput.length;
    const counterClass = this.inputTooLong() ? "red" : undefined;
    return (
      <Panel kind="input" title="Input">
        <ButtonGroup position="grp-left" items={this.formats} value={this.state.selectedFormat} onChange={this.handleFormatChange}/>
        <ButtonGroup position="grp-right" items={this.executionMode} value={this.state.selectedExecutionMode} onChange={this.handleModeChange}/>
        <textarea defaultValue={""} onChange={this.handleInputChange}/>
        <div className="counter">
          <span className={counterClass}>{numInputChars} characters</span> ({this.maxInputChars} max)
        </div>    
      </Panel>  
    );
  }
}

export default InputPanel;
