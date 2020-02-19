import React, { Component } from 'react';
import axios from 'axios';
import InputPanel from './InputPanel'
import OutputPanel from './OutputPanel';
import '../css/grid.css';
import '../css/main.css'; 
import logo from '../images/laika-top.png'


class App extends Component {

  state = {
    lastResult: "<p>Transformation starts a second after you stop typing.</p>",
    selectedInputFormat: "md",
    selectedOutputFormat: "html-rendered",
    renderedOutputFormat: "html-rendered",
    markupInput: ""
  }

  handleResponse = response => { 
    console.log(`Received data: ${response.data}`); 
    this.setState({ lastResult: response.data, renderedOutputFormat: this.state.selectedOutputFormat })
  }

  handleError = error => { 
    console.log(error); 
    const msg = (error.response) ? `Status: ${error.response.status}` : 'Unable to call server'; 
    this.setState({ lastResult: `<p>Server Error (${msg})</p>` }); 
  }

  fetchResult = () => {
    console.log(`fetching result for format: ${this.state.selectedInputFormat}`);
    const url = `/transform/${this.state.selectedInputFormat}/${this.state.selectedOutputFormat}`;
    axios.post(url, this.state.markupInput, {responseType: 'text'}).then(this.handleResponse).catch(this.handleError);
  }

  handleInputChange = (format, input) => {
    console.log(`input format changed to: ${format}`)
    this.setState({ selectedInputFormat: format, markupInput: input }, this.fetchResult)
  }

  handleOutputChange = format => {
    console.log(`output format changed to: ${format}`)
    this.setState({ selectedOutputFormat: format }, this.fetchResult)
  }

  render() {
    const lastResult = this.state.lastResult;
    return (
      <div className="row">
        
        <div className="logo">
          <img src={logo}/>
          <h2>Transformer Demo App</h2>
        </div>
 
        <div className="left">
          <InputPanel onChange={this.handleInputChange}/>
        </div>
        
        <div className="right">
          <OutputPanel title="Output" content={lastResult} renderedOutputFormat={this.state.renderedOutputFormat} onChange={this.handleOutputChange}/>        
        </div>          
      
      </div>
    );    
  }
}

export default App;
