import React, { Component } from 'react';
import axios from 'axios';
import InputPanel from './InputPanel'
import OutputPanel from './OutputPanel';
import '../css/grid.css';
import '../css/main.css'; 
import logo from '../images/laika-top.png'


class App extends Component {

  emptyResultWithMessage = msg => { return {
    rawTree: msg, 
    rewrittenTree: '', 
    html: ''
  }}

  state = {
    lastResult: this.emptyResultWithMessage('Transformation starts a second after you stop typing')
  }

  handleResponse = response => { console.log(response); this.setState({lastResult: response.data})}

  handleError = error => { 
    console.log(error); 
    const msg = (error.response) ? `Status: ${error.response.status}` : 'Unable to call server'; 
    this.setState({ lastResult: this.emptyResultWithMessage(`Server Error (${msg})`) }); 
  }

  handleInputChange = (format, input) => {
    console.log(`fetching result for format: ${format} - input: ${input}`);
    const url = `/transform/${format}`;
    axios.post(url, input).then(this.handleResponse).catch(this.handleError);
  }

  handleOutputChange = format => {
    console.log(`output format changed to: ${format}`)
  }

  render() {
    const lastResult = this.state.lastResult;
    return (
      <div className="row">

        <img src={logo}/>
        <h2>Transformer Demo App</h2>
 
        <div className="left">
          <InputPanel onChange={this.handleInputChange}/>
        </div>
        
        <div className="right">
          <OutputPanel title="Output" content={lastResult.html} onChange={this.handleOutputChange}/>        
        </div>          
      
      </div>
    );    
  }
}

export default App;
