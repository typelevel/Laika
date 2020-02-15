import React, { Component } from 'react';
import axios from 'axios';
import DocPanel from './DocPanel'
import InputPanel from './InputPanel'
import TextPanel from './TextPanel'
import HtmlPanel from './HtmlPanel';
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

  render() {
    const lastResult = this.state.lastResult;
    return (
      <div className="row">
 
        <div className="left">
          <img src={logo}/>
          <h2>Transformer Webtool</h2>

          <InputPanel onChange={this.handleInputChange}/>
          <DocPanel />
        </div>
        
        <div className="middle-right">
      
          <div className="middle">    
            <TextPanel title="Raw Document Tree Model" content={lastResult.rawTree} />
            <TextPanel title="Rewritten Document Tree Model" content={lastResult.rewrittenTree} bottom />          
          </div>
          
          <div className="right">        
            <TextPanel title="HTML Source" content={lastResult.html} />
            <HtmlPanel title="Rendered HTML" content={lastResult.html} />        
          </div>          
        </div>
      </div>
    );    
  }
}

export default App;
