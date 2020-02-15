const HtmlWebpackPlugin = require('html-webpack-plugin')
const path = require('path')

module.exports = {
  entry: './src/index.js',
  output: {
    path: path.resolve(__dirname, "../src/main/resources/public"),
    filename: 'bundle.js'
  },
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader'
        }  
      },
      {
        test: /\.css$/,
        exclude: /node_modules/,
        use: [{
          loader: 'style-loader'
        },
        {
          loader: 'css-loader'
        }]  
      },
      {
        test: /\.(jpg|png|woff)$/,
        use: {
          loader: 'file-loader',
          options: {
            name: '[name].[ext]',
            outputPath: 'assets/'
          }
        }  
      }
    ]  
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: './src/index.html'  
    })
  ],
  devServer: {
    proxy: {
      '/transform': 'http://localhost:8080'
    }
  }
}
