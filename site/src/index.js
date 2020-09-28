import React from 'react'
import ReactDOM from 'react-dom'
import App from './App'
import './index.css'
import * as Sentry from '@sentry/browser'

/**
 * Render App
 */

Sentry.init({dsn: "https://78526db59a1941c8aa09d11a0c4f279c@o391196.ingest.sentry.io/5236912"});

ReactDOM.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
  document.getElementById('root')
)