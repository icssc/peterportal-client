import React from 'react';
import ReactDOM from 'react-dom/client';

import App from './App';
import './index.css';

// Import Global Store
import { store } from './store/store';
import { Provider } from 'react-redux';
import { PostHogProvider } from 'posthog-js/react';

/**
 * Render App
 */

ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <Provider store={store}>
      <PostHogProvider
        apiKey={import.meta.env.VITE_PUBLIC_POSTHOG_KEY}
        options={{ api_host: import.meta.env.VITE_PUBLIC_POSTHOG_HOST, autocapture: true, enable_heatmaps: true }}
      >
        <App />
      </PostHogProvider>
    </Provider>
  </React.StrictMode>,
);
